"! <p class="shorttext synchronized" lang="en">Vertex Authentication Implementation</p>
CLASS zcl_llm_client_vertex_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Get Vertex AI JWT token</p>
    "! Checks shared memory for an existing token, if none exists create a new one.
    "! @parameter provider | <p class="shorttext synchronized">LLM Provider Entry</p>
    "! @parameter result   | <p class="shorttext synchronized">JWT Token</p>
    "! @raising zcx_llm_http_error | <p class="shorttext synchronized" lang="en">HTTP Error</p>
    "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en">Authorization Error</p>
    METHODS get_token IMPORTING provider      TYPE zllm_providers
                      RETURNING VALUE(result) TYPE zcl_llm_client_vertexai_sr=>token
                      RAISING   zcx_llm_http_error zcx_llm_authorization.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Get a token from shared memory if exists</p>
    "!
    "! @parameter provider_name | <p class="shorttext synchronized">Provider Name</p>
    "! @parameter result        | <p class="shorttext synchronized">Token</p>
    METHODS get_memory_token IMPORTING provider_name TYPE zllm_provider_name
                             RETURNING VALUE(result) TYPE zcl_llm_client_vertexai_sr=>token.

    "! <p class="shorttext synchronized">Check if the token is still valid</p>
    "!
    "! @parameter token  | <p class="shorttext synchronized">Token</p>
    "! @parameter result | <p class="shorttext synchronized">Validation Result</p>
    METHODS is_valid IMPORTING token         TYPE zcl_llm_client_vertexai_sr=>token
                     RETURNING VALUE(result) TYPE sap_bool.

    "! <p class="shorttext synchronized">Get a new token</p>
    "!
    "! @parameter provider              | <p class="shorttext synchronized">Provider Details</p>
    "! @parameter result                | <p class="shorttext synchronized">Token</p>
    "! @raising   zcx_llm_http_error    | <p class="shorttext synchronized">HTTP Error</p>
    "! @raising   zcx_llm_authorization | <p class="shorttext synchronized">Authorization Error</p>
    METHODS new_token IMPORTING provider      TYPE zllm_providers
                      RETURNING VALUE(result) TYPE zcl_llm_client_vertexai_sr=>token
                      RAISING   zcx_llm_http_error zcx_llm_authorization.

    "! <p class="shorttext synchronized">Save a new token to shared memory</p>
    "!
    "! @parameter token              | <p class="shorttext synchronized">Token</p>
    "! @raising   zcx_llm_http_error | <p class="shorttext synchronized">HTTP Error</p>
    METHODS save_token IMPORTING token TYPE zcl_llm_client_vertexai_sr=>token
                       RAISING   zcx_llm_http_error.

ENDCLASS.

CLASS zcl_llm_client_vertex_auth IMPLEMENTATION.
  METHOD get_token.
    DATA(token) = get_memory_token( provider-provider_name ).
    IF token IS NOT INITIAL AND is_valid( token ) = abap_true.
      result = token.
      RETURN.
    ENDIF.
    token = new_token( provider ).
    save_token( token ).
    result = token.
  ENDMETHOD.

  METHOD get_memory_token.
    TRY.
        DATA(area) = zcl_llm_client_vertexai_s_area=>attach_for_read( ).
        result = area->root->get_token( provider_name ).
        area->detach( ).
      CATCH cx_shm_inconsistent
            cx_shm_no_active_version
            cx_shm_read_lock_active
            cx_shm_exclusive_lock_active
            cx_shm_parameter_error
            cx_shm_change_lock_active.
        " we just set this as no token
        result = VALUE #( ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_valid.
    DATA current_timestamp TYPE timestamp.

    GET TIME STAMP FIELD current_timestamp.
    DATA(valid) = cl_abap_tstmp=>subtract( tstmp1 = token-valid_until
                                           tstmp2 = current_timestamp ).
    result = xsdbool( valid > 60 ).
  ENDMETHOD.

  METHOD new_token.
    DATA auth_config TYPE string.

    IF provider-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl
        RECEIVING result = DATA(enc_class).
      auth_config = enc_class->decrypt( provider-auth_encrypted ).
    ELSE.
      RAISE EXCEPTION NEW zcx_llm_http_error( textid = zcx_llm_http_error=>http_auth_processing
                                              attr1  = 'Missing google vertex configuration' ) ##NO_TEXT.
    ENDIF.

    SPLIT auth_config AT ';' INTO DATA(ssf_application) DATA(username).
    IF ssf_application IS INITIAL OR username IS INITIAL.
      RAISE EXCEPTION NEW zcx_llm_http_error(
          textid = zcx_llm_http_error=>http_auth_processing
          attr1  = 'Missing google vertex details it should be ssf_application;serviceaccountemail' ) ##NO_TEXT.
    ENDIF.

    DATA: BEGIN OF payload_header,
            alg TYPE string,
            typ TYPE string,
          END OF payload_header.
    DATA: BEGIN OF payload_body,
            iss   TYPE string,
            aud   TYPE string,
            scope TYPE string,
            iat   TYPE i,
            exp   TYPE i,
          END OF payload_body.

    payload_header-alg = 'RS256'.
    payload_header-typ = 'JWT'.

    payload_body-iss   = username.
    payload_body-scope = 'https://www.googleapis.com/auth/cloud-platform'.
    payload_body-aud   = 'https://oauth2.googleapis.com/token'.

    GET TIME STAMP FIELD DATA(timestamp).
    CONVERT TIME STAMP timestamp TIME ZONE 'UTC' INTO DATE DATA(date) TIME DATA(time).
    DATA unix_time TYPE string.
    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date      = date
                                                              iv_time      = time
                                                              iv_msec      = 0
                                                    IMPORTING ev_timestamp = unix_time ).

    payload_body-iat = substring( val = unix_time
                                  off = 0
                                  len = strlen( unix_time ) - 3 ).
    payload_body-exp = payload_body-iat + 3600.

    " Create the JSON strings inside the concatenation using string templates
    DATA(json_header) = zcl_llm_common=>to_json( data = payload_header ).
    DATA(json_body) = zcl_llm_common=>to_json( data = payload_body ).
    DATA(header_encoded) = cl_http_utility=>encode_base64( unencoded = json_header ).
    DATA(body_encoded) = cl_http_utility=>encode_base64( unencoded = json_body ).

    " Concatenate with '.'
    DATA(payload_encoded) = |{ header_encoded }.{ body_encoded }|.
    payload_encoded = replace( val  = payload_encoded
                               sub  = '='
                               with = ``
                               occ  = 0 ).
    payload_encoded = replace( val  = payload_encoded
                               sub  = '+'
                               with = '-'
                               occ  = 0 ).
    payload_encoded = replace( val  = payload_encoded
                               sub  = '/'
                               with = '_'
                               occ  = 0 ).

    DATA(bin_payload) = cl_binary_convert=>string_to_xstring_utf8( payload_encoded ).

    TRY.
        DATA(signature) = enc_class->sign( ssf_application = CONV ssfappl( ssf_application )
                                           xstring_to_sign = bin_payload ).
      CATCH zcx_llm_validation INTO DATA(error). " Validation error
        RAISE EXCEPTION NEW zcx_llm_http_error( textid   = zcx_llm_http_error=>http_auth_processing
                                                attr1    = 'Error during signing'
                                                previous = error ) ##NO_TEXT.
    ENDTRY.

    " Base64 encode the binary signature
    DATA(signature_base64) = cl_http_utility=>encode_x_base64( unencoded = signature ).
    " URL-safe replacements after standard Base64 encoding:
    signature_base64 = replace( val  = signature_base64
                                sub  = '='
                                with = ``
                                occ  = 0 ).
    signature_base64 = replace( val  = signature_base64
                                sub  = '+'
                                with = '-'
                                occ  = 0 ).
    signature_base64 = replace( val  = signature_base64
                                sub  = '/'
                                with = '_'
                                occ  = 0 ).

    " Append signature separated by '.' to the binary string
    result-content  = |{ payload_encoded }.{ signature_base64 }|.

    result-provider = provider-provider_name.

    " Use the signed jwt to authenticate and get the auth token
    DATA client TYPE REF TO if_http_client.

    cl_http_client=>create_by_destination( EXPORTING  destination              = provider-auth_rfc_destination
                                           IMPORTING  client                   = client
                                           EXCEPTIONS argument_not_found       = 1
                                                      destination_not_found    = 2
                                                      destination_no_authority = 3
                                                      plugin_not_active        = 4
                                                      internal_error           = 5
                                                      OTHERS                   = 6 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_llm_http_error( textid = zcx_llm_http_error=>http_auth_processing
                                              attr1  = 'Destination error, check setup' ) ##NO_TEXT.
    ENDIF.

    client->request->set_formfield_encoding( formfield_encoding = if_http_entity=>co_formfield_encoding_encoded ).
    client->request->set_form_field( name  = 'grant_type'
                                     value = 'urn:ietf:params:oauth:grant-type:jwt-bearer' ) ##NO_TEXT.
    client->request->set_form_field( name  = 'assertion'
                                     value = result-content ).

    client->request->set_method( if_http_request=>co_request_method_post ).

    client->send( EXCEPTIONS http_communication_failure = 1                  " Communication Error
                             http_invalid_state         = 2                  " Invalid state
                             http_processing_failed     = 3                  " Error When Processing Method
                             http_invalid_timeout       = 4                  " Invalid Time Entry
                             OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_llm_http_error( textid = zcx_llm_http_error=>http_auth_processing
                                              attr1  = 'Cannot send auth message' ) ##NO_TEXT.
    ENDIF.

    client->receive( EXCEPTIONS http_communication_failure = 1                " Communication Error
                                http_invalid_state         = 2                " Invalid state
                                http_processing_failed     = 3                " Error When Processing Method
                                OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_llm_http_error( textid = zcx_llm_http_error=>http_auth_processing
                                              attr1  = 'Auth communication error' ) ##NO_TEXT.
    ENDIF.

    DATA(response) = client->response->get_cdata( ).

    DATA: BEGIN OF oauth_response,
            access_token TYPE string,
            expires_in   TYPE i,
          END OF oauth_response.

    zcl_llm_common=>from_json( EXPORTING json = response
                               CHANGING  data = oauth_response ).
    result-valid_until = timestamp.
    result-valid_until = cl_abap_tstmp=>add_to_short( tstmp = result-valid_until
                                                      secs  = oauth_response-expires_in ).
    result-content     = oauth_response-access_token.

    IF result-content IS INITIAL.
      RAISE EXCEPTION NEW zcx_llm_http_error( textid = zcx_llm_http_error=>http_auth_processing
                                              attr1  = |No auth token returned{ response }| ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD save_token.
    TRY.
        zcl_llm_client_vertexai_s_area=>build( ).
        DATA(area) = zcl_llm_client_vertexai_s_area=>attach_for_update( ).
        area->root->set_token( token ).
        area->detach_commit( ).
      CATCH cx_shm_inconsistent
            cx_shm_no_active_version
            cx_shm_exclusive_lock_active
            cx_shm_version_limit_exceeded
            cx_shm_change_lock_active
            cx_shm_parameter_error
            cx_shm_pending_lock_removed.
        RAISE EXCEPTION NEW zcx_llm_http_error( textid = zcx_llm_http_error=>http_auth_processing
                                                attr1  = 'Unable to save token to shared memory' ) ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
