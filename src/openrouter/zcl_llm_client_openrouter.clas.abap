"! <p class="shorttext synchronized" lang="en">Openrouter Client</p>
CLASS zcl_llm_client_openrouter DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_llm_client.

    METHODS constructor
      IMPORTING
        client_config   TYPE zllm_clnt_config
        provider_config TYPE zllm_providers
      RAISING
        zcx_llm_validation
        zcx_llm_authorization.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF status,
        ok                TYPE i VALUE 200,
        too_many_requests TYPE i VALUE 429,
        timeout           TYPE i VALUE 408,
      END OF status,
      client_name TYPE string VALUE `OpenRouter` ##NO_TEXT.

    DATA:
      client          TYPE REF TO zif_llm_http_client_wrapper,
      client_config   TYPE zllm_clnt_config,
      provider_config TYPE zllm_providers,
      tool_parser     TYPE REF TO zif_llm_tool_parser,
      statistics      TYPE REF TO zif_llm_statistics,
      begin_request   TYPE timestamp,
      end_request     TYPE timestamp,
      id              TYPE uuid,
      msg             TYPE i.

    TYPES:
      BEGIN OF function,
        name      TYPE string,
        "OpenRouter returns the arguments as escaped JSON string.
        arguments TYPE string,
      END OF function,

      BEGIN OF tool_call,
        id       TYPE string,
        type     TYPE string,
        function TYPE function,
      END OF tool_call,

      BEGIN OF openrouter_message,
        role       TYPE string,
        content    TYPE string,
        tool_calls TYPE STANDARD TABLE OF tool_call WITH EMPTY KEY,
      END OF openrouter_message,

      BEGIN OF openrouter_choice,
        finish_reason TYPE string,
        message       TYPE openrouter_message,
      END OF openrouter_choice,

      BEGIN OF openrouter_usage,
        prompt_tokens     TYPE i,
        completion_tokens TYPE i,
        total_tokens      TYPE i,
      END OF openrouter_usage,

      BEGIN OF openrouter_response,
        id      TYPE string,
        choices TYPE STANDARD TABLE OF openrouter_choice WITH EMPTY KEY,
        usage   TYPE openrouter_usage,
        model   TYPE string,
      END OF openrouter_response.

    METHODS:
      get_http_client
        RAISING zcx_llm_validation,

      set_auth RAISING zcx_llm_authorization,

      build_request_json
        IMPORTING
          request       TYPE zllm_request
        RETURNING
          VALUE(result) TYPE string,

      parse_structured_output
        IMPORTING
          content  TYPE string
          request  TYPE zllm_request
        CHANGING
          response TYPE zllm_response,

      handle_http_response
        IMPORTING
                  request       TYPE zllm_request
                  http_response TYPE zif_llm_http_client_wrapper=>response
        RETURNING VALUE(result) TYPE zllm_response,

      parse_message
        IMPORTING message       TYPE zllm_msg
        RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_client_openrouter IMPLEMENTATION.
  METHOD constructor.
    me->client_config = client_config.
    me->provider_config = provider_config.
    me->tool_parser = NEW zcl_llm_tool_parser( ).
    TRY.
        id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        DATA help_timestamp TYPE timestampl.
        GET TIME STAMP FIELD help_timestamp.
        id = help_timestamp.
    ENDTRY.
    DATA stat_handler TYPE REF TO zllm_implementation.
    GET BADI stat_handler.
    CALL BADI stat_handler->get_statistics_impl RECEIVING result = statistics.
    get_http_client( ).
    set_auth( ).
  ENDMETHOD.

  METHOD zif_llm_client~chat.
    TRY.
        msg = msg + 1.
        client->set_url( '/chat/completions' ).
        GET TIME STAMP FIELD begin_request.
        DATA(resp) = client->communicate(
            request = build_request_json( request->get_internal_request( ) )
            session_id = id
            msg = msg ).
        GET TIME STAMP FIELD end_request.
        response = handle_http_response(
          http_response = resp
          request = request->get_internal_request( ) ).
        statistics->add( VALUE #(
            call_date = sy-datum
            call_time = sy-uzeit
            duration = end_request - begin_request
            model = client_config-model
            tokens_prompt = response-usage-prompt_tokens
            tokens_resp = response-usage-completion_tokens
            tokens_total = response-usage-total_tokens
            id = id
            msg = msg ) ).
      CATCH zcx_llm_http_error INTO DATA(http_error).
        response-error-error_text = http_error->if_message~get_text( ).
        response-error-retrieable = abap_false.
      CLEANUP.
        IF client IS BOUND.
          client->close_client( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config = client_config provider_config = provider_config ).

    "Set referer and title for openrouter statistics
    client->set_header( name = 'HTTP-Referer' value = 'https://abap-ai.com' ) ##NO_TEXT.
    client->set_header( name = 'X-Title' value = 'ABAP LLM Client' ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_http_response.
    IF http_response-code >= 400.
      result-error-http_code = http_response-code.
      result-error-error_text = http_response-message.
      IF http_response-code = status-too_many_requests OR
         http_response-code = status-timeout.
        result-error-retrieable = abap_true.
      ENDIF.
      RETURN.
    ENDIF.

    DATA response TYPE openrouter_response.

    zcl_llm_common=>from_json(
      EXPORTING
        json = http_response-response
      CHANGING
        data = response ).

    " There should always be only one response without streaming or none in case of errors
    " Note that we should not see an empty choices array without an HTTP error that is already
    " handled above.
    IF lines( response-choices ) <> 1.
      result-success = abap_false.
      result-error-error_text = 'Wrong number of choices!' ##NO_TEXT.
      RETURN.
    ENDIF.

    DATA(response_choice) = response-choices[ 1 ].
    result-choice = VALUE #(
        finish_reason = response_choice-finish_reason
        message = VALUE #( role = response_choice-message-role
                           content = response_choice-message-content ) ).

    result-usage = VALUE #(
      completion_tokens = response-usage-completion_tokens
      prompt_tokens    = response-usage-prompt_tokens
      total_tokens     = response-usage-total_tokens ).

    IF request-use_structured_output = abap_true.
      parse_structured_output(
        EXPORTING
          content  = response_choice-message-content
          request  = request
        CHANGING
          response = result ).
    ENDIF.

    "There can be multiple tool calls, we need to map them properly to the available tools.
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).

        LOOP AT response_choice-message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>) WHERE function-name = details-name.
          TRY.
              DATA func_result TYPE REF TO data.
              CREATE DATA func_result TYPE HANDLE details-parameters-data_desc.

              zcl_llm_common=>from_json( EXPORTING json = <tool_call>-function-arguments CHANGING data = func_result ).

              APPEND VALUE #(
                id = <tool_call>-id
                type = zif_llm_tool=>type_function
                function = VALUE #(
                  name = details-name
                  arguments = func_result
                  json_response = <tool_call>-function-arguments )
              ) TO result-choice-tool_calls.
              "Mapping error
            CATCH cx_root INTO DATA(error).
              result-success = abap_false.
              MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 016 WITH <tool_call>-function-name INTO DATA(message_text).
              result-error = VALUE #(
                 tool_parse_error = abap_true
                 error_text = message_text ).
              RETURN.
          ENDTRY.
        ENDLOOP.

        "Tool does not exist --> Hallucination
        IF sy-subrc <> 0.
          result-success = abap_false.
          MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 017 WITH details-name INTO message_text.
          result-error = VALUE #(
             tool_parse_error = abap_true
             error_text = message_text ).
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    result-success = abap_true.
  ENDMETHOD.

  METHOD parse_structured_output.
    DATA(data_desc) = request-structured_output->get_datatype( ).

    CREATE DATA response-choice-structured_output TYPE HANDLE data_desc.

    zcl_llm_common=>from_json(
      EXPORTING
        json = content
      CHANGING
        data = response-choice-structured_output ).
  ENDMETHOD.

  METHOD zif_llm_client~new_request.
    DATA request TYPE zllm_request.
    request-options = NEW zcl_llm_options( ).

    " Get configured default parameters and set them
    IF client_config-default_op IS NOT INITIAL.
      SPLIT client_config-default_op AT ';' INTO TABLE DATA(options).
      DATA parameters TYPE zllm_keyvalues.

      LOOP AT options INTO DATA(option).
        SPLIT option AT ':' INTO DATA(key) DATA(value).
        INSERT VALUE #( key = key value = value )
          INTO TABLE parameters.
      ENDLOOP.

      request-options->set_custom_parameters( parameters ).
    ENDIF.

    request-structured_output = NEW zcl_llm_so_js_or( ).
    response = NEW zcl_llm_chat_request( request ).
  ENDMETHOD.

  METHOD build_request_json.
    " Open content
    result = |\{"model":"{ client_config-provider_model }","messages":[|.
    DATA first_line TYPE abap_bool VALUE abap_true.

    DATA(messages) = request-messages.

    " Add messages
    LOOP AT messages INTO DATA(message).
      IF first_line = abap_true.
        result = |{ result }{ parse_message( message ) }|.
        first_line = abap_false.
      ELSE.
        result = |{ result },{ parse_message( message ) }|.
      ENDIF.
    ENDLOOP.
    result = |{ result }]|.

    " Add structured output if available and format
    IF request-use_structured_output = abap_true.
      result = |{ result },"response_format":\{"type":"json_schema","json_schema":{ request-structured_output->get_schema( ) }\}|.
    ENDIF.

    " Add tools if available and active
    IF lines( request-tools ) > 0 AND request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      result = |{ result },"tools":[|.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).
        IF sy-tabix = 1.
          result = |{ result }\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                &&  tool_parser->parse( data_desc = details-parameters-data_desc descriptions = details-parameters-descriptions )
                && |,"strict":true\}\}|.
        ELSE.
          result = |,{ result }\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                  && |,"description":"{ details-description }","parameters":|
                  && tool_parser->parse( data_desc = details-parameters-data_desc descriptions = details-parameters-descriptions )
                  && |,"strict":true\}\}|.
        ENDIF.
      ENDLOOP.

      result = |{ result }]|.

      " Tool choice
      IF request-tool_choice <> zif_llm_chat_request=>tool_choice_auto AND
        request-tool_choice <> zif_llm_chat_request=>tool_choice_required.
        result = |{ result },"tool_choice":"\{"type":"function","function":\{"name":"{ request-tool_choice }"\}\}|.
      ELSE.
        result = |{ result },"tool_choice":"{ request-tool_choice }"|.
      ENDIF.
    ENDIF.

    " Add options if available
    DATA(option_parameters) = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      LOOP AT option_parameters INTO DATA(parameter).
        result = |{ result },"{ parameter-key }":{ parameter-value }|.
      ENDLOOP.
    ENDIF.

    result = |{ result }\}|.
  ENDMETHOD.

  METHOD zif_llm_client~get_client.
    response = NEW zcl_llm_client_openrouter( client_config = client_config provider_config = provider_config ).
  ENDMETHOD.

  METHOD set_auth.
    "Openrouter uses API Key in a Authorization: Bearer Header
    DATA auth_value TYPE string.
    IF provider_config-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl RECEIVING result = DATA(enc_class).
      auth_value = enc_class->decrypt( encrypted = provider_config-auth_encrypted ).
    ENDIF.
    IF provider_config-auth_type = 'A'.
      client->set_header( name = 'Authorization' value = |Bearer { auth_value }| ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD parse_message.
    IF lines( message-tool_calls ) > 0.
      result = |\{"role":"{ message-role }","tool_calls":[|.
      LOOP AT message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>).
        IF sy-tabix <> 1.
          result = |{ result },|.
        ENDIF.
        result = |{ result }\{"id":"{ <tool_call>-id }","type":"{ <tool_call>-type }",|
              && |"{ <tool_call>-type }":\{"name":"{ <tool_call>-function-name }",|
              && |"arguments":"{ escape( val = <tool_call>-function-json_response format = cl_abap_format=>e_json_string ) }"\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      result = |\{"role":"{ message-role }","content":"{
               escape( val = message-content format = cl_abap_format=>e_json_string ) }"|.
      IF message-tool_call_id IS NOT INITIAL.
        result = |{ result },"tool_call_id":"{ message-tool_call_id }"\}|.
      ELSE.
        result = |{ result }\}|.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

