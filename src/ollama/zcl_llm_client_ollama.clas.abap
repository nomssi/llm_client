CLASS zcl_llm_client_ollama DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_client .

    METHODS constructor
      IMPORTING
        !client_config   TYPE zllm_clnt_config
        !provider_config TYPE zllm_providers
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
      client_name TYPE string VALUE `Ollama` ##NO_TEXT.

    DATA:
      client_config   TYPE zllm_clnt_config,
      provider_config TYPE zllm_providers,
      client          TYPE REF TO zif_llm_http_client_wrapper,
      tool_parser     TYPE REF TO zif_llm_tool_parser,
      statistics      TYPE REF TO zif_llm_statistics,
      begin_request   TYPE timestamp,
      end_request     TYPE timestamp,
      id              TYPE uuid,
      msg             TYPE i.

    TYPES:
      BEGIN OF function,
        name      TYPE string,
        "Arguments are dynamic based on the tool call therefore we parse the JSON later
        arguments TYPE /ui2/cl_json=>json,
      END OF function,

      BEGIN OF tool_call,
        function TYPE function,
      END OF tool_call,

      BEGIN OF ollama_message,
        role       TYPE string,
        content    TYPE string,
        tool_calls TYPE STANDARD TABLE OF tool_call WITH EMPTY KEY,
      END OF ollama_message,

      BEGIN OF ollama_response,
        prompt_eval_count TYPE i,
        eval_count        TYPE i,
        message           TYPE ollama_message,
        done_reason       TYPE string,
      END OF ollama_response.

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


CLASS zcl_llm_client_ollama IMPLEMENTATION.

  METHOD build_request_json.
    " Open content
    result = |\{"stream":false,"model":"{ client_config-provider_model }","messages":[|.
    DATA first_line TYPE abap_bool VALUE abap_true.

    " Workaround for Ollama currently not adding the json schema to the prompt.
    DATA(messages) = request-messages.
    IF request-use_structured_output = abap_true.
      DATA(last_line) = lines( messages ).
      READ TABLE messages INDEX last_line
        ASSIGNING FIELD-SYMBOL(<message>).
      <message>-content = |{ <message>-content } Respond in JSON based on the following schema: |
                         && |{ request-structured_output->get_schema( ) }| ##NO_TEXT .
    ENDIF.

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
      result = |{ result },"format":{ request-structured_output->get_schema( ) }|.
    ENDIF.

    " Add tools is available and active
    IF lines( request-tools ) > 0 AND request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      result = |{ result },"tools":[|.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).
        IF sy-tabix = 1.
          result = |{ result }\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                &&  tool_parser->parse( data = details-parameters-data descriptions = details-parameters-descriptions )
                && |\}\}|.
        ELSE.
          result = |,{ result }\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                  && |,"description":"{ details-description }","parameters":|
                  &&  tool_parser->parse( data = details-parameters-data descriptions = details-parameters-descriptions )
                  && |\}\}|.
        ENDIF.
      ENDLOOP.
      result = |{ result }]|.
      " OLLAMA does not support tool_choice, therefore we ignore whatever is set
    ENDIF.

    " Add options if available
    DATA(option_parameters) = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      result = |{ result },"options":\{|.
      first_line = abap_true.

      LOOP AT option_parameters INTO DATA(parameter).
        IF first_line = abap_true.
          result = |{ result }"{ parameter-key }":{ parameter-value }|.
          first_line = abap_false.
        ELSE.
          result = |{ result },"{ parameter-key }":{ parameter-value }|.
        ENDIF.
      ENDLOOP.

      result = |{ result }\}|.
    ENDIF.

    result = |{ result }\}|.
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
              && |"arguments":{ <tool_call>-function-json_response }\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      result = |\{"role":"{ message-role }","content":"{
               escape( val = message-content
                      format = cl_abap_format=>e_json_string ) }"\}|.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    me->client_config = client_config.
    me->provider_config = provider_config.
    me->tool_parser = NEW zcl_llm_tool_parser( ).
    TRY.
        id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        "Unusual that this class does not work but we just use a detailed timestamp instead.
        "It is very unlikely that we run into an issue with this high resolution.
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

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config = client_config provider_config = provider_config ).
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

    DATA ollama_response TYPE ollama_response.

    zcl_llm_common=>from_json(
      EXPORTING
        json = http_response-response
      CHANGING
        data = ollama_response ).

    result-choice = VALUE #(
        finish_reason = ollama_response-done_reason
        message = VALUE #(
          role    = ollama_response-message-role
          content = ollama_response-message-content
         ) ).

    result-usage = VALUE #(
      completion_tokens = ollama_response-eval_count
      prompt_tokens    = ollama_response-prompt_eval_count
      total_tokens     = ollama_response-eval_count +
                        ollama_response-prompt_eval_count ).

    "Parse errors will be ignored, we parse as good as we can
    IF request-use_structured_output = abap_true.
      parse_structured_output(
        EXPORTING
          content  = ollama_response-message-content
          request  = request
        CHANGING
          response = result ).
    ENDIF.

    "There can be multiple tool calls, we need to map them properly to the available tools.
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).

        LOOP AT ollama_response-message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>) WHERE function-name = details-name.
          TRY.
              DATA func_result TYPE REF TO data.
              CREATE DATA func_result TYPE HANDLE details-parameters-data_desc.

              zcl_llm_common=>from_json( EXPORTING json = <tool_call>-function-arguments CHANGING data = func_result ).

              APPEND VALUE #(
                id = ``
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
    FIELD-SYMBOLS <out> TYPE data.
    DATA structured_output TYPE REF TO data.

    request-structured_output->get_datatype(
      IMPORTING
        data = structured_output ).

    ASSIGN structured_output->* TO <out>.
    CREATE DATA response-choice-structured_output LIKE <out>.

    zcl_llm_common=>from_json(
      EXPORTING
        json = content
      CHANGING
        data = response-choice-structured_output ).
  ENDMETHOD.


  METHOD set_auth.
    " OLLAMA does not have any protection by default however we support API Keys
    " via headers in case it is secured e.g. by a reverse proxy or API Gateway.
    " Format is HeaderName:ApiKey
    DATA auth_value TYPE string.
    IF provider_config-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl RECEIVING result = DATA(enc_class).
      auth_value = enc_class->decrypt( encrypted = provider_config-auth_encrypted ).
    ENDIF.
    IF provider_config-auth_type = 'A'.
      SPLIT auth_value AT ':' INTO DATA(api_header) DATA(api_key).
      client->set_header( name = api_header value = api_key ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_llm_client~chat.
    TRY.
        ADD 1 TO msg.
        client->set_url( '/chat' ).
        GET TIME STAMP FIELD begin_request.
        DATA(resp) = client->communicate(
            request = build_request_json( request->get_internal_request( ) )
            session_id = id
            msg = msg ).
        GET TIME STAMP FIELD end_request.
        response = handle_http_response( http_response = resp request = request->get_internal_request( ) ).
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


  METHOD zif_llm_client~get_client.
    response = NEW zcl_llm_client_ollama( client_config = client_config provider_config = provider_config ).
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

    " Ollama uses the default structured output without any additions
    request-structured_output = NEW zcl_llm_so_js( ).
    response = NEW zcl_llm_chat_request( request ).
  ENDMETHOD.


ENDCLASS.
