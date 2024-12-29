CLASS zcl_llm_client_ollama DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        config TYPE zllm_clnt_config
      RAISING
        zcx_llm_validation.
    INTERFACES zif_llm_client.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF status,
        ok                TYPE i VALUE 200,
        too_many_requests TYPE i VALUE 429,
        timeout           TYPE i VALUE 408,
      END OF status,
      client_name TYPE string VALUE `Ollama` ##NO_TEXT.

    DATA:
      config TYPE zllm_clnt_config,
      client TYPE REF TO zif_llm_http_client_wrapper.

    TYPES:
      BEGIN OF ollama_message,
        role    TYPE string,
        content TYPE string,
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

      set_auth,

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
        RETURNING VALUE(result) TYPE zllm_response.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_client_ollama IMPLEMENTATION.
  METHOD constructor.
    me->config = config.
    get_http_client( ).
    set_auth( ).
  ENDMETHOD.

  METHOD zif_llm_client~chat.
    TRY.
        client->set_url( '/chat' ).
        DATA(resp) = client->communicate( request = build_request_json( request ) ).
        response = handle_http_response( http_response = resp request = request ).
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
    client = zcl_llm_http_client_wrapper=>get_client( config ).
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

    result-choices = VALUE #(
      ( index = 1
        finish_reason = ollama_response-done_reason
        message = VALUE #(
          role    = ollama_response-message-role
          content = ollama_response-message-content ) ) ).

    result-usage = VALUE #(
      completion_tokens = ollama_response-eval_count
      prompt_tokens    = ollama_response-prompt_eval_count
      total_tokens     = ollama_response-eval_count +
                        ollama_response-prompt_eval_count ).

    IF request-use_structured_output = abap_true.
      parse_structured_output(
        EXPORTING
          content  = ollama_response-message-content
          request  = request
        CHANGING
          response = result ).
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
    CREATE DATA response-structured_output LIKE <out>.

    zcl_llm_common=>from_json(
      EXPORTING
        json = content
      CHANGING
        data = response-structured_output ).
  ENDMETHOD.

  METHOD zif_llm_client~new_request.
    response-options = NEW zcl_llm_options( ).

    " Get configured default parameters and set them
    IF config-default_op IS NOT INITIAL.
      SPLIT config-default_op AT ';' INTO TABLE DATA(options).
      DATA parameters TYPE zllm_keyvalues.

      LOOP AT options INTO DATA(option).
        SPLIT option AT ':' INTO DATA(key) DATA(value).
        INSERT VALUE #( key = key value = value )
          INTO TABLE parameters.
      ENDLOOP.

      response-options->set_custom_parameters( parameters ).
    ENDIF.

    " Ollama uses the default structured output without any additions
    response-structured_output = NEW zcl_llm_so_js( ).
  ENDMETHOD.

  METHOD build_request_json.
    " In case we have forced options set them.
    " Note: default options are set in new_request
    IF config-forced_op IS NOT INITIAL.
      SPLIT config-forced_op AT ';' INTO TABLE DATA(options).
      DATA parameters TYPE zllm_keyvalues.

      LOOP AT options INTO DATA(option).
        SPLIT option AT ':' INTO DATA(key) DATA(value).
        INSERT VALUE #( key = key value = value )
          INTO TABLE parameters.
      ENDLOOP.

      request-options->set_custom_parameters( parameters ).
    ENDIF.

    " Open content
    result = |\{"stream":false,"model":"{ config-provider_model }","messages":[|.
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
        result = |{ result }\{"role":"{ message-role }","content":"{
          escape( val = message-content
                 format = cl_abap_format=>e_json_string ) }"\}|.
        first_line = abap_false.
      ELSE.
        result = |{ result },\{"role":"{ message-role }","content":"{
          escape( val = message-content
                 format = cl_abap_format=>e_json_string ) }"\}|.
      ENDIF.
    ENDLOOP.
    result = |{ result }]|.

    " Add structured output if available and format
    IF request-use_structured_output = abap_true.
      result = |{ result },"format":{ request-structured_output->get_schema( ) }|.
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

  METHOD zif_llm_client~get_client.
    response = NEW zcl_llm_client_ollama( config = config ).
  ENDMETHOD.

  METHOD set_auth.
    IF config-auth_type = 'A'.
      SPLIT config-auth_value AT ':' INTO DATA(api_header) DATA(api_key).
      client->set_header( name = api_header value = api_key ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

