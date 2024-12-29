CLASS zcl_llm_client_openrouter DEFINITION
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
      client_name TYPE string VALUE `Openrouter` ##NO_TEXT.

    DATA:
      config TYPE zllm_clnt_config,
      client TYPE REF TO zif_llm_http_client_wrapper.

    TYPES:
      BEGIN OF openrouter_message,
        role    TYPE string,
        content TYPE string,
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

CLASS zcl_llm_client_openrouter IMPLEMENTATION.
  METHOD constructor.
    me->config = config.
    get_http_client( ).
    set_auth( ).
  ENDMETHOD.

  METHOD zif_llm_client~chat.
    TRY.
        client->set_url( '/chat/completions' ).
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

    "Set referer and title for openrouter statistics
    client->set_header( name = 'HTTP-Referer' value = 'https://abap-ai.com' ).
    client->set_header( name = 'X-Title' value = 'ABAP LLM Client' ).
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

    DATA openrouter_response TYPE openrouter_response.

    zcl_llm_common=>from_json(
      EXPORTING
        json = http_response-response
      CHANGING
        data = openrouter_response ).

    LOOP AT openrouter_response-choices INTO DATA(choice).
      APPEND VALUE #(
          finish_reason = choice-finish_reason
          index = sy-tabix
          message = VALUE #( role = choice-message-role
                             content = choice-message-content ) )
          TO result-choices.
    ENDLOOP.

    result-usage = VALUE #(
      completion_tokens = openrouter_response-usage-completion_tokens
      prompt_tokens    = openrouter_response-usage-prompt_tokens
      total_tokens     = openrouter_response-usage-total_tokens ).

    IF request-use_structured_output = abap_true.
      IF lines( openrouter_response-choices ) = 1.
        parse_structured_output(
          EXPORTING
            content  = openrouter_response-choices[ 1 ]-message-content
            request  = request
          CHANGING
            response = result ).
      ELSE.
        result-success = abap_false.
        "TODO - add proper error handling - maybe a new class for client errors?
        result-error-error_text = 'More than one response for structured output not supported'.
      ENDIF.
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

    " openrouter uses the default structured output without any additions
    response-structured_output = NEW zcl_llm_so_js_or( ).
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


    DATA(messages) = request-messages.

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
      result = |{ result },"response_format":\{"type":"json_schema","json_schema":{ request-structured_output->get_schema( ) }\}|.
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
    response = NEW zcl_llm_client_openrouter( config = config ).
  ENDMETHOD.

  METHOD set_auth.
    "Openrouter uses API Key in a Authorization: Bearer Header
    IF config-auth_type = 'A'.
      client->set_header( name = 'Authorization' value = |Bearer { config-auth_value }| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

