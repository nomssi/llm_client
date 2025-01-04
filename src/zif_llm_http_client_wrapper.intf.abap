INTERFACE zif_llm_http_client_wrapper
PUBLIC .
  TYPES: BEGIN OF response,
           code          TYPE sysubrc,
           message       TYPE string,
           response      TYPE string,
           http_response TYPE REF TO if_http_response,
         END OF response.


  CLASS-METHODS:
    get_client
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RETURNING VALUE(client)   TYPE REF TO zif_llm_http_client_wrapper
      RAISING   zcx_llm_validation.
  METHODS:
    set_header
      IMPORTING
        name  TYPE string
        value TYPE string
      RAISING
        zcx_llm_validation,

    set_url
      IMPORTING
        url TYPE string
      RAISING
        zcx_llm_validation,

    communicate
      IMPORTING
        request         TYPE string
        session_id      TYPE zllm_session_id
        msg             TYPE i
      RETURNING
        VALUE(response) TYPE response
      RAISING
        zcx_llm_http_error,

    close_client.
ENDINTERFACE.
