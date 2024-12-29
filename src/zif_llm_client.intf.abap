INTERFACE zif_llm_client
  PUBLIC.
  CLASS-METHODS: get_client IMPORTING config TYPE zllm_clnt_config RETURNING VALUE(response) TYPE REF TO zif_llm_client RAISING zcx_llm_validation.
  METHODS:
    chat IMPORTING request TYPE zllm_request RETURNING VALUE(response) TYPE zllm_response,
    new_request RETURNING VALUE(response) TYPE zllm_request.
ENDINTERFACE.
