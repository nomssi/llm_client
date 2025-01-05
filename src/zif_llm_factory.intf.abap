"! <p class="shorttext synchronized" lang="en">LLM Factory</p>
INTERFACE zif_llm_factory
  PUBLIC.
  CLASS-METHODS: get_client
    IMPORTING model TYPE zllm_model
    RETURNING VALUE(response) TYPE REF TO zif_llm_client
    RAISING zcx_llm_validation zcx_llm_authorization.
ENDINTERFACE.
