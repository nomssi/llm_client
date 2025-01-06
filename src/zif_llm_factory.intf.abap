"! <p class="shorttext synchronized" lang="en">LLM Factory</p>
INTERFACE zif_llm_factory
  PUBLIC.
  CLASS-METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get a model client
    "! @parameter model | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter response | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
    get_client
      IMPORTING model           TYPE zllm_model
      RETURNING VALUE(response) TYPE REF TO zif_llm_client
      RAISING   zcx_llm_validation zcx_llm_authorization.
ENDINTERFACE.
