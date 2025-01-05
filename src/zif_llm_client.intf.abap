INTERFACE zif_llm_client
  PUBLIC.

  CONSTANTS: role_user      TYPE string VALUE `user`,
             role_system    TYPE string VALUE `system`,
             role_assistant TYPE string VALUE 'assistant',
             role_tool      TYPE string VALUE `tool`.

  CLASS-METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get a new client based on the configuration
    "! @parameter client_config | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter provider_config | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter response | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    get_client
      IMPORTING
                client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RETURNING VALUE(response) TYPE REF TO zif_llm_client
      RAISING   zcx_llm_validation zcx_llm_authorization.
  METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Execute the chat request
    "! @parameter request | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter response | <p class="shorttext synchronized" lang="en"></p>
    chat IMPORTING request TYPE REF TO zif_llm_chat_request RETURNING VALUE(response) TYPE zllm_response,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Create a new chat request
    "! @parameter response | <p class="shorttext synchronized" lang="en">Request implementation</p>
    new_request RETURNING VALUE(response) TYPE REF TO zif_llm_chat_request.
ENDINTERFACE.
