"! <p class="shorttext synchronized" lang="en">LLM Defaults</p>
INTERFACE zif_llm_default_impl
  PUBLIC .
  INTERFACES if_badi_interface.
  METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get the name of the JSON class. If not default check documentation on requirements!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    get_json_impl RETURNING VALUE(result) TYPE seoclsname,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get the encryption provider
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    get_encryption_impl RETURNING VALUE(result) TYPE REF TO zif_llm_encryption,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get the call logger implementation
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    get_call_logger_impl RETURNING VALUE(result) TYPE REF TO zif_llm_call_logger,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get the statistics implementation
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    get_statistics_impl RETURNING VALUE(result) TYPE REF TO zif_llm_statistics,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get the authorization implementation. Auth-Errors should raise zcx_llm_authorization.
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    get_authorization_impl RETURNING VALUE(result) TYPE REF TO zif_llm_auth.

ENDINTERFACE.
