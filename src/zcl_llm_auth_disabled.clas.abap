CLASS zcl_llm_auth_disabled DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_llm_auth .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_auth_disabled IMPLEMENTATION.
  METHOD zif_llm_auth~check_decrypt.
    "Implement a custom version and register in BADI ZLLM_DEFAULTS.
    "Raise the exception ZCX_LLM_AUTHORIZATION if not allowed
  ENDMETHOD.

  METHOD zif_llm_auth~check_encrypt.
    "Implement a custom version and register in BADI ZLLM_DEFAULTS.
    "Raise the exception ZCX_LLM_AUTHORIZATION if not allowed
  ENDMETHOD.

  METHOD zif_llm_auth~check_get_client.
    "Implement a custom version and register in BADI ZLLM_DEFAULTS.
    "Raise the exception ZCX_LLM_AUTHORIZATION if not allowed
  ENDMETHOD.

  METHOD zif_llm_auth~check_provider_maintenance.
    "Implement a custom version and register in BADI ZLLM_DEFAULTS.
    "Raise the exception ZCX_LLM_AUTHORIZATION if not allowed
  ENDMETHOD.

ENDCLASS.
