INTERFACE zif_llm_auth
  PUBLIC .

  METHODS check_provider_maintenance RAISING zcx_llm_authorization.
  METHODS check_get_client IMPORTING model TYPE zllm_model RAISING zcx_llm_authorization.
  METHODS check_decrypt RAISING zcx_llm_authorization.
  METHODS check_encrypt RAISING zcx_llm_authorization.

ENDINTERFACE.
