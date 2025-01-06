"! <p class="shorttext synchronized" lang="en">Authorization Checks</p>
INTERFACE zif_llm_auth
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Auth check for call of provider maintenance
  "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
  METHODS check_provider_maintenance RAISING zcx_llm_authorization.
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Auth check for getting a model client
  "! @parameter model | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
  METHODS check_get_client IMPORTING model TYPE zllm_model RAISING zcx_llm_authorization.
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Auth check allowed to decrypt the secret?
  "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
  METHODS check_decrypt RAISING zcx_llm_authorization.
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Auth check allowed to encrypt the secret?
  "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
  METHODS check_encrypt RAISING zcx_llm_authorization.

ENDINTERFACE.
