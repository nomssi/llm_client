"! <p class="shorttext synchronized" lang="en">Encryption Provider</p>
INTERFACE zif_llm_encryption
  PUBLIC .
  CLASS-METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Decrypt the secret
    "! @parameter encrypted | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
    decrypt
      IMPORTING encrypted     TYPE xstring
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_validation zcx_llm_authorization,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Encrypt the secret
    "! @parameter unencrypted | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_llm_authorization | <p class="shorttext synchronized" lang="en"></p>
    encrypt
      IMPORTING unencrypted   TYPE string
      RETURNING VALUE(result) TYPE xstring
      RAISING   zcx_llm_validation zcx_llm_authorization.
ENDINTERFACE.
