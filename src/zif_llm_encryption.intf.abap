"! <p class="shorttext synchronized" lang="en">Encryption Provider</p>
INTERFACE zif_llm_encryption
  PUBLIC.
  "! <p class="shorttext synchronized"></p>
  "! Decrypt the secret
  "! @parameter encrypted             | <p class="shorttext synchronized"></p>
  "! @parameter result                | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_validation    | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_authorization | <p class="shorttext synchronized"></p>
  CLASS-METHODS decrypt
    IMPORTING encrypted     TYPE xstring
    RETURNING VALUE(result) TYPE string
    RAISING   zcx_llm_validation zcx_llm_authorization.

  "! <p class="shorttext synchronized"></p>
  "! Encrypt the secret
  "! @parameter unencrypted           | <p class="shorttext synchronized"></p>
  "! @parameter result                | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_validation    | <p class="shorttext synchronized"></p>
  "! @raising   zcx_llm_authorization | <p class="shorttext synchronized"></p>
  CLASS-METHODS encrypt
    IMPORTING unencrypted   TYPE string
    RETURNING VALUE(result) TYPE xstring
    RAISING   zcx_llm_validation zcx_llm_authorization.

  "! <p class="shorttext synchronized">Sign a given binary string</p>
  "!
  "! @parameter ssf_application | <p class="shorttext synchronized">SSF Application</p>
  "! @parameter xstring_to_sign | <p class="shorttext synchronized">Binary to sign</p>
  "! @parameter result          | <p class="shorttext synchronized">Signature</p>
  CLASS-METHODS sign IMPORTING ssf_application TYPE ssfappl
                               xstring_to_sign TYPE xstring
                     RETURNING VALUE(result)   TYPE xstring
                     RAISING   zcx_llm_validation.
ENDINTERFACE.
