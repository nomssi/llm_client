INTERFACE zif_llm_encryption
  PUBLIC .
  CLASS-METHODS:
    decrypt IMPORTING encrypted TYPE xstring RETURNING VALUE(result) TYPE string RAISING zcx_llm_validation,
    encrypt IMPORTING unencrypted TYPE string RETURNING VALUE(result) TYPE xstring RAISING zcx_llm_validation.
ENDINTERFACE.
