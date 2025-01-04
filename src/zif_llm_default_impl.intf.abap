INTERFACE zif_llm_default_impl
  PUBLIC .
  INTERFACES if_badi_interface.
  METHODS:
    get_json_impl RETURNING VALUE(result) TYPE seoclsname,
    get_encryption_impl RETURNING VALUE(result) TYPE REF TO zif_llm_encryption,
    get_call_logger_impl RETURNING VALUE(result) TYPE REF TO zif_llm_call_logger,
    get_statistics_impl RETURNING VALUE(result) TYPE REF TO zif_llm_statistics.

ENDINTERFACE.
