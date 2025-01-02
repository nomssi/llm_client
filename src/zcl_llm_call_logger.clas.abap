CLASS zcl_llm_call_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_llm_call_logger .
    ALIASES add FOR zif_llm_call_logger~add.
    METHODS constructor.
  PROTECTED SECTION.
    DATA active TYPE sap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_call_logger IMPLEMENTATION.
  METHOD zif_llm_call_logger~add.
    IF active = abap_false.
      RETURN.
    ENDIF.
    INSERT zllm_call_log FROM @entry.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE save_calls INTO @active FROM zllm_system.
  ENDMETHOD.

ENDCLASS.
