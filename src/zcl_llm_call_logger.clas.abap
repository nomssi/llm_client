CLASS zcl_llm_call_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_llm_call_logger .
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
    SELECT SINGLE * INTO @DATA(system) FROM zllm_system.
    IF system-save_calls = abap_true AND
        ( system-call_filter_uname = '*' OR system-call_filter_uname = sy-uname ).
      active = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
