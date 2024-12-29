CLASS zcl_llm_common DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      to_json IMPORTING data TYPE data RETURNING VALUE(result) TYPE string,
      from_json IMPORTING json TYPE string CHANGING data TYPE data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: config TYPE zllm_config.
    CONSTANTS:
        def_json_class TYPE zllm_json_class VALUE '/UI2/CL_JSON'.
ENDCLASS.

CLASS zcl_llm_common IMPLEMENTATION.
  METHOD from_json.
    CALL METHOD (config-json_class)=>deserialize
      EXPORTING
        json        = json
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      CHANGING
        data        = data.
  ENDMETHOD.

  METHOD to_json.
    CALL METHOD (config-json_class)=>serialize
      EXPORTING
        data        = data
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
        compress    = abap_true
      RECEIVING
        r_json      = result.
  ENDMETHOD.

  METHOD class_constructor.
    SELECT SINGLE * FROM zllm_config INTO config.
    IF config-json_class IS INITIAL.
      config-json_class = def_json_class.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
