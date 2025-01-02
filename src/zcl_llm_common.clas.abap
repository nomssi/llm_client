CLASS zcl_llm_common DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      to_json IMPORTING data TYPE data RETURNING VALUE(result) TYPE string,
      from_json IMPORTING json TYPE string CHANGING data TYPE data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: json_class type seoclname.
ENDCLASS.

CLASS zcl_llm_common IMPLEMENTATION.
  METHOD from_json.
    CALL METHOD (json_class)=>deserialize
      EXPORTING
        json        = json
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      CHANGING
        data        = data.
  ENDMETHOD.

  METHOD to_json.
    CALL METHOD (json_class)=>serialize
      EXPORTING
        data        = data
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
        compress    = abap_true
      RECEIVING
        r_json      = result.
  ENDMETHOD.

  METHOD class_constructor.
    DATA enc_handler TYPE REF TO zllm_implementation.
    GET BADI enc_handler.
    CALL BADI enc_handler->get_json_impl RECEIVING result = json_class.
  ENDMETHOD.

ENDCLASS.
