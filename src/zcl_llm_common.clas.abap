CLASS zcl_llm_common DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-METHODS to_json IMPORTING !data         TYPE data
                                    compress      TYPE sap_bool DEFAULT abap_true
                          RETURNING VALUE(result) TYPE string.

    CLASS-METHODS from_json IMPORTING json  TYPE string
                            CHANGING  !data TYPE data.

    CLASS-METHODS get_llm_badi RETURNING VALUE(result) TYPE REF TO zllm_implementation.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: json_class TYPE seoclname,
                llm_badi   TYPE REF TO zllm_implementation.
ENDCLASS.

CLASS zcl_llm_common IMPLEMENTATION.
  METHOD from_json.
    CALL METHOD (json_class)=>deserialize
      EXPORTING json        = json
                pretty_name = /ui2/cl_json=>pretty_mode-low_case
      CHANGING  data        = data.
  ENDMETHOD.

  METHOD to_json.
    CALL METHOD (json_class)=>serialize
      EXPORTING data        = data
                pretty_name = /ui2/cl_json=>pretty_mode-low_case
                compress    = compress
      RECEIVING r_json      = result.
  ENDMETHOD.

  METHOD class_constructor.
    GET BADI llm_badi.
    CALL BADI llm_badi->get_json_impl
      RECEIVING result = json_class.
  ENDMETHOD.

  METHOD get_llm_badi.
    result = llm_badi.
  ENDMETHOD.

ENDCLASS.
