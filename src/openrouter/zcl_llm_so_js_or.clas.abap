CLASS zcl_llm_so_js_or DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_so_js
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      pre_schema REDEFINITION,
      post_schema REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_so_js_or IMPLEMENTATION.

  METHOD pre_schema.
    append_to_schema( |"name":"Response","strict":true,"schema":\{| ).
  ENDMETHOD.

  METHOD post_schema.
    append_to_schema( |\}| ).
  ENDMETHOD.

ENDCLASS.
