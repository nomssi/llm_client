"! <p class="shorttext synchronized" lang="en">OpenAI JSON Schema Implementation</p>
CLASS zcl_llm_so_js_oa DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_so_js
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_llm_so~set_schema REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      pre_schema REDEFINITION,
      post_schema REDEFINITION,
      post_object REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_so_js_oa IMPLEMENTATION.

  METHOD pre_schema.
    append_to_schema( |"name":"Response","strict":true,"schema":\{| ).
  ENDMETHOD.

  METHOD post_schema.
    append_to_schema( |\}| ).
  ENDMETHOD.

  METHOD post_object.
    append_to_schema( |,"additionalProperties":false| ).
  ENDMETHOD.

  METHOD zif_llm_so~set_schema.
    GET REFERENCE OF data INTO data_ref.

    DATA(type_descriptor) = cl_abap_typedescr=>describe_by_data( data ).
    me->descriptions = description.

    append_to_schema( |\{| ).
    pre_schema( ).

    CASE type_descriptor->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        process_type(
          type_descriptor = type_descriptor
          field = get_field_info( ) ).
        "OpenAI currently only support objects on top-level
        "WHEN cl_abap_typedescr=>kind_table.
        "  " For root-level tables, we need to create the array structure directly
        "  append_to_schema( |"type":"array","items":\{| ).
        "  DATA(table_descriptor) = CAST cl_abap_tabledescr( type_descriptor ).
        "  process_type(
        "    type_descriptor = table_descriptor->get_table_line_type( )
        "    field = get_field_info( ) ).
        "  append_to_schema( |\}| ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_llm_validation
          EXPORTING
            textid = zcx_llm_validation=>unsupported_type
            attr1  = |Unsupported type: { type_descriptor->kind }| ##NO_TEXT.
    ENDCASE.

    post_schema( ).
    append_to_schema( |\}| ).
  ENDMETHOD.
ENDCLASS.
