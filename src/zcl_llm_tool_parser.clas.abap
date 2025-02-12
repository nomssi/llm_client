"! <p class="shorttext synchronized" lang="en">Default Tool Parser using JSON Schema</p>
CLASS zcl_llm_tool_parser DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_llm_tool_parser.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF field_info,
        name        TYPE string ##NEEDED,
        path        TYPE string,
        description TYPE zif_llm_tool_parser=>def_description,
      END OF field_info.

    DATA descriptions TYPE zif_llm_tool_parser=>def_descriptions.
    DATA schema       TYPE string.

    METHODS pre_schema.
    METHODS post_schema.

    METHODS pre_object
      IMPORTING !field TYPE field_info ##NEEDED
      RAISING   zcx_llm_validation.

    METHODS post_object
      IMPORTING !field TYPE field_info ##NEEDED
      RAISING   zcx_llm_validation.

    METHODS pre_array.
    METHODS post_array.

    METHODS process_type
      IMPORTING type_descriptor TYPE REF TO cl_abap_typedescr
                !field          TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS process_structure
      IMPORTING structure_descriptor TYPE REF TO cl_abap_structdescr
                !field               TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS process_table
      IMPORTING table_descriptor TYPE REF TO cl_abap_tabledescr
                !field           TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS process_element
      IMPORTING element_descriptor TYPE REF TO cl_abap_elemdescr
                !field             TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS get_field_info
      IMPORTING !name         TYPE string OPTIONAL
                !path         TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE field_info.

    METHODS append_to_schema
      IMPORTING content TYPE string.

    METHODS get_path
      IMPORTING current_path  TYPE string OPTIONAL
                field_name    TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE string.

    METHODS get_enum_values
      IMPORTING !description  TYPE zif_llm_tool_parser=>def_description
      RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_tool_parser IMPLEMENTATION.
  METHOD append_to_schema.
    schema = schema && content.
  ENDMETHOD.

  METHOD get_enum_values.
    result = REDUCE string(
      INIT temp = ``
      FOR value IN description-enum_values
      INDEX INTO idx
      NEXT temp = COND #(
        WHEN idx = 1
        THEN |"{ value }"|
        ELSE |{ temp },"{ value }"| ) ).
  ENDMETHOD.

  METHOD get_field_info.
    result = VALUE #( name        = name
                      path        = path
                      description = VALUE #( descriptions[ fieldname = path ] OPTIONAL ) ).
  ENDMETHOD.

  METHOD get_path.
    result = COND #(
      WHEN current_path IS INITIAL
      THEN to_lower( field_name )
      ELSE |{ current_path }-{ to_lower( field_name ) }| ).
  ENDMETHOD.


  METHOD post_array.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE


  METHOD post_object.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE


  METHOD post_schema.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE


  METHOD pre_array.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE


  METHOD pre_object.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE


  METHOD pre_schema.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD process_element.
    IF field-name IS NOT INITIAL.
      append_to_schema( |"{ field-name }":\{"title":"{ to_upper( field-name(1) ) }{ field-name+1 }",| ).
    ENDIF.

    CASE element_descriptor->type_kind.
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int8.
        append_to_schema( |"type":"integer"| ).
      WHEN cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        append_to_schema( |"type":"number"| ).
      WHEN cl_abap_typedescr=>typekind_string.
        append_to_schema( |"type":"string"| ).
      WHEN cl_abap_typedescr=>typekind_char.
        IF     element_descriptor->output_length = 1
           AND '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD' CS element_descriptor->absolute_name.
          append_to_schema( |"type":"boolean"| ).
        ELSE.
          RAISE EXCEPTION NEW zcx_llm_validation(
                                  textid = zcx_llm_validation=>unsupported_type
                                  attr1  = |Unsupported elementary type: { element_descriptor->type_kind }| ) ##NO_TEXT.
        ENDIF.
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_llm_validation(
                                textid = zcx_llm_validation=>unsupported_type
                                attr1  = |Unsupported elementary type: { element_descriptor->type_kind }| ) ##NO_TEXT.
    ENDCASE.

    IF field-description-description IS NOT INITIAL.
      append_to_schema( |,"description":"{ escape( val    = field-description-description
                                                   format = cl_abap_format=>e_json_string ) }"| ).
    ENDIF.

    IF lines( field-description-enum_values ) > 0.
      append_to_schema( |,"enum":[{ get_enum_values( field-description ) }]| ).
    ENDIF.
    IF field-name IS NOT INITIAL.
      append_to_schema( |\}| ).
    ENDIF.
  ENDMETHOD.

  METHOD process_structure.
    IF field-name IS NOT INITIAL.
      append_to_schema( |,"{ field-name }":\{"title":"{ field-name }"| ).
      IF field-description-description IS NOT INITIAL.
        append_to_schema( |,"description":"{ escape( val    = field-description-description
                                                     format = cl_abap_format=>e_json_string ) }"| ).
      ENDIF.
    ENDIF.

    pre_object( field ).

    IF field-name IS NOT INITIAL.
      append_to_schema( |,"type":"object","properties":\{| ).
    ELSE.
      append_to_schema( |"type":"object","properties":\{| ).
    ENDIF.

    DATA(components) = structure_descriptor->get_components( ).
    DATA needs_comma TYPE abap_bool.

    LOOP AT components REFERENCE INTO DATA(component).
      " Add comma if needed from previous iteration
      IF needs_comma = abap_true.
        append_to_schema( |,| ).
      ENDIF.
      " Default to true for next iteration
      needs_comma = abap_true.

      DATA(child_field) = get_field_info( name = to_lower( component->name )
                                          path = get_path( current_path = field-path
                                                           field_name   = component->name ) ).

      process_type( type_descriptor = component->type
                    field           = child_field ).
    ENDLOOP.

    append_to_schema( |\},"required":[| ).
    append_to_schema( REDUCE string(
      INIT result = ``
      FOR comp IN components
      INDEX INTO idx
      NEXT result = COND #(
        WHEN idx = 1
        THEN |"{ to_lower( comp-name ) }"|
        ELSE |{ result },"{ to_lower( comp-name ) }"| ) ) ).
    append_to_schema( |],"additionalProperties":false| ).

    post_object( field ).

    IF field-name IS NOT INITIAL.
      append_to_schema( |\}| ).
    ENDIF.
  ENDMETHOD.

  METHOD process_table.
    IF field-name IS NOT INITIAL.
      append_to_schema( |"{ field-name }":\{"type":"array"| ).
    ENDIF.
    pre_array( ).

    IF field-description-description IS NOT INITIAL.
      append_to_schema( |,"description":"{ escape( val    = field-description-description
                                                   format = cl_abap_format=>e_json_string ) }"| ).
    ENDIF.
    append_to_schema( |,"items":\{| ).

    DATA(child_field) = get_field_info( path = COND #(
                                          WHEN field-path IS INITIAL AND field-name IS INITIAL THEN ''
                                          WHEN field-path IS INITIAL                           THEN field-name
                                          ELSE                                                      field-path ) ).

    DATA(line_type) = table_descriptor->get_table_line_type( ).
    process_type( type_descriptor = line_type
                  field           = child_field ).

    append_to_schema( |\}| ).
    post_array( ).
    IF field-name IS NOT INITIAL.
      append_to_schema( |\}| ).
    ENDIF.
  ENDMETHOD.

  METHOD process_type.
    CASE type_descriptor->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        process_structure( structure_descriptor = CAST cl_abap_structdescr( type_descriptor )
                           field                = field ).
      WHEN cl_abap_typedescr=>kind_table.
        process_table( table_descriptor = CAST cl_abap_tabledescr( type_descriptor )
                       field            = field ).
      WHEN cl_abap_typedescr=>kind_elem.
        process_element( element_descriptor = CAST cl_abap_elemdescr( type_descriptor )
                         field              = field ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_llm_validation( textid = zcx_llm_validation=>unsupported_type
                                                attr1  = |Unsupported type: { type_descriptor->kind }| ) ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_llm_tool_parser~parse.
    " Check if the schema is already available, then just return it (we are called twice)
    IF schema IS NOT INITIAL.
      result = schema.
      RETURN.
    ENDIF.

    me->descriptions = descriptions.

    append_to_schema( |\{| ).
    pre_schema( ).

    CASE data_desc->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        process_type( type_descriptor = data_desc
                      field           = get_field_info( ) ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_llm_validation( textid = zcx_llm_validation=>unsupported_type
                                                attr1  = |Unsupported type: { data_desc->kind }| ) ##NO_TEXT.
    ENDCASE.

    post_schema( ).
    append_to_schema( |\}| ).
    result = schema.
  ENDMETHOD.
ENDCLASS.
