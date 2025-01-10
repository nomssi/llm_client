CLASS ltcl_template_parser_test DEFINITION DEFERRED.
CLASS zcl_llm_template_parser DEFINITION LOCAL FRIENDS ltcl_template_parser_test.
CLASS ltcl_template_parser_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CONSTANTS newline TYPE string VALUE cl_abap_char_utilities=>newline.

    DATA:
      parser TYPE REF TO zcl_llm_template_parser.

    METHODS:
      setup,
      basic_variable_substitution FOR TESTING,
      test_nested_data FOR TESTING,
      test_if_condition FOR TESTING,
      test_if_else_condition FOR TESTING,
      test_nested_if_conditions FOR TESTING,
      test_for_loop FOR TESTING,
      test_nested_loop FOR TESTING,
      test_loop_metadata FOR TESTING,
      test_filters FOR TESTING,
      test_escaped_chars FOR TESTING,
      test_comments FOR TESTING,
      test_error_cases FOR TESTING RAISING cx_static_check,
      test_complex_conditions FOR TESTING,
      test_nested_variable_resolutio FOR TESTING,
      test_condition_operators FOR TESTING,
      test_variable_resolution_error FOR TESTING,
      test_variable_resolution_types FOR TESTING,
      test_format_table FOR TESTING.

ENDCLASS.

CLASS ltcl_template_parser_test IMPLEMENTATION.

  METHOD setup.
    parser = NEW #( ).
  ENDMETHOD.

  METHOD basic_variable_substitution.
    " Test Data
    TYPES:
      BEGIN OF test_data,
        name    TYPE string,
        company TYPE string,
      END OF test_data.

    DATA(template_content) = CONV string( 'Hello {{name}}, welcome to {{company}}!' ).
    DATA(expected_result) = CONV string( 'Hello John, welcome to ACME Corp!' ).

    " Create context
    DATA context TYPE test_data.
    context = VALUE #( name    = 'John'
                       company = 'ACME Corp' ).
    DATA(context_ref) = REF #( context ).

    " Add template and render
    TRY.
        parser->add_template( name    = 'greeting'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'greeting'
                                       context       = context_ref ).

        " Assert
        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'Basic variable substitution failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_nested_data.
    " Test Data
    TYPES: BEGIN OF address_type,
             street  TYPE string,
             city    TYPE string,
             country TYPE string,
           END OF address_type.
    TYPES: BEGIN OF person_type,
             name    TYPE string,
             age     TYPE i,
             address TYPE address_type,
           END OF person_type.

    DATA(template_content) =
      |Name: \{\{name\}\}{ newline }| &&
      |Age: \{\{age\}\}{ newline }| &&
      |Address:{ newline }| &&
      |  \{\{address.street\}\}{ newline }| &&
      |  \{\{address.city\}\}, \{\{address.country\}\}|.

    DATA(expected_result) =
      |Name: Alice{ newline }| &&
      |Age: 30{ newline }| &&
      |Address:{ newline }| &&
      |  123 Main St{ newline }| &&
      |  New York, USA|.

    " Create context
    DATA context TYPE person_type.
    context = VALUE #( name    = 'Alice'
                       age     = 30
                       address = VALUE #( street  = '123 Main St'
                                          city    = 'New York'
                                          country = 'USA' ) ).
    DATA(context_ref) = REF #( context ).

    TRY.

        " Add template and render
        parser->add_template( name    = 'person_details'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'person_details'
                                       context       = context_ref ).

        " Assert
        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'Nested data structure rendering failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_if_condition.
    TYPES:
      BEGIN OF test_data,
        name   TYPE string,
        active TYPE abap_bool,
      END OF test_data.

    DATA(template_content) =
      |\{% if active %\}| &&
      |User \{\{name\}\} is active| &&
      |\{% endif %\}|.

    DATA(expected_result) = |User John is active|.

    DATA context TYPE test_data.
    context = VALUE #( name   = 'John'
                       active = abap_true ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'condition_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'condition_test'
                                       context       = context_ref ).

        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'If condition failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_if_else_condition.
    TYPES:
      BEGIN OF test_data,
        name   TYPE string,
        active TYPE abap_bool,
      END OF test_data.

    DATA(template_content) =
      |\{% if active %\}| &&
      |User \{\{name\}\} is active| &&
      |\{% else %\}| &&
      |User \{\{name\}\} is inactive| &&
      |\{% endif %\}|.

    DATA context TYPE test_data.
    context = VALUE #( name   = 'John'
                       active = abap_false ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'if_else_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'if_else_test'
                                       context       = context_ref ).

        cl_abap_unit_assert=>assert_equals( exp = 'User John is inactive'
                                            act = result
                                            msg = 'If-else condition failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_nested_if_conditions.
    TYPES:
      BEGIN OF test_data,
        name   TYPE string,
        active TYPE abap_bool,
        admin  TYPE abap_bool,
      END OF test_data.

    DATA(template_content) =
      |\{% if active %\}| &&
      |\{% if admin %\}| &&
      |Admin \{\{name\}\} is active| &&
      |\{% else %\}| &&
      |User \{\{name\}\} is active| &&
      |\{% endif %\}| &&
      |\{% endif %\}|.

    DATA context TYPE test_data.
    context = VALUE #( name   = 'John'
                       active = abap_true
                       admin  = abap_true ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'nested_if_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'nested_if_test'
                                       context       = context_ref ).

        cl_abap_unit_assert=>assert_equals( exp = 'Admin John is active'
                                            act = result
                                            msg = 'Nested if conditions failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_for_loop.
    TYPES: BEGIN OF item_type,
             name  TYPE string,
             value TYPE i,
           END OF item_type,
           item_table_type TYPE STANDARD TABLE OF item_type WITH EMPTY KEY.
    TYPES: BEGIN OF test_data,
             items TYPE item_table_type,
           END OF test_data.

    DATA(template_content) =
      |Items:{ newline }| &&
      |\{% for item in items %\}| &&
      |- \{\{item.name\}\}: \{\{item.value\}\}{ newline }| &&
      |\{% endfor %\}|.

    DATA context TYPE test_data.
    context-items = VALUE #( ( name = 'Item 1' value = 10 )
                             ( name = 'Item 2' value = 20 )
                             ( name = 'Item 3' value = 30 ) ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'loop_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'loop_test'
                                       context       = context_ref ).

        DATA(expected_result) =
          |Items:{ newline }| &&
          |- Item 1: 10{ newline }| &&
          |- Item 2: 20{ newline }| &&
          |- Item 3: 30{ newline }|.

        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'For loop failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_loop_metadata.
    TYPES:
      BEGIN OF test_data,
        items TYPE string_table,
        loop  TYPE zcl_llm_template_parser=>loop_meta_type,
      END OF test_data.

    " Update template to include proper formatting
    DATA(template_content) =
      |\{% for item in items %\}| &&
      |\{\{item\}\}\{% if not loop.last %\}, \{% endif %\}| &&
      |\{% endfor %\}|.

    DATA context TYPE test_data.
    context-items = VALUE #( ( |A| ) ( |B| ) ( |C| ) ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'LOOP_META_TEST'
                            content = template_content ).

        DATA(result) = parser->render( template_name = 'LOOP_META_TEST'
                                     context       = context_ref ).

        cl_abap_unit_assert=>assert_equals(
            exp = 'A, B, C'
            act = result
            msg = 'Loop metadata failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_filters.
    TYPES:
      BEGIN OF test_data,
        name  TYPE string,
        empty TYPE string,
      END OF test_data.

    DATA(template_content) =
      |\{\{name\|upper\}\}{ newline }| &&
      |\{\{name\|lower\}\}{ newline }| &&
      |\{\{name\|capitalize\}\}{ newline }| &&
      |\{\{empty\|default("N/A")\}\}|.

    DATA context TYPE test_data.
    context = VALUE #( name  = 'John Doe'
                       empty = `` ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'filter_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'filter_test'
                                       context       = context_ref ).

        DATA(expected_result) =
          |JOHN DOE{ newline }| &&
          |john doe{ newline }| &&
          |John doe{ newline }| &&
          |N/A|.

        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'Filters failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_escaped_chars.
    DATA(template_content) =
      |\\\{\\\{ Not a variable \\\}\\\}{ newline }| &&
      |\\n creates a newline{ newline }| &&
      |\\ shows a backslash|.

    DATA context TYPE REF TO data.
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'escape_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'escape_test'
                                       context       = context_ref ).

        DATA(expected_result) =
          |\{\{ Not a variable \}\}{ newline }| &&
          |{ newline } creates a newline{ newline }| &&
          |\\ shows a backslash|.

        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'Escaped characters failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_comments.
    DATA(template_content) =
      |Before comment{ newline }| &&
      |\{# This is a comment #\}| &&
      |After comment|.

    DATA context TYPE REF TO data.
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'comment_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'comment_test'
                                       context       = context_ref ).

        DATA(expected_result) =
          |Before comment{ newline }After comment|.

        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'Comments failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_error_cases.
    " Test unclosed variable
    TRY.
        parser->add_template( name    = 'error_test'
                              content = 'Hello {{name' ).

        parser->render( template_name = 'error_test'
                        context       = REF #( ` ` ) ).

        cl_abap_unit_assert=>fail( 'Expected exception for unclosed variable' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>assert_bound( act = error
                                           msg = 'Exception should be raised for unclosed variable' ).
    ENDTRY.

    " Test invalid path
    TYPES: BEGIN OF test_data,
             name TYPE string,
           END OF test_data.

    DATA context TYPE test_data.
    context = VALUE #( name = 'John' ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'error_test2'
                              content = 'Hello {{invalid.path}}' ).

        parser->render( template_name = 'error_test2'
                        context       = context_ref ).

        cl_abap_unit_assert=>fail( 'Expected exception for invalid path' ).
      CATCH zcx_llm_template_parser INTO error.
        cl_abap_unit_assert=>assert_bound( act = error
                                           msg = 'Exception should be raised for invalid path' ).
    ENDTRY.

    " Test unclosed control structure
    TRY.
        parser->add_template( name    = 'error_test3'
                              content = '{% if true %}No endif' ).

        parser->render( template_name = 'error_test3'
                        context       = context_ref ).

        cl_abap_unit_assert=>fail( 'Expected exception for unclosed control structure' ).
      CATCH zcx_llm_template_parser INTO error.
        cl_abap_unit_assert=>assert_bound( act = error
                                           msg = 'Exception should be raised for unclosed control structure' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_nested_loop.
    " Define inner item type
    TYPES: BEGIN OF inner_item_type,
             name  TYPE string,
             value TYPE i,
           END OF inner_item_type,
           inner_table_type TYPE STANDARD TABLE OF inner_item_type WITH EMPTY KEY.

    " Define outer item type with inner items
    TYPES: BEGIN OF outer_item_type,
             category TYPE string,
             items    TYPE inner_table_type,
           END OF outer_item_type,
           outer_table_type TYPE STANDARD TABLE OF outer_item_type WITH EMPTY KEY.

    " Define context structure
    TYPES: BEGIN OF test_data,
             categories TYPE outer_table_type,
           END OF test_data.

    " Create template with nested loops
    DATA(template_content) =
      |Categories:{ newline }| &&
      |\{% for category in categories %\}| &&
      |\{\{category.category\}\}:{ newline }| &&
      |\{% for item in category.items %\}| &&
      |  - \{\{item.name\}\}: \{\{item.value\}\}{ newline }| &&
      |\{% endfor %\}| &&
      |\{% endfor %\}|.

    " Prepare test data
    DATA context TYPE test_data.

    " Add first category with items
    DATA(category1_items) = VALUE inner_table_type( ( name = 'Item 1A' value = 10 )
                                                    ( name = 'Item 1B' value = 20 ) ).

    " Add second category with items
    DATA(category2_items) = VALUE inner_table_type( ( name = 'Item 2A' value = 30 )
                                                    ( name = 'Item 2B' value = 40 ) ).

    " Build complete context
    context-categories = VALUE #( ( category = 'Category 1' items = category1_items )
                                  ( category = 'Category 2' items = category2_items ) ).

    DATA(context_ref) = REF #( context ).

    TRY.
        " Add template and render
        parser->add_template( name    = 'nested_loop_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'nested_loop_test'
                                       context       = context_ref ).

        " Define expected result
        DATA(expected_result) =
          |Categories:{ newline }| &&
          |Category 1:{ newline }| &&
          |  - Item 1A: 10{ newline }| &&
          |  - Item 1B: 20{ newline }| &&
          |Category 2:{ newline }| &&
          |  - Item 2A: 30{ newline }| &&
          |  - Item 2B: 40{ newline }|.

        " Assert
        cl_abap_unit_assert=>assert_equals( exp = expected_result
                                            act = result
                                            msg = 'Nested loops failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_complex_conditions.
    TYPES: BEGIN OF test_data,
             value1 TYPE i,
             value2 TYPE i,
             value3 TYPE string,
             value4 TYPE abap_bool,
           END OF test_data.

    DATA(template_content) =
      |\{% if value1 > value2 and value3 == "test" %\}| &&
      |Condition 1 met| &&
      |\{% elif value4 or value1 < value2 %\}| &&
      |Condition 2 met| &&
      |\{% else %\}| &&
      |No condition met| &&
      |\{% endif %\}|.

    DATA context TYPE test_data.
    context = VALUE #( value1 = 10
                       value2 = 5
                       value3 = 'test'
                       value4 = abap_false ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'complex_condition_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'complex_condition_test'
                                       context       = context_ref ).

        cl_abap_unit_assert=>assert_equals( exp = 'Condition 1 met'
                                            act = result
                                            msg = 'Complex conditions failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_variable_resolution_types.
    TYPES: BEGIN OF nested_struct,
             field1 TYPE i,
             field2 TYPE string,
           END OF nested_struct.

    TYPES: BEGIN OF test_data,
             int_value    TYPE i,
             string_value TYPE string,
             bool_value   TYPE abap_bool,
             nested       TYPE nested_struct,
             table        TYPE STANDARD TABLE OF nested_struct WITH EMPTY KEY,
           END OF test_data.

    DATA(template_content) =
      |Int: \{\{int_value\}\}{ newline }| &&
      |String: \{\{string_value\}\}{ newline }| &&
      |Bool: \{\{bool_value\}\}{ newline }| &&
      |Nested: \{\{nested.field1\}\} - \{\{nested.field2\}\}{ newline }| &&
      |Table[1]: \{\{table[1].field1\}\}|.

    DATA context TYPE test_data.
    context = VALUE #( int_value    = 42
                       string_value = 'test'
                       bool_value   = abap_true
                       nested       = VALUE #( field1 = 1
                                               field2 = 'nested' )
                       table        = VALUE #( ( field1 = 10 field2 = 'row1' ) ) ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'type_test'
                              content = template_content ).

        DATA(result) = parser->render( template_name = 'type_test'
                                       context       = context_ref ).

        DATA(expected) =
          |Int: 42{ newline }| &&
          |String: test{ newline }| &&
          |Bool: X{ newline }| &&
          |Nested: 1 - nested{ newline }| &&
          |Table[1]: 10|.

        cl_abap_unit_assert=>assert_equals( exp = expected
                                            act = result
                                            msg = 'Variable resolution for different types failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_variable_resolution_error.
    TYPES: BEGIN OF test_data,
             table TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           END OF test_data.

    " Test invalid table index
    DATA(template1) = `{{table[999].field}}`.
    DATA context TYPE test_data.
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'error_test1'
                              content = template1 ).
        parser->render( template_name = 'error_test1'
                        context       = context_ref ).
        cl_abap_unit_assert=>fail( 'Should raise exception' ).
      CATCH zcx_llm_template_parser INTO DATA(error). "#EC EMPTY_CATCH
    ENDTRY.

    " Test invalid component access
    DATA(template2) = `{{table.invalid_field}`.
    TRY.
        parser->add_template( name    = 'error_test2'
                              content = template2 ).
        parser->render( template_name = 'error_test2'
                        context       = context_ref ).
        cl_abap_unit_assert=>fail( 'Should raise exception' ).
      CATCH zcx_llm_template_parser INTO error. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_condition_operators.
    TYPES: BEGIN OF test_data,
             value1 TYPE i,
             value2 TYPE i,
           END OF test_data.

    " Test each operator
    DATA(tests) = VALUE string_table( ( |\{% if value1 == value2 %\}eq\{% endif %\}| )
                                      ( |\{% if value1 != value2 %\}ne\{% endif %\}| )
                                      ( |\{% if value1 > value2 %\}gt\{% endif %\}| )
                                      ( |\{% if value2 < value1 %\}lt\{% endif %\}| )
                                      ( |\{% if value1 >= value2 %\}ge\{% endif %\}| )
                                      ( |\{% if value2 <= value1 %\}le\{% endif %\}| ) ).

    DATA context TYPE test_data.
    context = VALUE #( value1 = 10
                       value2 = 5 ).
    DATA(context_ref) = REF #( context ).

    DATA(iteration) = 1.
    LOOP AT tests INTO DATA(test_template).
      TRY.
          parser->add_template( name    = |operator_test_{ sy-tabix }|
                                content = test_template ).
          DATA(result) = parser->render( template_name = |operator_test_{ sy-tabix }|
                                         context       = context_ref ).

          " Assert based on expected operator behavior
          CASE iteration.
            " ==
            WHEN 1.
              cl_abap_unit_assert=>assert_equals( exp = ''
                                                  act = result ).
            " !=
            WHEN 2.
              cl_abap_unit_assert=>assert_equals( exp = 'ne'
                                                  act = result ).
            " >
            WHEN 3.
              cl_abap_unit_assert=>assert_equals( exp = 'gt'
                                                  act = result ).
            " <
            WHEN 4.
              cl_abap_unit_assert=>assert_equals( exp = 'lt'
                                                  act = result ).
            " >=
            WHEN 5.
              cl_abap_unit_assert=>assert_equals( exp = 'ge'
                                                  act = result ).
            " <=
            WHEN 6.
              cl_abap_unit_assert=>assert_equals( exp = 'le'
                                                  act = result ).
          ENDCASE.
          iteration = iteration + 1.
        CATCH zcx_llm_template_parser INTO DATA(error).
          cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD test_nested_variable_resolutio.
    TYPES: BEGIN OF level3,
             value TYPE string,
           END OF level3.

    TYPES: BEGIN OF level2,
             data  TYPE level3,
             table TYPE STANDARD TABLE OF level3 WITH EMPTY KEY,
           END OF level2.

    TYPES: BEGIN OF level1,
             nested TYPE level2,
           END OF level1.

    DATA(template) =
      |Deep: \{\{nested.data.value\}\}{ newline }| &&
      |Array: \{\{nested.table[1].value\}\}|.

    DATA context TYPE level1.
    context = VALUE #( nested = VALUE #( data  = VALUE #( value = 'deep' )
                                         table = VALUE #( ( value = 'array' ) ) ) ).
    DATA(context_ref) = REF #( context ).

    TRY.
        parser->add_template( name    = 'nested_test'
                              content = template ).
        DATA(result) = parser->render( template_name = 'nested_test'
                                       context       = context_ref ).

        DATA(expected) =
          |Deep: deep{ newline }| &&
          |Array: array|.

        cl_abap_unit_assert=>assert_equals( exp = expected
                                            act = result
                                            msg = 'Nested variable resolution failed' ).
      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Parser Exception raised unexpectedly{ error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_format_table.
    " Test case 1: Elementary type table (strings)
    TYPES string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA string_tab TYPE string_table.
    string_tab = VALUE #( ( |first| ) ( |second| ) ( |third| ) ).

    " Test case 2: Structure table
    TYPES: BEGIN OF struct_type,
             id     TYPE i,
             name   TYPE string,
             active TYPE abap_bool,
           END OF struct_type,
           struct_table TYPE STANDARD TABLE OF struct_type WITH EMPTY KEY.

    DATA struct_tab TYPE struct_table.
    struct_tab = VALUE #(
      ( id = 1 name = 'One'   active = abap_true )
      ( id = 2 name = 'Two'   active = abap_false )
      ( id = 3 name = 'Three' active = abap_true ) ).

    " Test case 3: Nested table
    TYPES: BEGIN OF nested_type,
             items TYPE string_table,
           END OF nested_type,
           nested_table TYPE STANDARD TABLE OF nested_type WITH EMPTY KEY.

    DATA nested_tab TYPE nested_table.
    nested_tab = VALUE #(
      ( items = VALUE #( ( |a| ) ( |b| ) ) )
      ( items = VALUE #( ( |c| ) ( |d| ) ) ) ).

    TRY.
        " Test elementary type table
        DATA(result1) = parser->format_table( string_tab ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'first, second, third'  " Note the space after comma
          act = result1
          msg = 'Elementary type table formatting failed' ).

        " Test structure table
        DATA(result2) = parser->format_table( struct_tab ).
        " Note: Component names are in uppercase and ABAP_BOOL is X or space
        cl_abap_unit_assert=>assert_char_cp(
          act = result2
          exp = '*ID: 1*NAME: One*ACTIVE: X*' ).
        cl_abap_unit_assert=>assert_char_cp(
          act = result2
          exp = '*ID: 2*NAME: Two*ACTIVE: *' ).  " space for false
        cl_abap_unit_assert=>assert_char_cp(
          act = result2
          exp = '*ID: 3*NAME: Three*ACTIVE: X*' ).

        " Verify the format is [key: value; key: value]
        cl_abap_unit_assert=>assert_char_cp(
          act = result2
          exp = '[*; *; *]' ).  " Note the space after semicolon

        " Test nested table
        DATA(result3) = parser->format_table( nested_tab ).
        cl_abap_unit_assert=>assert_equals(
          exp = '[ITEMS: [NESTED TABLE]; ITEMS: [NESTED TABLE]ITEMS: [NESTED TABLE]]'
          act = result3
          msg = 'Nested table formatting failed' ).

        " Test empty table
        DATA empty_tab TYPE string_table.
        DATA(result4) = parser->format_table( empty_tab ).
        cl_abap_unit_assert=>assert_equals(
          exp = ''
          act = result4
          msg = 'Empty table formatting failed' ).

        " Test table with special characters
        DATA special_tab TYPE string_table.
        special_tab = VALUE #(
          ( |text with spaces| )
          ( |text,with,commas| )
          ( |text;with;semicolons| ) ).
        DATA(result5) = parser->format_table( special_tab ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'text with spaces, text,with,commas, text;with;semicolons'
          act = result5
          msg = 'Special character table formatting failed' ).

      CATCH zcx_llm_template_parser INTO DATA(error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
