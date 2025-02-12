CLASS ltcl_calculator_tool DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA calculator TYPE REF TO zcl_llm_tool_calculator.

    METHODS setup.
    METHODS addition                     FOR TESTING.
    METHODS subtraction                  FOR TESTING.
    METHODS multiplication               FOR TESTING.
    METHODS division                     FOR TESTING.
    METHODS power                        FOR TESTING.
    METHODS modulo                       FOR TESTING.
    METHODS complex_expression           FOR TESTING.
    METHODS spaces_in_expression         FOR TESTING.
    METHODS division_by_zero             FOR TESTING.
    METHODS invalid_expression           FOR TESTING.
    METHODS check_tool_details           FOR TESTING.
    METHODS complex_expressions_brackets FOR TESTING.
    METHODS power_operations             FOR TESTING.
    METHODS power_operation_errors       FOR TESTING.
ENDCLASS.

CLASS ltcl_calculator_tool IMPLEMENTATION.
  METHOD setup.
    calculator = NEW #( ).
  ENDMETHOD.

  METHOD addition.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '2 + 2' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '4'
                                        act = output-result ).
  ENDMETHOD.

  METHOD subtraction.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '5 - 3' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '2'
                                        act = output-result ).
  ENDMETHOD.

  METHOD multiplication.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '4 * 3' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '12'
                                        act = output-result ).
  ENDMETHOD.

  METHOD division.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '10 / 2' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '5'
                                        act = output-result ).
  ENDMETHOD.

  METHOD power.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '2 ** 3' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '8'
                                        act = output-result ).
  ENDMETHOD.

  METHOD modulo.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '10 MOD 3' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '1'
                                        act = output-result ).
  ENDMETHOD.

  METHOD complex_expression.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '(5 + 3) * 2 - 4' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '12'
                                        act = output-result ).
  ENDMETHOD.

  METHOD spaces_in_expression.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '  2   +    2  ' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '4'
                                        act = output-result ).
  ENDMETHOD.

  METHOD division_by_zero.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = '1 / 0' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    cl_abap_unit_assert=>assert_char_cp( exp = 'Error:*'
                                         act = output-result ).
  ENDMETHOD.

  METHOD invalid_expression.
    DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = 'abc + 2' ).

    DATA(data) = REF #( input ).
    DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                    tool_call_id = 'test_id' ).

    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
    output = <output>.

    " The error message from cx_sy_conversion_no_number
    cl_abap_unit_assert=>assert_char_cp(
      exp = 'Error: Invalid expression: abc + 2'
      act = output-result ).
  ENDMETHOD.

  METHOD check_tool_details.
    DATA(tool_details) = calculator->zif_llm_tool~get_tool_details( ).

    cl_abap_unit_assert=>assert_equals( exp = 'calculator'
                                        act = tool_details-name ).

    cl_abap_unit_assert=>assert_equals( exp = zif_llm_tool=>type_function
                                        act = tool_details-type ).

    cl_abap_unit_assert=>assert_not_initial( tool_details-description ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( tool_details-parameters-descriptions ) ).
  ENDMETHOD.

  METHOD complex_expressions_brackets.
    " Structure for test cases
    TYPES: BEGIN OF test_case,
             expression TYPE string,
             expected   TYPE string,
           END OF test_case,
           test_cases TYPE STANDARD TABLE OF test_case WITH EMPTY KEY.

    DATA(test_cases) = VALUE test_cases( ( expression = '(5 + 3) * 2'     expected = '16' )
                                         ( expression = '2 + 3 * 4'       expected = '14' )
                                         ( expression = '(2 + 3) * 4'     expected = '20' )
                                         ( expression = '2 ** (2 + 1)'    expected = '8' )
                                         ( expression = '((4 + 2) * 3)'   expected = '18' )
                                         ( expression = '10 - (2 + 3)'    expected = '5' ) ).

    LOOP AT test_cases INTO DATA(test_case).
      DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = test_case-expression ).
      DATA(data) = REF #( input ).
      DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                       tool_call_id = 'test_id' ).
      DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
      ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
      output = <output>.
      cl_abap_unit_assert=>assert_equals( exp = test_case-expected
                                          act = output-result
                                          msg = |Expression: { test_case-expression }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD power_operations.
    TYPES: BEGIN OF test_case,
             expression TYPE string,
             expected   TYPE string,
           END OF test_case,
           test_cases TYPE STANDARD TABLE OF test_case WITH EMPTY KEY.

    DATA(test_cases) = VALUE test_cases( ( expression = '4 ** 0.5'        expected = '2' )         " Square root
                                         ( expression = '2 ** 0'          expected = '1' )         " Zero power
                                         ( expression = '2.5 ** 2'        expected = '6,25' )      " Decimal base
                                         ( expression = '3 ** 2 + 4'      expected = '13' )        " Combined with addition
                                         ( expression = '2 ** (3 ** 2)'   expected = '512' )       " Nested power
                                         ( expression = '(2 ** 3) ** 2'   expected = '64' )        " Parentheses
                                         ( expression = '2 ** -2'         expected = '0,25' )      " Negative exponent
                                         ( expression = '2 ** -3'         expected = '0,125' )     " Another negative exponent
                                         ( expression = '4 ** -0.5'       expected = '0,5' ) ).    " Negative fractional exponent

    LOOP AT test_cases INTO DATA(test_case).
      DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input( expression = test_case-expression ).

      DATA(data) = REF #( input ).
      DATA(result) = calculator->zif_llm_tool~execute( data         = data
                                                       tool_call_id = 'test_id' ).

      DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
      ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
      output = <output>.

      cl_abap_unit_assert=>assert_equals( exp = test_case-expected
                                          act = output-result
                                          msg = |Expression: { test_case-expression }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD power_operation_errors.
    TYPES: BEGIN OF test_case,
             expression TYPE string,
             exp_error  TYPE abap_bool,
           END OF test_case,
           test_cases TYPE STANDARD TABLE OF test_case WITH EMPTY KEY.

    DATA(test_cases) = VALUE test_cases(
      ( expression = '2 ** a'        exp_error = abap_true )  " Invalid operand
      ( expression = '2 ** '         exp_error = abap_true )  " Missing operand
      ( expression = '** 2'          exp_error = abap_true )  " Missing operand
      ( expression = '0 ** -1'       exp_error = abap_true )  " Division by zero
    ).

    LOOP AT test_cases INTO DATA(test_case).
      DATA(input) = VALUE zcl_llm_tool_calculator=>calculation_input(
        expression = test_case-expression
      ).

      DATA(data) = REF #( input ).
      DATA(result) = calculator->zif_llm_tool~execute(
        data         = data
        tool_call_id = 'test_id'
      ).

      DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
      ASSIGN result-data->* TO FIELD-SYMBOL(<output>).
      output = <output>.

      IF test_case-exp_error = abap_true.
        cl_abap_unit_assert=>assert_char_cp(
          exp = 'Error:*'
          act = output-result
          msg = |Expression: { test_case-expression }|
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
