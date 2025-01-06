CLASS ltcl_llm_common DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF test_structure,
        name   TYPE string,
        number TYPE i,
      END OF test_structure.

    METHODS setup.
    METHODS teardown.

    METHODS convert_struct_to_json
        FOR TESTING.
    METHODS convert_json_to_struct
        FOR TESTING.
    METHODS convert_empty_struct
        FOR TESTING.

    DATA:
      test_data TYPE test_structure.
ENDCLASS.

CLASS ltcl_llm_common IMPLEMENTATION.

  METHOD setup.
    " Prepare test data
    test_data-name = 'Test Name'.
    test_data-number = 42.

  ENDMETHOD.

  METHOD teardown.
    " Clear test data
    CLEAR test_data.
  ENDMETHOD.

  METHOD convert_struct_to_json.
    " Given
    DATA(expected_json) = '{"name":"Test Name","number":42}'.

    " When
    DATA(result_json) = zcl_llm_common=>to_json( test_data ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      exp = expected_json
      act = result_json
      msg = 'JSON conversion failed'
    ).
  ENDMETHOD.

  METHOD convert_json_to_struct.
    " Given
    DATA(input_json) = `{"name":"Test Name","number":42}`.
    DATA(expected_structure) = test_data.

    " When
    zcl_llm_common=>from_json(
      EXPORTING json = input_json
      CHANGING  data = test_data
    ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      exp = expected_structure
      act = test_data
      msg = 'JSON deserialization failed'
    ).
  ENDMETHOD.

  METHOD convert_empty_struct.
    " Given
    DATA(empty_structure) = VALUE test_structure( ).
    DATA(expected_json) = '{}'.

    " When
    DATA(result_json) = zcl_llm_common=>to_json( empty_structure ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      exp = expected_json
      act = result_json
      msg = 'Empty structure JSON conversion failed'
    ).
  ENDMETHOD.
ENDCLASS.
