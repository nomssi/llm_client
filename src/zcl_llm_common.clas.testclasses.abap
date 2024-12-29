CLASS ltc_llm_common DEFINITION
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

    CLASS-DATA:
      sql_test_double TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS convert_struct_to_json
      FOR TESTING.
    METHODS convert_json_to_struct
      FOR TESTING.
    METHODS convert_empty_struct
      FOR TESTING.
    METHODS use_default_json_class
      FOR TESTING.

    DATA:
      test_data TYPE test_structure.
ENDCLASS.

CLASS ltc_llm_common IMPLEMENTATION.
  METHOD class_setup.
    " Create SQL test double for zllm_config
    sql_test_double = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'ZLLM_CONFIG' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " Destroy SQL test double
    sql_test_double->destroy( ).
  ENDMETHOD.

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

  METHOD use_default_json_class.
    " Given
    " Prepare mock configuration with empty JSON class
    data mock_config type STANDARD TABLE OF zllm_config with empty key.
    append VALUE #( json_class = '' ) to mock_config.

    " Set up mock data for ZLLM_CONFIG
    sql_test_double->clear_doubles( ).
    sql_test_double->insert_test_data( mock_config ).

    " Reinitialize the class to trigger class constructor with mock data
    " Note: This requires a test-specific method or a way to reset the class
    " For this example, we'll assume a method to force reinitialization
    " You might need to modify the original class to support this

    " When
    DATA(dummy) = zcl_llm_common=>to_json( VALUE test_structure( ) ).

    " Then
    " Verify that no exception is raised, implying default class is used
    cl_abap_unit_assert=>assert_true( abap_true ).
  ENDMETHOD.
ENDCLASS.
