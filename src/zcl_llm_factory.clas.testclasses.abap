CLASS ltcl_llm_factory DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      mc_valid_model   TYPE zllm_model VALUE 'VALID_MODEL',
      mc_invalid_model TYPE zllm_model VALUE 'INVALID_MODEL'.

    CLASS-DATA:
      mo_sql_test_double TYPE REF TO if_osql_test_environment.

    METHODS:
      get_client_valid_model FOR TESTING,
      get_client_invalid_model FOR TESTING.
    CLASS-METHODS:
      class_setup,
      class_teardown.

ENDCLASS.

CLASS ltcl_llm_factory IMPLEMENTATION.

  METHOD class_setup.
    " Create SQL test double environment for zllm_config table
    mo_sql_test_double = cl_osql_test_environment=>create( VALUE #( ( 'ZLLM_CLNT_CONFIG' ) ) ).

    " Prepare test data using test double
    DATA lt_config_data TYPE STANDARD TABLE OF zllm_clnt_config.

    lt_config_data = VALUE #(
      ( model = mc_valid_model provider = 'LTCL_MOCK_LLM_CLIENT' )
    ).

    " Mock the database table content
    mo_sql_test_double->insert_test_data( lt_config_data ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up test double environment
    mo_sql_test_double->destroy( ).
  ENDMETHOD.

  METHOD get_client_valid_model.
    " Test getting a client for a valid model
    DATA:
      client TYPE REF TO zif_llm_client.

    " Test getting client for valid model
    TRY.
        client = zcl_llm_factory=>zif_llm_factory~get_client( mc_valid_model ).

        " Assert that client is not initial
        cl_abap_unit_assert=>assert_not_initial(
          act = client
          msg = 'Client should be created for valid model' ).

      CATCH zcx_llm_validation INTO DATA(error).
        cl_abap_unit_assert=>fail( 'Unexpected validation error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_client_invalid_model.
    " Test getting a client for an invalid model
    DATA:
      client TYPE REF TO zif_llm_client,
      error  TYPE REF TO zcx_llm_validation.

    " Test getting client for invalid model
    TRY.
        client = zcl_llm_factory=>zif_llm_factory~get_client( mc_invalid_model ).

        " If no exception is raised, fail the test
        cl_abap_unit_assert=>fail( 'Expected validation error not raised' ).

      CATCH zcx_llm_validation INTO error.
        " Assert that the exception was raised with the correct parameters
        cl_abap_unit_assert=>assert_not_initial(
          act = error
          msg = 'Validation error should be raised' ).

        cl_abap_unit_assert=>assert_equals(
          exp = 'ZLLM_CLIENT'
          act = error->if_t100_message~t100key-msgid
          msg = 'Incorrect message ID' ).

        cl_abap_unit_assert=>assert_equals(
          exp = '002'
          act = error->if_t100_message~t100key-msgno
          msg = 'Incorrect message number' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

" Mock LLM client class for testing
CLASS ltcl_mock_llm_client DEFINITION
  FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_llm_client.
ENDCLASS.

CLASS ltcl_mock_llm_client IMPLEMENTATION.

  METHOD zif_llm_client~chat.
    "Not used in this test
  ENDMETHOD.

  METHOD zif_llm_client~new_request.
  ENDMETHOD.

  METHOD zif_llm_client~get_client.
    " Create a mock client instance
    response = NEW ltcl_mock_llm_client( ).
  ENDMETHOD.

ENDCLASS.
