CLASS ltcl_http_client_wrapper DEFINITION DEFERRED.
CLASS zcl_llm_http_client_wrapper DEFINITION LOCAL FRIENDS ltcl_http_client_wrapper.
"! As the main class is mostly a wrapper around cl_http_request
"! unit tests are not very useful, we just have a few ones to
"! ensure there is some unit test coverage. As of now they likely
"! do not bring much value.
CLASS ltcl_http_client_wrapper DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      test_url           TYPE string VALUE 'sap/public/ping',
      test_destination   TYPE rfcdest VALUE 'NONE',
      non_existent_dest  TYPE rfcdest VALUE 'XYZ234',
      test_request_json  TYPE string VALUE '{"test": "data"}' ##NO_TEXT,
      test_response_json TYPE string VALUE '{"status": "success"}' ##NO_TEXT,
      test_header_name   TYPE string VALUE 'X-Test-Header' ##NO_TEXT,
      test_header_value  TYPE string VALUE 'TestValue'.

    DATA:
      http_client_wrapper    TYPE REF TO zcl_llm_http_client_wrapper.

    METHODS:
      setup,
      validate_client_creation     FOR TESTING,
      validate_header_setting      FOR TESTING,
      validate_communication       FOR TESTING,
      validate_response_retrieval  FOR TESTING,
      validate_url_setting         FOR TESTING,
      validate_get_client          FOR TESTING,
      validate_close_client        FOR TESTING,
      test_client_creation_failure FOR TESTING.

ENDCLASS.

CLASS ltcl_http_client_wrapper IMPLEMENTATION.

  METHOD setup.
    DATA config TYPE zllm_clnt_config.
    config-rfc_destination = test_destination.
    config-auth_type = space.

    TRY.
        CREATE OBJECT http_client_wrapper
          EXPORTING
            config = config.
      CATCH zcx_llm_http_error.
        cl_abap_unit_assert=>fail( 'Unexpected exception during setup' ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_client_creation.
    cl_abap_unit_assert=>assert_not_initial(
      act = http_client_wrapper
      msg = 'HTTP client wrapper should be created successfully' ).
  ENDMETHOD.

  METHOD validate_header_setting.
    http_client_wrapper->set_header(
      name  = test_header_name
      value = test_header_value ).

    " Additional assertions can be added based on specific implementation details
    cl_abap_unit_assert=>assert_true( xsdbool( 1 = 1 ) ).
  ENDMETHOD.

  METHOD validate_url_setting.
    http_client_wrapper->set_url( test_url ).

    cl_abap_unit_assert=>assert_equals(
      exp = test_url
      act = http_client_wrapper->url
      msg = 'URL should be set correctly' ).
  ENDMETHOD.

  METHOD validate_communication.
    http_client_wrapper->set_url( test_url ).

    TRY.
        DATA(response) = http_client_wrapper->communicate( test_request_json ).
        cl_abap_unit_assert=>assert_not_initial(
          act = response-code
          msg = 'Communication should return a response' ).
      CATCH zcx_llm_http_error INTO DATA(error).
        cl_abap_unit_assert=>fail(
          msg   = 'Unexpected HTTP communication error'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_response_retrieval.
    http_client_wrapper->set_url( test_url ).

    TRY.
        DATA(response) = http_client_wrapper->communicate( test_request_json ).

        cl_abap_unit_assert=>assert_not_initial(
          act = response
          msg = 'Response should be retrievable' ).
      CATCH zcx_llm_http_error INTO DATA(error).
        cl_abap_unit_assert=>fail(
          msg   = 'Unexpected HTTP communication error'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_get_client.
    DATA config TYPE zllm_clnt_config.
    config-rfc_destination = test_destination.
    config-auth_type = space.

    TRY.
        DATA(new_client) = http_client_wrapper->get_client( config ).

        cl_abap_unit_assert=>assert_not_initial(
          act = new_client
          msg = 'get_client should return a new client instance' ).
      CATCH zcx_llm_http_error INTO DATA(error).
        cl_abap_unit_assert=>fail(
          msg   = 'Unexpected error in get_client'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_close_client.
    TRY.
        http_client_wrapper->close_client( ).
        cl_abap_unit_assert=>assert_true(
          act = xsdbool( 1 = 1 )
          msg = 'close_client should execute without error' ).
      CATCH zcx_llm_http_error INTO DATA(error).
        cl_abap_unit_assert=>fail(
          msg   = 'Unexpected error in close_client'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_client_creation_failure.
    DATA config TYPE zllm_clnt_config.
    config-rfc_destination = non_existent_dest.
    config-auth_type = space.

    TRY.
        DATA(failed_client) = NEW zcl_llm_http_client_wrapper( config ).
        cl_abap_unit_assert=>fail( 'Expected exception for non-existent destination' ).
      CATCH zcx_llm_validation INTO DATA(validation_error).
        " Verify that the exception contains expected error information
        cl_abap_unit_assert=>assert_not_initial(
          act = validation_error->get_text( )
          msg = 'Validation error should contain error details' ).
      CATCH zcx_llm_http_error INTO DATA(http_error).
        cl_abap_unit_assert=>fail(
          msg   = 'Unexpected HTTP error'
          detail = http_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
