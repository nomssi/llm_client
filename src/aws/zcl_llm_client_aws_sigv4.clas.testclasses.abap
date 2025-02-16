CLASS ltcl_aws_sigv4 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      c_access_key TYPE string VALUE 'AKIDEXAMPLE',
      c_secret_key TYPE string VALUE 'wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY',
      c_region     TYPE string VALUE 'us-east-1',
      c_service    TYPE string VALUE 'service'.

    DATA:
      cut TYPE REF TO zcl_llm_client_aws_sigv4.

    METHODS:
      setup,
      test_get FOR TESTING.
ENDCLASS.

CLASS ltcl_aws_sigv4 IMPLEMENTATION.
  METHOD setup.
    DATA(credentials) = VALUE zcl_llm_client_aws_sigv4=>auth_credentials(
      access_key = c_access_key
      secret_key = c_secret_key
      region     = c_region
      service    = c_service
    ).

    cut = NEW #( credentials ).
  ENDMETHOD.

  METHOD test_get.
    " Test case for GET /test.txt
    DATA(headers) = VALUE tihttpnvp(
      ( name = 'Host'           value = 'example.amazonaws.com' )
      ( name = 'x-amz-date'     value = '20150830T123600Z' )
    ).

    DATA request_time TYPE timestamp.
    CONVERT DATE '20150830' TIME '123600'
       INTO TIME STAMP request_time TIME ZONE 'UTC'.

    DATA(auth) = cut->get_auth_header(
      request_method = 'GET'
      request_uri    = '/'
      headers        = headers
      payload        = ''
      request_time   = request_time
    ).

    cl_abap_unit_assert=>assert_equals(
      act = auth
      exp = |AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/aws4_request, |
          && |SignedHeaders=host;x-amz-date, Signature=5fa00fa31553b73ebf1942676e86291e8372ff2a2260956d9b8aae1d763fbf31| ).
  ENDMETHOD.
ENDCLASS.
