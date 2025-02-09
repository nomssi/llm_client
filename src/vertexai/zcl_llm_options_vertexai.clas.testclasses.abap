CLASS ltcl_llm_options DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_llm_options_vertexai.

    METHODS setup.

    METHODS set_temperature_valid FOR TESTING.
    METHODS set_temperature_invalid FOR TESTING.

    METHODS set_top_p_valid FOR TESTING.
    METHODS set_top_p_invalid FOR TESTING.

    METHODS set_top_k_valid FOR TESTING.
    METHODS set_top_k_invalid FOR TESTING.

    METHODS set_seed_valid FOR TESTING.
    METHODS set_seed_invalid FOR TESTING.

    METHODS set_frequency_penalty_valid FOR TESTING.
    METHODS set_frequency_penalty_invalid FOR TESTING.

    METHODS set_presence_penalty_valid FOR TESTING.
    METHODS set_presence_penalty_invalid FOR TESTING.

    METHODS set_min_p_valid FOR TESTING.
    METHODS set_min_p_invalid FOR TESTING.

    METHODS set_top_a_valid FOR TESTING.
    METHODS set_top_a_invalid FOR TESTING.

    METHODS set_custom_parameters FOR TESTING.
    METHODS get_parameters FOR TESTING.

    METHODS set_custom_parameters_ovrwrte FOR TESTING.
ENDCLASS.                                           "#EC NUMBER_METHODS

CLASS ltcl_llm_options IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD set_temperature_valid.
    cut->set_temperature( CONV decfloat16( '1.0' ) ).
    cut->set_temperature( CONV decfloat16( '0.0' ) ).
    cut->set_temperature( CONV decfloat16( '2.0' ) ).
  ENDMETHOD.

  METHOD set_temperature_invalid.
    TRY.
        cut->set_temperature( CONV decfloat16( '-0.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for temperature below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        cut->set_temperature( CONV decfloat16( '2.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for temperature above 2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_top_p_valid.
    cut->set_top_p( CONV decfloat16( '0.0' ) ).
    cut->set_top_p( CONV decfloat16( '0.5' ) ).
    cut->set_top_p( CONV decfloat16( '1.0' ) ).
  ENDMETHOD.

  METHOD set_top_p_invalid.
    TRY.
        cut->set_top_p( CONV decfloat16( '-0.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_p below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        cut->set_top_p( CONV decfloat16( '1.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_p above 1' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_top_k_valid.
    cut->set_top_k( 1 ).
    cut->set_top_k( 10 ).
    cut->set_top_k( 100 ).
  ENDMETHOD.

  METHOD set_top_k_invalid.
    TRY.
        cut->set_top_k( 0 ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_k below 1' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_seed_valid.
    cut->set_seed( 0 ).
    cut->set_seed( 42 ).
    cut->set_seed( 100 ).
  ENDMETHOD.

  METHOD set_seed_invalid.
    TRY.
        cut->set_seed( -1 ).
        cl_abap_unit_assert=>fail( 'Expected exception for seed below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_frequency_penalty_valid.
    cut->set_frequency_penalty( CONV decfloat16( '-2.0' ) ).
    cut->set_frequency_penalty( CONV decfloat16( '0.0' ) ).
    cut->set_frequency_penalty( CONV decfloat16( '2.0' ) ).
  ENDMETHOD.

  METHOD set_frequency_penalty_invalid.
    TRY.
        cut->set_frequency_penalty( CONV decfloat16( '-2.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for frequency_penalty below -2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        cut->set_frequency_penalty( CONV decfloat16( '2.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for frequency_penalty above 2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_presence_penalty_valid.
    cut->set_presence_penalty( CONV decfloat16( '-2.0' ) ).
    cut->set_presence_penalty( CONV decfloat16( '0.0' ) ).
    cut->set_presence_penalty( CONV decfloat16( '2.0' ) ).
  ENDMETHOD.

  METHOD set_presence_penalty_invalid.
    TRY.
        cut->set_presence_penalty( CONV decfloat16( '-2.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for presence_penalty below -2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        cut->set_presence_penalty( CONV decfloat16( '2.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for presence_penalty above 2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_min_p_valid.
    cut->set_min_p( CONV decfloat16( '0.0' ) ).
    cut->set_min_p( CONV decfloat16( '0.5' ) ).
    cut->set_min_p( CONV decfloat16( '1.0' ) ).
  ENDMETHOD.

  METHOD set_min_p_invalid.
    TRY.
        cut->set_min_p( CONV decfloat16( '-0.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for min_p below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        cut->set_min_p( CONV decfloat16( '1.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for min_p above 1' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_top_a_valid.
    cut->set_top_a( CONV decfloat16( '0.0' ) ).
    cut->set_top_a( CONV decfloat16( '0.5' ) ).
    cut->set_top_a( CONV decfloat16( '1.0' ) ).
  ENDMETHOD.

  METHOD set_top_a_invalid.
    TRY.
        cut->set_top_a( CONV decfloat16( '-0.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_a below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        cut->set_top_a( CONV decfloat16( '1.1' ) ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_a above 1' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_custom_parameters.
    DATA parameters TYPE zllm_keyvalues.
    INSERT VALUE #( key = 'custom_param1' value = 'value1' ) INTO TABLE parameters.
    INSERT VALUE #( key = 'custom_param2' value = 'value2' ) INTO TABLE parameters.

    cut->set_custom_parameters( parameters ).

    DATA result TYPE zllm_keyvalues.
    result = cut->get_paramters( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( result )
    ).
  ENDMETHOD.

  METHOD get_parameters.
    cut->set_temperature( CONV decfloat16( '0.7' ) ).

    DATA result TYPE zllm_keyvalues.
    result = cut->get_paramters( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( result )
    ).

    LOOP AT result ASSIGNING FIELD-SYMBOL(<param>).
      cl_abap_unit_assert=>assert_equals(
        exp = 'temperature'
        act = <param>-key
      ).

      cl_abap_unit_assert=>assert_equals(
        exp = '0.7'
        act = <param>-value
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_custom_parameters_ovrwrte.
    " First set a temperature
    cut->set_temperature( CONV decfloat16( '0.7' ) ).

    " Prepare custom parameters with a different temperature
    DATA parameters TYPE zllm_keyvalues.
    INSERT VALUE #(
      key = 'temperature'
      value = '0.5'
    ) INTO TABLE parameters.

    " Set custom parameters
    cut->set_custom_parameters( parameters ).

    " Retrieve parameters
    DATA result TYPE zllm_keyvalues.
    result = cut->get_paramters( ).

    " Assert only one parameter exists
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( result )
    ).

    " Check that the value has been overwritten
    LOOP AT result ASSIGNING FIELD-SYMBOL(<param>).
      cl_abap_unit_assert=>assert_equals(
        exp = 'temperature'
        act = <param>-key
      ).

      cl_abap_unit_assert=>assert_equals(
        exp = '0.5'
        act = <param>-value
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
