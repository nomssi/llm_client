CLASS zcl_llm_options_vertexai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_llm_options .

    ALIASES get_paramters
      FOR zif_llm_options~get_paramters .
    ALIASES set_custom_parameters
      FOR zif_llm_options~set_custom_parameters .
    ALIASES set_frequency_penalty
      FOR zif_llm_options~set_frequency_penalty .
    ALIASES set_min_p
      FOR zif_llm_options~set_min_p .
    ALIASES set_presence_penalty
      FOR zif_llm_options~set_presence_penalty .
    ALIASES set_seed
      FOR zif_llm_options~set_seed .
    ALIASES set_temperature
      FOR zif_llm_options~set_temperature .
    ALIASES set_top_a
      FOR zif_llm_options~set_top_a .
    ALIASES set_top_k
      FOR zif_llm_options~set_top_k .
    ALIASES set_top_p
      FOR zif_llm_options~set_top_p .
    ALIASES set_max_tokens
      FOR zif_llm_options~set_max_tokens .
  PROTECTED SECTION.

    DATA int_parameters TYPE zllm_keyvalues .

    METHODS validate_range_float
      IMPORTING
        !value TYPE decfloat16
        !min   TYPE decfloat16
        !max   TYPE decfloat16
      RAISING
        zcx_llm_validation .
    METHODS validate_range_int
      IMPORTING
        !value TYPE i
        !min   TYPE i
        !max   TYPE i OPTIONAL
      RAISING
        zcx_llm_validation .
    METHODS set_parameter
      IMPORTING
        !key   TYPE string
        !value TYPE string .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_llm_options_vertexai IMPLEMENTATION.

  METHOD set_parameter.
    " If parameter already exists, replace it
    DELETE int_parameters WHERE key = key.

    " Add new parameter
    INSERT VALUE #(
      key   = key
      value = value
    ) INTO TABLE int_parameters.
  ENDMETHOD.

  METHOD validate_range_float.
    IF value < min OR value > max.
      RAISE EXCEPTION NEW zcx_llm_validation(
          textid = zcx_llm_validation=>value_out_of_range
          attr1  = |Value { value } is out of range [{ min }, { max }]| ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD validate_range_int.
    IF value < min.
      RAISE EXCEPTION NEW zcx_llm_validation(
          textid = zcx_llm_validation=>value_out_of_range
          attr1  = |Value { value } is below minimum { min }| ) ##NO_TEXT.
    ENDIF.

    IF max IS NOT INITIAL AND value > max.
      RAISE EXCEPTION NEW zcx_llm_validation(
          textid = zcx_llm_validation=>value_out_of_range
          attr1  = |Value { value } is above maximum { max }| ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_paramters.
    parameters = int_parameters.
  ENDMETHOD.

  METHOD zif_llm_options~set_custom_parameters.
    LOOP AT parameters INTO DATA(par).
      READ TABLE int_parameters ASSIGNING FIELD-SYMBOL(<par>) WITH KEY key = par-key.
      IF sy-subrc = 0.
        <par>-value = par-value.
      ELSE.
        INSERT par INTO TABLE int_parameters.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_llm_options~set_frequency_penalty.
    validate_range_float(
      value = frequency_penalty
      min   = -2
      max   = 2
    ).
    set_parameter(
      key   = 'frequencyPenalty'
      value = |{ frequency_penalty }|
    ).
  ENDMETHOD.

  METHOD zif_llm_options~set_min_p.
    " does not exist, ignore
  ENDMETHOD.

  METHOD zif_llm_options~set_presence_penalty.
    validate_range_float(
      value = presence_penalty
      min   = -2
      max   = 2
    ).
    set_parameter(
      key   = 'presencePenalty'
      value = |{ presence_penalty }|
    ).
  ENDMETHOD.

  METHOD zif_llm_options~set_seed.
    validate_range_int(
      value = seed
      min   = 0
    ).
    set_parameter(
      key   = 'seed'
      value = |{ seed }|
    ).
  ENDMETHOD.

  METHOD zif_llm_options~set_temperature.
    validate_range_float(
      value = temperature
      min   = 0
      max   = 2
    ).
    set_parameter(
      key   = 'temperature'
      value = |{ temperature }|
    ).
  ENDMETHOD.

  METHOD zif_llm_options~set_top_a.
    " Does not exist, ignore
  ENDMETHOD.

  METHOD zif_llm_options~set_top_k.
    validate_range_int(
      value = top_k
      min   = 1
    ).
    set_parameter(
      key   = 'topK'
      value = |{ top_k }|
    ).
  ENDMETHOD.

  METHOD zif_llm_options~set_top_p.
    validate_range_float(
      value = top_p
      min   = 0
      max   = 1
    ).
    set_parameter(
      key   = 'topP'
      value = |{ top_p }|
    ).
  ENDMETHOD.

  METHOD zif_llm_options~set_max_tokens.
    validate_range_int(
       value = tokens
       min = 0 ).
    set_parameter(
      key = 'maxOutputTokens'
      value = |{ tokens }| ).
  ENDMETHOD.

ENDCLASS.
