class ZCX_LLM_TEMPLATE_PARSER definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF zcx_llm_template_parser,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_llm_template_parser .
  constants:
    BEGIN OF unclosed_token,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unclosed_token .
  constants:
    BEGIN OF unclosed_control_structure,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unclosed_control_structure .
  constants:
    BEGIN OF invalid_variable_path,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_variable_path .
  constants:
    BEGIN OF invalid_table_index,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_table_index .
  constants:
    BEGIN OF unsupported_variable_type,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unsupported_variable_type .
  constants:
    BEGIN OF variable_resolution_error,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF variable_resolution_error .
  constants:
    BEGIN OF condition_evaluation_error,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF condition_evaluation_error .
  constants:
    BEGIN OF unknown_filter,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unknown_filter .
  constants:
    BEGIN OF invalid_condition,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '031',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_condition .
  constants:
    BEGIN OF invalid_operator,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '032',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_operator .
  constants:
    BEGIN OF invalid_loop_syntax,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_loop_syntax .
  constants:
    BEGIN OF invalid_loop_collection,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '034',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_loop_collection .
  constants:
    BEGIN OF unclosed_loop,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '035',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unclosed_loop .
  constants:
    begin of INVALID_ESCAPE_SEQUENCE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '036',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_ESCAPE_SEQUENCE .
  constants:
    begin of MISMATCHED_TOKEN,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '037',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISMATCHED_TOKEN .
  constants:
    begin of LOOP_INITIALIZATION_ERROR,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '038',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LOOP_INITIALIZATION_ERROR .
  constants:
    begin of UNEXPECTED_ENDFOR,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '039',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNEXPECTED_ENDFOR .
  constants:
    begin of UNEXPECTED_ELSE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '040',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNEXPECTED_ELSE .
  constants:
    begin of UNEXPECTED_ENDIF,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '041',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNEXPECTED_ENDIF .
  constants:
    begin of UNEXPECTED_ELIF,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '042',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNEXPECTED_ELIF .
  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .
  data MSGV3 type SYMSGV .
  data MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_LLM_TEMPLATE_PARSER IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
  ENDMETHOD.
ENDCLASS.
