class ZCX_LLM_AUTHORIZATION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of PROVIDER_MAINTENANCE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PROVIDER_MAINTENANCE .
  constants:
    begin of GET_CLIENT,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GET_CLIENT .
  constants:
    begin of DECRYPT,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '020',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DECRYPT .
  constants:
    begin of ENCRYPT,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '021',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ENCRYPT .
  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_LLM_AUTHORIZATION IMPLEMENTATION.


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
  ENDMETHOD.
ENDCLASS.
