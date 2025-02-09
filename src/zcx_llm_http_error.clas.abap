class ZCX_LLM_HTTP_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF http_client_not_initialized,
        msgid TYPE symsgid VALUE 'ZLLM_CLIENT',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_ATTR1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF http_client_not_initialized .
  constants:
    begin of HTTP_COMMUNICATION_FAILURE,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HTTP_COMMUNICATION_FAILURE .
  constants:
    begin of HTTP_PROCESSING_FAILED,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HTTP_PROCESSING_FAILED .
  constants:
    begin of HTTP_OTHERS,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HTTP_OTHERS .
  constants:
    begin of HTTP_AUTH_PROCESSING,
      msgid type symsgid value 'ZLLM_CLIENT',
      msgno type symsgno value '043',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value 'MV_ATTR2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HTTP_AUTH_PROCESSING .
  data MV_ATTR1 type STRING .
  data MV_ATTR2 type STRING .
  data MV_ATTR3 type STRING .
  data MV_ATTR4 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !ATTR1 type STRING optional
      !ATTR2 type STRING optional
      !ATTR3 type STRING optional
      !ATTR4 type STRING optional
      !PREVIOUS like PREVIOUS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_LLM_HTTP_ERROR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).
    mv_attr1 = attr1.
    mv_attr2 = attr2.
    mv_attr3 = attr3.
    mv_attr4 = attr4.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
