"! <p class="shorttext synchronized" lang="en">LLM Factory</p>
CLASS zcl_llm_factory DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_llm_factory .
    ALIASES get_client FOR zif_llm_factory~get_client.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_factory IMPLEMENTATION.

  METHOD zif_llm_factory~get_client.
    SELECT SINGLE * INTO @DATA(client_configuration) FROM zllm_clnt_config WHERE model = @model.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>model_does_not_exist
          attr1  = CONV string( model ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(provider_configuration) FROM zllm_providers WHERE provider_name = @client_configuration-provider_name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>provider_does_not_exist
          attr1  = CONV string( client_configuration-provider_name ).
    ENDIF.

    CALL METHOD (provider_configuration-provider_class)=>('ZIF_LLM_CLIENT~GET_CLIENT')
      EXPORTING
        client_config   = client_configuration
        provider_config = provider_configuration
      RECEIVING
        response        = response.
  ENDMETHOD.

ENDCLASS.
