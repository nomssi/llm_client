CLASS zcl_llm_tool_echo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_llm_tool.

    METHODS constructor IMPORTING tool_details TYPE zif_llm_tool=>tool_details.

    ALIASES get_tool_details FOR zif_llm_tool~get_tool_details.
    ALIASES execute          FOR zif_llm_tool~execute.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA tool_details  TYPE zif_llm_tool=>tool_details.
    DATA response_data TYPE REF TO data.
    DATA name          TYPE string.
    DATA tool_call_id  TYPE string.
ENDCLASS.


CLASS zcl_llm_tool_echo IMPLEMENTATION.
  METHOD zif_llm_tool~get_tool_details.
    result = tool_details.
  ENDMETHOD.

  METHOD zif_llm_tool~get_result.
    result-data = response_data.
    result-name = tool_details-name.
    result-tool_call_id = tool_call_id.
  ENDMETHOD.

  METHOD constructor.
    me->tool_details = tool_details.
  ENDMETHOD.

  METHOD zif_llm_tool~execute.
    response_data = data.
    me->tool_call_id = tool_call_id.
    result-data         = response_data.
    result-name         = tool_details-name.
    result-tool_call_id = tool_call_id.
  ENDMETHOD.

ENDCLASS.
