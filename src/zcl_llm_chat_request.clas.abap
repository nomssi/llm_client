CLASS zcl_llm_chat_request DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_llm_chat_request .
    "Data aliases
    ALIASES structured_output FOR zif_llm_chat_request~structured_output.
    ALIASES tools FOR zif_llm_chat_request~tools.
    ALIASES options FOR zif_llm_chat_request~options.
    ALIASES use_structured_output FOR zif_llm_chat_request~use_structured_output.
    ALIASES use_tools FOR zif_llm_chat_request~use_tools.

    "Method aliases
    ALIASES add_response_msg FOR zif_llm_chat_request~add_response_msg.
    ALIASES add_tool_result FOR zif_llm_chat_request~add_tool_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_chat_request IMPLEMENTATION.

  METHOD zif_llm_chat_request~add_response_msg.
    "TODO - Implement

  ENDMETHOD.

  METHOD zif_llm_chat_request~add_tool_result.
    "TODO - Implement
  ENDMETHOD.

ENDCLASS.
