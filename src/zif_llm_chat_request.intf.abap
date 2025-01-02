"! <p class="shorttext synchronized" lang="en">Chat request</p>
INTERFACE zif_llm_chat_request
  PUBLIC .


  DATA use_tools TYPE sap_bool .
  DATA use_structured_output TYPE sap_bool .
  DATA messages TYPE zllm_msgs .
  DATA structured_output TYPE REF TO zif_llm_so .
  DATA tools TYPE zllm_tools .
  DATA options TYPE REF TO zif_llm_options .

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add all choices from the response as assistant messages to the request
  "! @parameter response | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_response_msg
    IMPORTING
      !response TYPE REF TO zif_llm_chat_response .
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add the response from the tool as tool message to the request
  "! @parameter function | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_tool_result
    IMPORTING
      !function TYPE REF TO zif_llm_tool .
ENDINTERFACE.
