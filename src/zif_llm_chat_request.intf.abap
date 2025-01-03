"! <p class="shorttext synchronized" lang="en">Chat request</p>
INTERFACE zif_llm_chat_request
  PUBLIC.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add a message
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_message IMPORTING message TYPE zllm_msg.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add multiple messages
  "! @parameter messages | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_messages IMPORTING messages TYPE zllm_msgs.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get all message
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_messages RETURNING VALUE(result) TYPE zllm_msgs.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Delete all messages
  METHODS clear_messages.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add a tool
  "! @parameter tool | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter use_tools | <p class="shorttext synchronized" lang="en">Enable tool usage (default)</p>
  METHODS add_tool IMPORTING tool TYPE REF TO zif_llm_tool use_tools TYPE sap_bool DEFAULT abap_true.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add multiple tools
  "! @parameter tools | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter use_tools | <p class="shorttext synchronized" lang="en">Enable tool usage (default)</p>
  METHODS add_tools IMPORTING tools TYPE zllm_tools use_tools TYPE sap_bool DEFAULT abap_true.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get all tools
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_tools RETURNING VALUE(result) TYPE zllm_tools.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Delete all tools
  METHODS clear_tools.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Enable/Disable tool usage
  "! @parameter active | <p class="shorttext synchronized" lang="en"></p>
  METHODS set_tools_active IMPORTING active TYPE sap_bool.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add the tool result to the message
  "! @parameter tool | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_tool_result
    IMPORTING
      tool TYPE REF TO zif_llm_tool.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Set structured output details
  "! @parameter data | <p class="shorttext synchronized" lang="en">Data reference</p>
  "! @parameter descriptions | <p class="shorttext synchronized" lang="en">Descriptions for the LLM</p>
  "! @parameter use_structured_output | <p class="shorttext synchronized" lang="en">Enable structured output (default)</p>
  METHODS set_structured_output
    IMPORTING
      data                  TYPE any
      descriptions          TYPE zif_llm_so=>def_descriptions
      use_structured_output TYPE sap_bool DEFAULT abap_true.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Enable/Disable structured output usage
  "! @parameter active | <p class="shorttext synchronized" lang="en"></p>
  METHODS set_structured_output_active IMPORTING active TYPE sap_bool.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Append the LLM choice to the message list
  "! @parameter choice | <p class="shorttext synchronized" lang="en"></p>
  METHODS add_choice
    IMPORTING
      choice TYPE zllm_choice.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Used internally to read the full request details
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_internal_request RETURNING VALUE(result) TYPE zllm_request.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get options implementation, use it to set options
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS options RETURNING VALUE(result) TYPE REF TO zif_llm_options.


ENDINTERFACE.
