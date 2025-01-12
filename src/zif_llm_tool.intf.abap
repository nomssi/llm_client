"! <p class="shorttext synchronized" lang="en">Tool (Function) Call</p>
INTERFACE zif_llm_tool
  PUBLIC.
  CONSTANTS type_function TYPE string VALUE `function`.

  TYPES: BEGIN OF tool_parameters,
           data_desc    TYPE REF TO cl_abap_datadescr,
           descriptions TYPE zif_llm_tool_parser=>def_descriptions,
         END OF tool_parameters.

  TYPES: BEGIN OF tool_details,
           name        TYPE string,
           description TYPE string,
           type        TYPE string,
           parameters  TYPE tool_parameters,
         END OF tool_details.

  TYPES: BEGIN OF tool_result,
           data         TYPE REF TO data,
           tool_call_id TYPE string,
           name         TYPE string,
         END OF tool_result.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get the result of the tool call
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_result RETURNING VALUE(result) TYPE tool_result.

  "! <p class="shorttext synchronized"></p>
  "! Returns tool details required to parse it as tool for the LLM
  "! @parameter result | <p class="shorttext synchronized">Tool details</p>
  METHODS get_tool_details RETURNING VALUE(result) TYPE tool_details.


  "! <p class="shorttext synchronized"></p>
  "! Execute the tool.
  "! @parameter data         | <p class="shorttext synchronized">LLM Model Tool Call response</p>
  "! @parameter tool_call_id | <p class="shorttext synchronized">LLM Model Tool Call ID</p>
  "! @parameter result       | <p class="shorttext synchronized">Execution result</p>
  METHODS execute IMPORTING data         TYPE REF TO data
                            tool_call_id  TYPE string
                  RETURNING VALUE(result) TYPE tool_result.

ENDINTERFACE.
