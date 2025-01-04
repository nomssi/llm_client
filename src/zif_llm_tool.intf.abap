INTERFACE zif_llm_tool
  PUBLIC .
  CONSTANTS: type_function TYPE string VALUE `function`.

  TYPES: BEGIN OF tool_parameters,
           data         TYPE REF TO data,
           data_desc    TYPE REF TO cl_abap_datadescr,
           descriptions TYPE zif_llm_tool_parser=>def_descriptions,
         END OF tool_parameters.

  TYPES: BEGIN OF tool_details,
           name        TYPE string,
           description TYPE string,
           type        TYPE string,
           parameters  TYPE tool_parameters,
         END OF tool_details.

  types: begin of tool_result,
           data type ref to data,
           tool_call_id type string,
           name type string,
         end of tool_result.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get the result of the tool call
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_result RETURNING VALUE(result) TYPE tool_result.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Returns tool details required to parse it as tool for the LLM
  "! @parameter result | <p class="shorttext synchronized" lang="en">Tool details</p>
  METHODS get_tool_details RETURNING VALUE(result) TYPE tool_details.

ENDINTERFACE.
