"! <p class="shorttext synchronized" lang="en">Tool Parser</p>
INTERFACE zif_llm_tool_parser
  PUBLIC.
  TYPES: BEGIN OF def_description,
           fieldname   TYPE string,
           description TYPE string,
           enum_values TYPE string_table,
         END OF def_description.
  TYPES def_descriptions TYPE STANDARD TABLE OF def_description WITH KEY fieldname.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Parse the tool definition
  "! @parameter data_desc | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter descriptions | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
  METHODS parse
    IMPORTING
              data_desc type ref to cl_abap_datadescr
              descriptions  TYPE def_descriptions OPTIONAL
    RETURNING VALUE(result) TYPE string
    RAISING
              zcx_llm_validation.

ENDINTERFACE.
