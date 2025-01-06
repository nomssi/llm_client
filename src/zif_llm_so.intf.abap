"! <p class="shorttext synchronized" lang="en">Structured Output support</p>
INTERFACE zif_llm_so
  PUBLIC.
  TYPES: BEGIN OF def_description,
           fieldname   TYPE string,
           description TYPE string,
           enum_values TYPE string_table,
         END OF def_description.
  TYPES def_descriptions TYPE STANDARD TABLE OF def_description WITH KEY fieldname.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Set the schema for the structured output.
  "! @parameter data_desc | <p class="shorttext synchronized" lang="en">CL_ABAP_DATADESCR reference based on the datatype to be used</p>
  "! @parameter description | <p class="shorttext synchronized" lang="en">Field descriptions for more details</p>
  "! @raising zcx_llm_validation | <p class="shorttext synchronized" lang="en"></p>
  METHODS set_schema
    IMPORTING
      data_desc   TYPE REF TO cl_abap_datadescr
      description TYPE def_descriptions OPTIONAL
    RAISING
      zcx_llm_validation.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get the converted schema
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_schema
    RETURNING VALUE(result) TYPE string.

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Mostly used internally to get the data type definition
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_datatype RETURNING VALUE(result) TYPE REF TO cl_abap_datadescr.

ENDINTERFACE.
