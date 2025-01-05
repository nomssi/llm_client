"! <p class="shorttext synchronized" lang="en">Tool Parser</p>
INTERFACE zif_llm_tool_parser
  PUBLIC.
  TYPES: BEGIN OF def_description,
           fieldname   TYPE string,
           description TYPE string,
           enum_values TYPE string_table,
         END OF def_description.
  TYPES def_descriptions TYPE STANDARD TABLE OF def_description WITH KEY fieldname.

  METHODS parse
    IMPORTING
              data          TYPE any
              "data_desc type ref to cl_abap_datadescr
              descriptions  TYPE def_descriptions OPTIONAL
    RETURNING VALUE(result) TYPE string
    RAISING
              zcx_llm_validation.

ENDINTERFACE.
