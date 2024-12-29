INTERFACE zif_llm_so
  PUBLIC.
  TYPES: BEGIN OF def_description,
           fieldname   TYPE string,
           description TYPE string,
           enum_values TYPE string_table,
         END OF def_description.
  TYPES def_descriptions TYPE STANDARD TABLE OF def_description WITH KEY fieldname.

  METHODS set_schema
    IMPORTING
      data        TYPE any
      description TYPE def_descriptions OPTIONAL
    RAISING
      zcx_llm_validation.

  METHODS get_schema
    RETURNING VALUE(result) TYPE string.

  METHODS get_datatype EXPORTING data TYPE any.

ENDINTERFACE.
