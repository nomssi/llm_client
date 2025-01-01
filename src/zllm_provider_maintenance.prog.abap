REPORT zllm_provider_maintenance.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF provider_config,
        provider_name   TYPE zllm_provider_name,
        provider_class  TYPE zllm_provider,
        rfc_destination TYPE rfcdest,
        auth_type       TYPE zllm_auth_type,
        auth_value      TYPE zllm_auth_value,
        auth_encrypted  TYPE xstring,
      END OF provider_config,
      provider_configs TYPE STANDARD TABLE OF provider_config WITH KEY provider_name.

    METHODS:
      constructor, "Initialize application
      display_providers, "Display the providers in ALV

      handle_action_add,    "Handle adding a new provider
      handle_action_change, "Handle updating a provider
      handle_action_delete. "Handle deleting a provider

  PRIVATE SECTION.
    DATA:
      providers TYPE provider_configs,
      grid      TYPE REF TO cl_gui_alv_grid,
      container TYPE REF TO cl_gui_custom_container.

    TYPES sval_tab TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

    METHODS:
      load_providers, "Load the provider list from the database
      save_provider IMPORTING config TYPE provider_config, "Save a provider to the database
      encrypt_auth_value IMPORTING plain TYPE zllm_auth_value
                         RETURNING VALUE(result) TYPE xstring, "Encrypt a value

      build_field_catalog RETURNING VALUE(fieldcat) TYPE lvc_t_fcat, "Build ALV field catalog
      refresh_display, "Refresh displayed ALV data
      show_popup IMPORTING title TYPE string
               CHANGING values TYPE sval_tab
                    RETURNING VALUE(config) TYPE provider_config, "Show user input popup
      show_confirm_popup IMPORTING title TYPE string text TYPE string
                         RETURNING VALUE(result) TYPE abap_bool. "Show confirm popup
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    load_providers( ).
  ENDMETHOD.

  METHOD load_providers.
    SELECT provider_name, provider_class, rfc_destination, auth_type, auth_value, auth_encrypted
      FROM zllm_providers
      INTO CORRESPONDING FIELDS OF TABLE @providers.
  ENDMETHOD.

  METHOD display_providers.
    IF grid IS NOT BOUND.
      grid = NEW #( i_parent = cl_gui_container=>default_screen ).

      " Build field catalog with reusable logic
      DATA(fieldcat) = build_field_catalog( ).

      " Configure ALV layout
      DATA(layout) = VALUE lvc_s_layo(
        sel_mode    = 'A'
        zebra       = abap_true
        col_opt     = abap_true
        cwidth_opt  = abap_true
        no_toolbar  = abap_true ).

      " Set ALV for display
      grid->set_table_for_first_display(
        EXPORTING
          is_layout        = layout
          i_save           = 'A'
          i_default        = 'X'
        CHANGING
          it_outtab        = providers
          it_fieldcatalog  = fieldcat ).
    ELSE.
      refresh_display( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_action_add.
    DATA(fields) = VALUE sval_tab(
      ( fieldname = 'PROVIDER_NAME' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Provider Name' field_attr = '01' field_obl = 'X' )
      ( fieldname = 'PROVIDER_CLASS' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Implementation Class' field_obl = 'X' )
      ( fieldname = 'RFC_DESTINATION' tabname = 'ZLLM_PROVIDERS' fieldtext = 'RFC Destination' )
      ( fieldname = 'AUTH_TYPE' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Auth Type' )
      ( fieldname = 'AUTH_VALUE' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Auth Value' ) ).

    DATA(new_config) = show_popup( EXPORTING title = 'Add Provider Configuration' CHANGING values = fields ).

    IF new_config IS NOT INITIAL.
      new_config-auth_encrypted = encrypt_auth_value( new_config-auth_value ).
      save_provider( new_config ).
      load_providers( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_action_change.
    grid->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
    CHECK lines( sel_rows ) = 1.

    DATA(selected_provider) = providers[ sel_rows[ 1 ]-index ].

    DATA(fields) = VALUE sval_tab(
      ( fieldname = 'PROVIDER_NAME' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Provider Name' field_attr = '02' field_obl = 'X' value = selected_provider-provider_name )
      ( fieldname = 'PROVIDER_CLASS' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Implementation Class' value = selected_provider-provider_class )
      ( fieldname = 'RFC_DESTINATION' tabname = 'ZLLM_PROVIDERS' fieldtext = 'RFC Destination' value = selected_provider-rfc_destination )
      ( fieldname = 'AUTH_TYPE' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Auth Type' value = selected_provider-auth_type )
      ( fieldname = 'AUTH_VALUE' tabname = 'ZLLM_PROVIDERS' fieldtext = 'Auth Value' value = selected_provider-auth_value ) ).

    DATA(updated_config) = show_popup( EXPORTING title = 'Change Provider Configuration' CHANGING values = fields ).

    IF updated_config IS NOT INITIAL.
      updated_config-auth_encrypted = encrypt_auth_value( updated_config-auth_value ).
      save_provider( updated_config ).
      load_providers( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_action_delete.
    grid->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
    CHECK lines( sel_rows ) = 1.

    DATA(selected_provider) = providers[ sel_rows[ 1 ]-index ].
    DATA(confirmed) = show_confirm_popup(
      title = 'Confirm Deletion'
      text  = |Delete provider { selected_provider-provider_name }?| ).

    IF confirmed = abap_true.
      DELETE FROM zllm_providers WHERE provider_name = @selected_provider-provider_name.
      IF sy-subrc = 0.
        load_providers( ).
        MESSAGE 'Provider deleted successfully' TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD save_provider.
    MODIFY zllm_providers FROM @( CORRESPONDING #( config ) ).
    IF sy-subrc = 0.
      MESSAGE 'Provider configuration saved successfully' TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD encrypt_auth_value.
    " Placeholder for value encryption logic
    result = ''.
  ENDMETHOD.

  METHOD build_field_catalog.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZLLM_PROVIDERS'
      CHANGING
        ct_fieldcat      = fieldcat.

    LOOP AT fieldcat ASSIGNING FIELD-SYMBOL(<fieldcat>).
      IF <fieldcat>-fieldname <> 'PROVIDER_NAME'.
        <fieldcat>-edit = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD refresh_display.
    grid->refresh_table_display( ).
  ENDMETHOD.

  METHOD show_popup.
    DATA(returncode) = ''.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = title
      IMPORTING
        returncode  = returncode
      TABLES
        fields      = values.

    IF returncode = 'A'.
      RETURN.
    ENDIF.

    DATA(resulting_config) = VALUE provider_config( ).
    LOOP AT values INTO DATA(field).
      ASSIGN COMPONENT field-fieldname OF STRUCTURE resulting_config TO FIELD-SYMBOL(<fs>).
      IF sy-subrc = 0.
        <fs> = field-value.
      ENDIF.
    ENDLOOP.
    config = resulting_config.
  ENDMETHOD.

  METHOD show_confirm_popup.
    DATA(answer) = ''.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar      = title
        text_question = text
        text_button_1 = 'Yes'
        text_button_2 = 'No'
      IMPORTING
        answer        = answer.

    result = xsdbool( answer = '1' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_screen DEFINITION.
  PUBLIC SECTION.
    METHODS:
      start,
      pai IMPORTING dynnr TYPE sy-dynnr
                    ucomm TYPE sy-ucomm,
      pbo IMPORTING dynnr TYPE sy-dynnr.

  PRIVATE SECTION.
    DATA app TYPE REF TO lcl_app.
ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.
  METHOD start.
    app = NEW #( ).
    CALL SCREEN 100.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
        LEAVE TO SCREEN 0.
      WHEN 'ADD'.
        app->handle_action_add( ).
      WHEN 'CHANGE'.
        app->handle_action_change( ).
      WHEN 'DELETE'.
        app->handle_action_delete( ).
      WHEN 'REFRESH'.
        app->display_providers( ).
    ENDCASE.
  ENDMETHOD.

  METHOD pbo.
    SET PF-STATUS 'MAIN100'.
    SET TITLEBAR 'TITLE100'.
    app->display_providers( ).
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA(screen) = NEW lcl_screen( ).

START-OF-SELECTION.
  screen->start( ).

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  screen->pbo( sy-dynnr ).
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  screen->pai( dynnr = sy-dynnr ucomm = sy-ucomm ).
ENDMODULE.
