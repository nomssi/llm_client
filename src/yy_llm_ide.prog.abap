*&---------------------------------------------------------------------*
*& Include          YY_LLM_IDE
*&---------------------------------------------------------------------*

CONSTANTS:
  c_llm_untitled    TYPE programm VALUE 'Untitled lLM Query',
  c_new_abap_editor TYPE flag VALUE abap_true,
  c_source_type     TYPE string VALUE 'LISP'.

DATA g_ok_code TYPE syucomm.

INTERFACE lif_unit_test.
ENDINTERFACE.

TYPES: BEGIN OF ts_settings,
         stack      TYPE string_table,
         new_editor TYPE flag,
       END OF ts_settings.

CLASS lcl_stack DEFINITION FRIENDS lif_unit_test.
  PUBLIC SECTION.
    TYPES tv_data TYPE string.

    METHODS constructor IMPORTING it_stack TYPE string_table OPTIONAL.
    METHODS previous RETURNING VALUE(rv_data) TYPE tv_data.
    METHODS next RETURNING VALUE(rv_data) TYPE tv_data.

    METHODS push IMPORTING iv_key TYPE tv_data.

    METHODS serialize RETURNING VALUE(rt_string) TYPE string_table.
    METHODS deserialize IMPORTING it_string      TYPE string_table.
  PROTECTED SECTION.
    DATA mt_stack TYPE string_table.
    DATA mv_index TYPE i.
ENDCLASS.

CLASS lcl_stack IMPLEMENTATION.

  METHOD constructor.
    mt_stack = it_stack.
  ENDMETHOD.

  METHOD push.
    CHECK iv_key IS NOT INITIAL.
    APPEND iv_key TO mt_stack.
    mv_index = lines( mt_stack ).
  ENDMETHOD.

  METHOD previous.
    IF mv_index GT 1.
      mv_index = mv_index - 1.
    ENDIF.
    rv_data = VALUE #( mt_stack[ mv_index ] OPTIONAL ).
  ENDMETHOD.

  METHOD next.
    IF mv_index LT lines( mt_stack ).
      mv_index = mv_index + 1.
    ENDIF.
    rv_data = VALUE #( mt_stack[ mv_index ] OPTIONAL ).
  ENDMETHOD.

  METHOD deserialize.
    mt_stack = it_string.
    mv_index = lines( mt_stack ).
  ENDMETHOD.

  METHOD serialize.
    rt_string = mt_stack.
  ENDMETHOD.

ENDCLASS.

INTERFACE lif_source_editor.
  METHODS clear.
  METHODS push_text.
  METHODS previous.
  METHODS next.
  METHODS to_string RETURNING VALUE(rv_text) TYPE string.
  METHODS update_status IMPORTING iv_string TYPE string.
  METHODS setup IMPORTING is_settings TYPE ts_settings.

  METHODS set_focus.
  METHODS free RETURNING VALUE(rt_string) TYPE string_table.
ENDINTERFACE.

*----------------------------------------------------------------------*
*       CLASS lcl_editor DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_editor DEFINITION INHERITING FROM cl_gui_textedit.
  PUBLIC SECTION.
    CONSTANTS c_max_line_count  TYPE i VALUE 10000.

    METHODS constructor IMPORTING io_container TYPE REF TO cl_gui_container
                                  iv_read_only TYPE flag DEFAULT abap_true
                                  iv_toolbar   TYPE flag DEFAULT abap_false.
    METHODS append_source IMPORTING iv_text TYPE string.

    INTERFACES lif_source_editor.

    METHODS append_string IMPORTING iv_text TYPE string.
  PROTECTED SECTION.
    DATA mv_counter TYPE i.
    DATA mo_stack TYPE REF TO lcl_stack.

    METHODS format_input IMPORTING query          TYPE string
                         RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_editor IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_editor IMPLEMENTATION.

  METHOD constructor.
    DATA mode TYPE i.
    io_container->set_visible( abap_true ).
    super->constructor( io_container ).
    IF iv_toolbar EQ abap_true.
      mode = 1.
    ELSE.
      mode = 0.
    ENDIF.
    set_toolbar_mode( mode ).
    cl_gui_cfw=>flush( ).

    IF iv_read_only EQ abap_true.
      set_readonly_mode( cl_gui_textedit=>true ).
      mode = 0.
    ELSE.
      mode = 1.
    ENDIF.
    set_statusbar_mode( mode ).
*   Work around to avoid NO DATA dump on first read
    lif_source_editor~clear( ).
    mo_stack = NEW #( ).
  ENDMETHOD.                    "constructor

  METHOD lif_source_editor~clear.
    delete_text( ).
  ENDMETHOD.                    "append_string

  METHOD append_string.
    DATA lv_text TYPE string.
    lv_text = lif_source_editor~to_string( ).
    CONCATENATE lv_text iv_text INTO lv_text RESPECTING BLANKS.
    set_textstream( lv_text ).
    go_to_line( c_max_line_count ).
  ENDMETHOD.

  METHOD format_input.
    ADD 1 TO mv_counter.
    rv_text = | ${ mv_counter }> { query }\n|.
  ENDMETHOD.                    "format_input

  METHOD append_source.
    append_string( format_input( iv_text ) ).
  ENDMETHOD.                    "append_string

  METHOD lif_source_editor~to_string.
    get_textstream( IMPORTING text = rv_text
                    EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc EQ 0.
    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "to_string

  METHOD lif_source_editor~update_status.
    DATA lv_text TYPE char72.
    lv_text = iv_string.
    set_status_text( lv_text ).
  ENDMETHOD.                    "update_status

  METHOD lif_source_editor~push_text.
    DATA code TYPE string.
    code = lif_source_editor~to_string( ).
    CHECK code NE space.
    mo_stack->push( code ).
    lif_source_editor~clear( ).
  ENDMETHOD.

  METHOD lif_source_editor~previous.
    lif_source_editor~clear( ).
    append_string( mo_stack->previous( ) ).
  ENDMETHOD.

  METHOD lif_source_editor~next.
    lif_source_editor~clear( ).
    append_string( mo_stack->next( ) ).
  ENDMETHOD.

  METHOD lif_source_editor~set_focus.
    set_focus( EXPORTING control = me
               EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~free.
    free( ).
    rt_string = mo_stack->serialize( ).
  ENDMETHOD.

  METHOD lif_source_editor~setup.
    mo_stack->deserialize( is_settings-stack ).
  ENDMETHOD.

ENDCLASS.                    "lcl_editor IMPLEMENTATION

CLASS lcl_console DEFINITION INHERITING FROM lcl_editor.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_container TYPE REF TO cl_gui_container
                                  iv_toolbar   TYPE flag DEFAULT abap_false.
    METHODS set_textstream REDEFINITION.
    METHODS lif_source_editor~to_string REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_content TYPE string.
ENDCLASS.


CLASS lcl_console IMPLEMENTATION.

  METHOD constructor.
    super->constructor( io_container = io_container
                        iv_read_only = abap_true
                        iv_toolbar = iv_toolbar ).
  ENDMETHOD.

  METHOD set_textstream.
    super->set_textstream( text ).
    mv_content = text.
  ENDMETHOD.

  METHOD lif_source_editor~to_string.
    rv_text = mv_content.
  ENDMETHOD.                    "to_string

ENDCLASS.

CLASS lcl_source DEFINITION INHERITING FROM cl_gui_sourceedit.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_container TYPE REF TO cl_gui_container
                                  iv_read_only TYPE flag DEFAULT abap_false
                                  iv_toolbar   TYPE flag DEFAULT abap_false
                                  iv_title     TYPE string.
    INTERFACES lif_source_editor.
  PRIVATE SECTION.
    DATA mo_stack TYPE REF TO lcl_stack.
ENDCLASS.

CLASS lcl_source IMPLEMENTATION.

  METHOD constructor.
    DATA mode TYPE i.
    DATA exception_name TYPE string.

    io_container->set_visible( abap_true ).
    super->constructor(
      EXPORTING
        parent = io_container
        max_number_chars = '255'
      EXCEPTIONS
        error_cntl_create      = 1
        error_dp_create        = 2
        gui_type_not_supported = 3
        error_cntl_init        = 4 ).

    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          exception_name = 'ERROR_CNTL_CREATE'.
        WHEN 2.
          exception_name = 'ERROR_DP_CREATE'.
        WHEN 3.
          exception_name = 'GUI_TYPE_NOT_SUPPORTED'.
        WHEN 4.
          exception_name = 'ERROR_CNTL_INIT'.
      ENDCASE.
      RAISE EXCEPTION TYPE cx_coverage_api_adapter.
*        EXPORTING
*          exception_name = exception_name.
    ENDIF.

    set_source_type( c_source_type ).
    IF iv_toolbar EQ abap_true.
      mode = 1.
    ELSE.
      mode = 0.
    ENDIF.
    set_toolbar_mode( mode ).
    cl_gui_cfw=>flush( ).

    IF iv_read_only EQ abap_true.
      set_readonly_mode( cl_gui_textedit=>true ).
      mode = 0.
    ELSE.
      mode = 1.
    ENDIF.
    set_statusbar_mode( mode ).
    set_actual_name( CONV syrepid( iv_title ) ).
    upload_properties( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
*      MESSAGE e215(ed).
    ENDIF.
    create_document( ).

*   Work around to avoid NO DATA dump on first read
    lif_source_editor~clear( ).

    CREATE OBJECT mo_stack.
  ENDMETHOD.                    "constructor

  METHOD lif_source_editor~set_focus.
    set_focus( EXPORTING control = me
               EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~free.
    free( ).
    rt_string = mo_stack->serialize( ).
  ENDMETHOD.

  METHOD lif_source_editor~setup.
    mo_stack->deserialize( is_settings-stack ).
  ENDMETHOD.

  METHOD lif_source_editor~to_string.
    DATA lt_text TYPE STANDARD TABLE OF string.

    get_text( IMPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
    "cl_gui_cfw=>flush( ).
    rv_text = concat_lines_of( table = lt_text sep = |\n| ).
  ENDMETHOD.                    "to_string

  METHOD lif_source_editor~update_status.
    MESSAGE iv_string TYPE 'S'.
  ENDMETHOD.                    "update_status

  METHOD lif_source_editor~clear.
    DATA lt_text TYPE STANDARD TABLE OF string.

    set_text( EXPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~push_text.
    DATA code TYPE string.
    code = lif_source_editor~to_string( ).
    CHECK code NE space.
    mo_stack->push( code ).
    lif_source_editor~clear( ).
  ENDMETHOD.

  METHOD lif_source_editor~previous.
    DATA lt_text TYPE STANDARD TABLE OF string.

    APPEND mo_stack->previous( ) TO lt_text.
    set_text( EXPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

  METHOD lif_source_editor~next.
    DATA lt_text TYPE STANDARD TABLE OF string.

    APPEND mo_stack->next( ) TO lt_text.
    set_text( EXPORTING table = lt_text
              EXCEPTIONS OTHERS = 0 ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_container DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_container DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS free_controls.
    DATA mo_input TYPE REF TO cl_gui_container READ-ONLY.
    DATA mo_output TYPE REF TO cl_gui_container READ-ONLY.
    DATA mo_console TYPE REF TO cl_gui_container READ-ONLY.
  PRIVATE SECTION.
    DATA mo_splitter_h TYPE REF TO cl_gui_splitter_container.
    DATA mo_splitter_v TYPE REF TO cl_gui_splitter_container.
    DATA mo_left TYPE REF TO cl_gui_container.
ENDCLASS.

CLASS lcl_container IMPLEMENTATION.
  METHOD constructor.
    " Splitter Container
    mo_splitter_v = NEW #( link_dynnr = '0100'
                           link_repid = sy-repid
                           parent     = cl_gui_container=>screen0
                           rows       = 1
                           columns    = 2 ).
    mo_splitter_v->set_border( border = cl_gui_cfw=>false ).

    mo_splitter_v->set_column_mode( mode = mo_splitter_v->mode_absolute ).
    mo_splitter_v->set_column_width( id    = 1
                                     width = 750 ).
    mo_left = mo_splitter_v->get_container( row    = 1
                                            column = 1 ).
    mo_console = mo_splitter_v->get_container( row    = 1
                                               column = 2 ).

    mo_splitter_h = NEW #( parent  = mo_left
                           rows    = 2
                           columns = 1 ).
    mo_splitter_h->set_border( border = cl_gui_cfw=>false ).
    mo_splitter_h->set_row_mode( mode = mo_splitter_v->mode_relative ).

    mo_input  = mo_splitter_h->get_container( row    = 1
                                              column = 1 ).
    mo_output = mo_splitter_h->get_container( row    = 2
                                              column = 1 ).

  ENDMETHOD.

  METHOD free_controls.
    FREE: mo_input,
          mo_output,
          mo_console.
    FREE mo_left.
    FREE mo_splitter_h.
    FREE mo_splitter_v.
  ENDMETHOD.                    "free_controls

ENDCLASS.                    "lcl_container IMPLEMENTATION

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    DATA mv_title TYPE string VALUE c_llm_untitled READ-ONLY.

    CLASS-METHODS:
      chat,
      free,
      default_model RETURNING VALUE(rd_model) TYPE zllm_clnt_config-provider_model,
      pbo,
      pai IMPORTING id_code        TYPE syucomm
          RETURNING VALUE(rd_flag) TYPE flag.

    METHODS constructor.
    METHODS first_output.
    METHODS free_controls.
    METHODS handle_user_command IMPORTING ucomm          TYPE sy-ucomm
                                RETURNING VALUE(rd_flag) TYPE flag.

  PRIVATE SECTION.
    CLASS-DATA app TYPE REF TO lcl_app.

    DATA mv_first TYPE flag VALUE abap_true.
    DATA mo_cont TYPE REF TO lcl_container.
    DATA mi_source TYPE REF TO lif_source_editor.
    DATA mo_output TYPE REF TO lcl_editor.
    DATA mo_console TYPE REF TO lcl_console.
    DATA ms_settings TYPE ts_settings.

    METHODS welcome RETURNING VALUE(text) TYPE string.
    METHODS console_header RETURNING VALUE(text) TYPE string.
    METHODS refresh.

    METHODS new_source_editor IMPORTING io_cont          TYPE REF TO cl_gui_container
                              RETURNING VALUE(ri_source) TYPE REF TO lif_source_editor.
    METHODS previous.
    METHODS next.
    METHODS evaluate.

    DATA runtime TYPE i.

    METHODS read_time RETURNING VALUE(rd_time) TYPE i.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  lcl_app=>pbo( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CHECK lcl_app=>pai( g_ok_code ).
  CLEAR g_ok_code.
ENDMODULE.

MODULE cancel_0100 INPUT.
  lcl_app=>free( ).
  LEAVE PROGRAM.
ENDMODULE.

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    CREATE OBJECT:
      mo_cont,

      mo_output
        EXPORTING
          io_container = mo_cont->mo_output
          iv_toolbar = abap_true,
      mo_console
        EXPORTING
          io_container = mo_cont->mo_console.

    mi_source = new_source_editor( io_cont = mo_cont->mo_input ).
    refresh( ).
  ENDMETHOD.

  METHOD read_time.
    GET RUN TIME FIELD rd_time.
  ENDMETHOD.

  METHOD first_output.
    CHECK mv_first EQ abap_true.
    CLEAR mv_first.
    mi_source->set_focus( ).
    mo_output->append_string( |{ welcome( ) }\n| ).
    mo_console->append_string( console_header( ) ).
  ENDMETHOD.

  METHOD free_controls.
    mo_console->free( ).
    mo_output->free( ).
    ms_settings-stack = mi_source->free( ).
    mo_cont->free_controls( ).
    "save_settings( ).
  ENDMETHOD.

  METHOD welcome.
    text = |==> Welcome to LLM Chat!\n|.
  ENDMETHOD.                    "welcome

  METHOD console_header.
    text = |==> LLM -- Console { sy-uname } -- { sy-datlo DATE = ENVIRONMENT } { sy-uzeit TIME = ENVIRONMENT }\n|.
  ENDMETHOD.

  METHOD new_source_editor.
    DATA gui_support TYPE boolean.
*   Check for frontend support for the new ABAP Editor
    cl_gui_frontend_services=>check_gui_support(
      EXPORTING
        component            = 'abapeditor'                 "#EC NOTEXT
        feature_name         = 'ab4'                        "#EC NOTEXT
      RECEIVING
        result               = gui_support
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        unknown_error        = 5
        OTHERS               = 6 ).
    IF sy-subrc NE 0 OR gui_support NE abap_true OR c_new_abap_editor NE abap_true.
      ri_source = NEW lcl_editor(
          io_container = io_cont
          iv_read_only = abap_false
          iv_toolbar   = abap_true ).

    ELSE.
      ri_source = NEW lcl_source(
          io_container = io_cont
          iv_read_only = abap_false
          iv_toolbar   = abap_true
          iv_title     = mv_title ).
    ENDIF.
    ri_source->setup( ms_settings ).

  ENDMETHOD.

  METHOD previous.
    mi_source->previous( ).
  ENDMETHOD.

  METHOD next.
    mi_source->next( ).
  ENDMETHOD.

  METHOD refresh.
    mi_source->clear( ).
    mo_output->delete_text( ).
    mo_console->delete_text( ).
  ENDMETHOD.                    "refresh

  METHOD pbo.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100' WITH app->mv_title.
    app->first_output( ).
  ENDMETHOD.

  METHOD pai.
    rd_flag = app->handle_user_command( id_code ).
  ENDMETHOD.

  METHOD free.
    app->free_controls( ).
  ENDMETHOD.


  METHOD chat.
    app = NEW #( ).
    CALL SCREEN 100.
  ENDMETHOD.

  METHOD default_model.
    SELECT provider_model FROM zllm_clnt_config UP TO 1 ROWS INTO @rd_Model.
    ENDSELECT.
  ENDMETHOD.

  METHOD handle_user_command.
    rd_flag = abap_false.

    CASE ucomm.
      WHEN 'EXECUTE'.
        evaluate( ). "         get_response( ).
      WHEN 'CLEAR'.
        refresh( ).
      WHEN 'PREV'.
        previous( ).
      WHEN 'NEXT'.
        next( ).

      WHEN 'SM59'.
        TRY.
            CALL TRANSACTION 'SM59' WITH AUTHORITY-CHECK.
          CATCH cx_sy_authorization_error INTO DATA(lx_error).
            MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
        ENDTRY.
      WHEN 'PROVIDERS'.
        TRY.
            CALL TRANSACTION 'ZLLM_PROVIDER_CONFIG' WITH AUTHORITY-CHECK.
          CATCH cx_sy_authorization_error INTO lx_error.
            MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
        ENDTRY.
      WHEN 'MODELS'.
        TRY.
            CALL TRANSACTION 'ZLLM_CLIENT_CONFIG' WITH AUTHORITY-CHECK.
          CATCH cx_sy_authorization_error INTO lx_error.
            MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
        ENDTRY.
      WHEN 'STATS'.
        TRY.
            CALL TRANSACTION 'ZLLM_SYSTEM_CONF' WITH AUTHORITY-CHECK.
          CATCH cx_sy_authorization_error INTO lx_error.
            MESSAGE lx_error TYPE 'I' DISPLAY LIKE 'E'.
        ENDTRY.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    rd_flag = abap_true.
  ENDMETHOD.

  METHOD evaluate.
    DATA query TYPE string.
    DATA response TYPE string.
    DATA output TYPE string.
    DATA lx_root TYPE REF TO cx_root.
    TRY.
        query = mi_source->to_string( ).

        IF query IS INITIAL.
          MESSAGE 'Please enter a message.'(003) TYPE 'I' DISPLAY LIKE 'W'.
          RETURN.
        ENDIF.

        DATA(ld_start_time) = read_time( ).

        DATA(client) = zcl_llm_factory=>get_client( model ).
        DATA(resp) = client->execute( user_message = query ).

        runtime = read_time( ) - ld_start_time.

        IF resp-error-error_text IS NOT INITIAL.
          mo_console->append_source( resp-error-error_text && |\n| ).
        ENDIF.

        response = resp-choice-message-content.

        mi_source->push_text( ).
        mi_source->update_status( |[ { runtime } µs ] { response }| ).

      CATCH zcx_llm_authorization.
        mo_console->append_source( 'Not authorized.'(005) && |\n| ).

      CATCH cx_root INTO lx_root.
        response = lx_root->get_text( ).
        mi_source->update_status( response ).
    ENDTRY.

    mo_output->append_source( |{ query }\n=> { response }\n| ).
  ENDMETHOD.

ENDCLASS.
