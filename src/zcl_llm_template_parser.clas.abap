CLASS zcl_llm_template_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF template_type,
        name    TYPE string,
        content TYPE string,
      END OF template_type,
      templates_type TYPE SORTED TABLE OF template_type WITH UNIQUE KEY name.

    TYPES:
      BEGIN OF token_type,
        type    TYPE string,
        content TYPE string,
      END OF token_type,
      tokens_type TYPE STANDARD TABLE OF token_type WITH EMPTY KEY.

    METHODS constructor.

    METHODS add_template
      IMPORTING !name   TYPE string
                content TYPE string
      RAISING   zcx_llm_template_parser.

    METHODS render
      IMPORTING template_name TYPE string
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF token_types,
        text     TYPE string VALUE 'TEXT',
        variable TYPE string VALUE 'VARIABLE',
        control  TYPE string VALUE 'CONTROL',
        comment  TYPE string VALUE 'COMMENT',
        include  TYPE string VALUE 'INCLUDE',
      END OF token_types.

    TYPES:
      BEGIN OF loop_meta_type,
        index TYPE i,
        first TYPE abap_bool,
        last  TYPE abap_bool,
      END OF loop_meta_type.

    TYPES:
      BEGIN OF control_stack_type,
        type              TYPE string,
        condition_met     TYPE abap_bool,
        any_condition_met TYPE abap_bool,
        tokens            TYPE tokens_type,
        loop_var          TYPE string,
        collection        TYPE REF TO data,
        loop_index        TYPE i,
        total_items       TYPE i,
        loop_tokens       TYPE tokens_type,
      END OF control_stack_type.

    TYPES control_stack_types TYPE STANDARD TABLE OF control_stack_type.

    DATA templates TYPE templates_type.

    METHODS tokenize
      IMPORTING template      TYPE string
      RETURNING VALUE(tokens) TYPE tokens_type
      RAISING   zcx_llm_template_parser.

    METHODS parse_tokens
      IMPORTING !tokens       TYPE tokens_type
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    METHODS resolve_variable
      IMPORTING variable_path TYPE string
                !context      TYPE REF TO data
                control_stack TYPE control_stack_types OPTIONAL
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    METHODS get_template_by_name
      IMPORTING !name          TYPE string
      RETURNING VALUE(content) TYPE string
      RAISING   zcx_llm_template_parser.

    METHODS evaluate_condition_true
      IMPORTING !condition    TYPE string
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_llm_template_parser.

    METHODS apply_filter
      IMPORTING !value        TYPE string
                !filter       TYPE string
                param         TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    METHODS resolve_variable_ref
      IMPORTING variable_path TYPE string
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE REF TO data
      RAISING   zcx_llm_template_parser.

    METHODS process_loop_content
      IMPORTING !tokens       TYPE tokens_type
                !context      TYPE REF TO data
                loop_var      TYPE string
                !collection   TYPE REF TO data
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    METHODS format_table
      IMPORTING !table        TYPE ANY TABLE
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.
ENDCLASS.

CLASS zcl_llm_template_parser IMPLEMENTATION.
  METHOD constructor.
    templates = VALUE #( ).
  ENDMETHOD.

  METHOD add_template.
    " Check if we already have this template, if yes replace it
    ASSIGN templates[ name = name ] TO FIELD-SYMBOL(<template>).
    IF sy-subrc = 0.
      <template>-content = content.
    ELSE.
      INSERT VALUE #( name    = name
                      content = content ) INTO TABLE templates.
    ENDIF.
  ENDMETHOD.

  METHOD render.
    DATA(template_content) = get_template_by_name( template_name ).
    DATA(tokens) = tokenize( template_content ).
    result = parse_tokens(
      tokens  = tokens
      context = context ).
  ENDMETHOD.

  METHOD tokenize.
    TYPES: BEGIN OF token_stack_type,
             type    TYPE string,
             start   TYPE i,
             content TYPE string,
           END OF token_stack_type.

    DATA current_pos   TYPE i VALUE 0.
    DATA current_char  TYPE string.
    DATA token_buffer  TYPE string.
    DATA escape_active TYPE abap_bool.
    DATA token_stack   TYPE STANDARD TABLE OF token_stack_type.

    WHILE current_pos < strlen( template ).
      current_char = template+current_pos(1).

      " Handle escape sequences
      IF current_char = '\' AND escape_active = abap_false.
        escape_active = abap_true.
        current_pos = current_pos + 1.
        CONTINUE.
      ENDIF.

      IF escape_active = abap_true.
        " Handle escape sequences
        CASE current_char.
          WHEN 'n'.  " Newline
            token_buffer = token_buffer && cl_abap_char_utilities=>newline.
          WHEN 't'.  " Tab
            token_buffer = token_buffer && cl_abap_char_utilities=>horizontal_tab.
          WHEN '{' OR '}' OR '\'.  " Escaped special characters
            token_buffer = token_buffer && current_char.
          WHEN OTHERS.
            token_buffer = |{ token_buffer }\\{ current_char }|.
        ENDCASE.
        escape_active = abap_false.
      ELSE.
        CASE current_char.
          WHEN '{'.
            " Check for start of special token
            IF current_pos + 1 < strlen( template ).
              DATA(next_char) = template+current_pos(2).
              DATA(token_type) = COND #(
                WHEN next_char = '{{' THEN token_types-variable
                WHEN next_char = '{%' THEN token_types-control
                WHEN next_char = '{#' THEN token_types-comment
                ELSE                       space ).

              IF token_type IS NOT INITIAL.
                " Add previous text token if buffer not empty
                IF token_buffer IS NOT INITIAL.
                  APPEND VALUE #( type    = token_types-text
                                  content = token_buffer )
                         TO tokens.
                  CLEAR token_buffer.
                ENDIF.

                " Push token to stack
                APPEND VALUE #( type  = token_type
                                start = current_pos + 2 )
                       TO token_stack.

                current_pos = current_pos + 1.
              ELSE.
                token_buffer = token_buffer && current_char.
              ENDIF.
            ENDIF.

          WHEN '}'.
            IF current_pos >= 1 AND current_pos < strlen( template ).
              " Check the current '}' together with the previous character
              DATA(prev_pos) = current_pos - 1.
              next_char = template+prev_pos(2).
              DATA(is_token_end) = xsdbool(
                   next_char = '}}'
                OR next_char = '%}'
                OR next_char = '#}' ).

              IF is_token_end = abap_true AND lines( token_stack ) > 0.
                " Get last token from stack
                DATA(last_token) = token_stack[ lines( token_stack ) ].
                DELETE token_stack INDEX lines( token_stack ).

                " Get token content - subtract 1 to exclude the first closing character
                DATA(token_length) = current_pos - 1 - last_token-start.
                DATA(token_content) = template+last_token-start(token_length).

                " Validate token end matches start
                DATA(expected_end) = SWITCH #( last_token-type
                                               WHEN token_types-variable THEN '}}'
                                               WHEN token_types-control  THEN '%}'
                                               WHEN token_types-comment  THEN '#}' ).
                IF next_char <> expected_end.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>mismatched_token
                                                               msgv1  = CONV #( expected_end )
                                                               msgv2  = CONV #( next_char ) ).
                ENDIF.

                CLEAR token_buffer.

                IF token_type = token_types-control.
                  CONDENSE token_content.
                ENDIF.

                " Add token
                APPEND VALUE #( type    = last_token-type
                                content = token_content )
                       TO tokens.

              ELSE.
                token_buffer = token_buffer && current_char.
              ENDIF.
            ENDIF.

          WHEN OTHERS.
            IF lines( token_stack ) = 0.
              " In order to append a space to the token buffer we must use a string
              IF current_char = space.
                token_buffer = |{ token_buffer } |.
              ELSE.
                token_buffer = token_buffer && current_char.
              ENDIF.
            ELSE.
              " We're inside a token, collect the character
              DATA(current_token) = token_stack[ lines( token_stack ) ].
              IF current_token-type = token_types-variable
                 OR current_token-type = token_types-control
                 OR current_token-type = token_types-comment.
                token_buffer = token_buffer && current_char.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDIF.

      current_pos = current_pos + 1.
    ENDWHILE.

    " Add remaining text token if exists
    IF token_buffer IS NOT INITIAL.
      APPEND VALUE #( type    = token_types-text
                      content = token_buffer )
             TO tokens.
    ENDIF.

    " Check for unclosed tokens
    IF lines( token_stack ) > 0.
      DATA(unclosed_token) = token_stack[ 1 ].
      RAISE EXCEPTION NEW zcx_llm_template_parser(
          textid = zcx_llm_template_parser=>unclosed_token
          msgv1  = |{ unclosed_token-type } { unclosed_token-start }| ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_tokens.
    DATA control_stack     TYPE control_stack_types.
    DATA output_buffer     TYPE string.
    DATA for_nesting_level TYPE i VALUE 0.

    FIELD-SYMBOLS <current_stack> TYPE control_stack_type.

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>).
      " During a for loop we collect all tokens for loop processing later
      ASSIGN control_stack[ lines( control_stack ) ] TO <current_stack>.
      IF sy-subrc = 0 AND <current_stack>-type = 'FOR'.
        " Check if this is another nested for loop
        IF <token>-type = token_types-control AND condense( <token>-content ) CP 'for *'.
          for_nesting_level = for_nesting_level + 1.
        ENDIF.

        " Check if this is an endfor
        IF <token>-type = token_types-control AND condense( <token>-content ) = 'endfor'.
          IF for_nesting_level > 0.
            for_nesting_level = for_nesting_level - 1.
            APPEND <token> TO <current_stack>-loop_tokens.
            CONTINUE.
          ENDIF.
        ELSE.
          " If we're in a nested loop or not at the endfor, collect tokens
          APPEND <token> TO <current_stack>-loop_tokens.
          CONTINUE.  " Skip normal processing for these tokens
        ENDIF.
      ENDIF.

      " Handle different token types
      CASE <token>-type.
        WHEN token_types-control.
          DATA(control_content) = condense( <token>-content ).

          " Handle control structures
          IF control_content CP 'if *'.
            " Start new if block
            APPEND INITIAL LINE TO control_stack ASSIGNING <current_stack>.
            <current_stack>-type              = 'IF'.
            <current_stack>-any_condition_met = abap_false.

            TRY.
                DATA(condition) = substring_after( val = control_content
                                                   sub = 'if ' ).
                <current_stack>-condition_met = evaluate_condition_true( condition = condition
                                                                    context   = context ).
                IF <current_stack>-condition_met = abap_true.
                  <current_stack>-any_condition_met = abap_true.
                ENDIF.
              CATCH cx_root INTO DATA(lx_if).
                RAISE EXCEPTION NEW zcx_llm_template_parser(
                                        textid   = zcx_llm_template_parser=>condition_evaluation_error
                                        msgv1    = CONV #( condition )
                                        previous = lx_if ).
            ENDTRY.

          ELSEIF control_content CP 'elif *' ##NO_TEXT.
            " Handle elif (else if)
            IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'IF'.
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_elif ).
            ENDIF.

            ASSIGN control_stack[ lines( control_stack ) ] TO <current_stack>.
            " Only evaluate elif if no previous condition was met
            IF <current_stack>-any_condition_met = abap_false.
              TRY.
                  condition = substring_after( val = control_content
                                               sub = 'elif ' ).
                  <current_stack>-condition_met = evaluate_condition_true( condition = condition
                                                                      context   = context ).
                  IF <current_stack>-condition_met = abap_true.
                    <current_stack>-any_condition_met = abap_true.
                  ENDIF.
                CATCH cx_root INTO DATA(lx_elif).
                  RAISE EXCEPTION NEW zcx_llm_template_parser(
                                          textid   = zcx_llm_template_parser=>condition_evaluation_error
                                          msgv1    = CONV #( condition )
                                          previous = lx_elif ).
              ENDTRY.
            ELSE.
              <current_stack>-condition_met = abap_false.
            ENDIF.

          ELSEIF control_content = 'else'.
            " Handle else
            IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'IF'.
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_else ).
            ENDIF.

            ASSIGN control_stack[ lines( control_stack ) ] TO <current_stack>.
            " Only execute else if no previous condition was met
            <current_stack>-condition_met = xsdbool( <current_stack>-any_condition_met = abap_false ).

          ELSEIF control_content = 'endif'.
            " End if block
            IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'IF'.
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_endif ).
            ENDIF.

            DELETE control_stack INDEX lines( control_stack ).

          ELSEIF control_content CP 'for *' ##NO_TEXT.
            " Start new for loop
            APPEND INITIAL LINE TO control_stack ASSIGNING <current_stack>.
            <current_stack>-type        = 'FOR'.
            <current_stack>-loop_tokens = VALUE #( ).

            TRY.
                " Parse for loop syntax: "for item in collection"
                DATA(loop_content) = condense( control_content ).
                SPLIT loop_content AT ' ' INTO TABLE DATA(loop_parts).
                IF lines( loop_parts ) <> 4 OR loop_parts[ 3 ] <> 'in'.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_loop_syntax ).
                ENDIF.

                <current_stack>-loop_var = loop_parts[ 2 ].
                DATA(collection_path) = loop_parts[ 4 ].

                " Clean up collection path
                collection_path = replace( val  = collection_path
                                           sub  = '{{'
                                           with = '' ).
                collection_path = replace( val  = collection_path
                                           sub  = '}}'
                                           with = '' ).
                collection_path = condense( collection_path ).

                <current_stack>-collection = resolve_variable_ref( variable_path = collection_path
                                                                   context       = context ).

              CATCH cx_root INTO DATA(lx_for).
                RAISE EXCEPTION NEW zcx_llm_template_parser(
                                        textid   = zcx_llm_template_parser=>loop_initialization_error
                                        previous = lx_for ).
            ENDTRY.

          ELSEIF control_content = 'endfor'.
            " Process for loop content
            IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'FOR'.
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_endfor ).
            ENDIF.

            DATA(loop_stack) = control_stack[ lines( control_stack ) ].
            output_buffer = output_buffer && process_loop_content( tokens     = loop_stack-loop_tokens
                                                                   context    = context
                                                                   loop_var   = loop_stack-loop_var
                                                                   collection = loop_stack-collection ).

            DELETE control_stack INDEX lines( control_stack ).

          ENDIF.

        WHEN OTHERS.
          " Normal token processing for non-loop content
          IF <token>-type = token_types-text.
            " Check all conditions in the stack are met
            DATA(conditions_met) = abap_true.
            LOOP AT control_stack ASSIGNING FIELD-SYMBOL(<stack_entry>).
              IF <stack_entry>-condition_met = abap_false.
                conditions_met = abap_false.
                EXIT.
              ENDIF.
            ENDLOOP.

            IF lines( control_stack ) = 0 OR conditions_met = abap_true.
              output_buffer = output_buffer && <token>-content.
            ENDIF.

          ELSEIF <token>-type = token_types-variable.
            " Check all conditions in the stack are met
            conditions_met = abap_true.
            LOOP AT control_stack ASSIGNING <stack_entry>.
              IF <stack_entry>-condition_met = abap_false.
                conditions_met = abap_false.
                EXIT.
              ENDIF.
            ENDLOOP.

            IF lines( control_stack ) = 0 OR conditions_met = abap_true.
              TRY.
                  output_buffer = output_buffer && resolve_variable( variable_path = <token>-content
                                                                     context       = context
                                                                     control_stack = control_stack ).
                CATCH zcx_llm_template_parser INTO DATA(lx_var).
                  RAISE EXCEPTION NEW zcx_llm_template_parser(
                                          textid   = zcx_llm_template_parser=>variable_resolution_error
                                          msgv1    = CONV #( <token>-content )
                                          previous = lx_var ).
              ENDTRY.
            ENDIF.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    " Check for unclosed control structures
    IF lines( control_stack ) > 0.
      DATA(unclosed_control) = control_stack[ 1 ].
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unclosed_token
                                                   msgv1  = CONV #( unclosed_control-type ) ).
    ENDIF.

    result = output_buffer.
  ENDMETHOD.

  METHOD resolve_variable.
    DATA path_segments TYPE string_table.
    DATA current_ref   TYPE REF TO data.
    DATA filter_name   TYPE string.
    DATA filter_param  TYPE string.

    " Split variable path and filter if present
    SPLIT variable_path AT '|' INTO TABLE DATA(parts).
    DATA(var_path) = condense( parts[ 1 ] ).

    IF lines( parts ) > 1.
      DATA(filter_part) = condense( parts[ 2 ] ).
      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\((.*)\))?'
           IN filter_part
           SUBMATCHES filter_name filter_param.
    ENDIF.

    " Split path into segments
    SPLIT var_path AT '.' INTO TABLE path_segments.

    " Check if first segment is current loop variable
    IF     lines( control_stack ) > 0
       AND control_stack[ lines( control_stack ) ]-type = 'FOR'
       AND path_segments[ 1 ] = control_stack[ lines( control_stack ) ]-loop_var.
      " We're accessing a loop variable property
      " Remove the loop variable name from path segments
      DELETE path_segments INDEX 1.

      " Get current item from the loop
      DATA loop_ref TYPE REF TO data.
      loop_ref = control_stack[ lines( control_stack ) ]-collection.
      FIELD-SYMBOLS <loop_table> TYPE STANDARD TABLE.
      ASSIGN loop_ref->* TO <loop_table>.

      ASSIGN <loop_table>[ control_stack[ lines( control_stack ) ]-loop_index ] TO FIELD-SYMBOL(<current_item>).

      " Create reference to current item
      CREATE DATA current_ref LIKE <current_item>.
      ASSIGN current_ref->* TO FIELD-SYMBOL(<new_value>).
      <new_value> = <current_item>.
    ELSE.
      " Start with the context
      current_ref = context.
    ENDIF.

    " Navigate through the path
    LOOP AT path_segments INTO DATA(segment).
      " Handle array access notation (e.g., table[1])
      DATA component TYPE string.
      DATA idx       TYPE string.

      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\[(\d+)\])?'
           IN segment
           SUBMATCHES component idx.

      TRY.
          DATA(descr) = cl_abap_typedescr=>describe_by_data_ref( current_ref ).

          CASE descr->kind.
            WHEN cl_abap_typedescr=>kind_struct.
              DATA(struct_ref) = CAST cl_abap_structdescr( descr ).
              " TODO: variable is assigned but never used (ABAP cleaner)
              DATA(components) = struct_ref->get_components( ).

              FIELD-SYMBOLS <struct> TYPE any.
              ASSIGN current_ref->* TO <struct>.

              ASSIGN COMPONENT component OF STRUCTURE <struct> TO FIELD-SYMBOL(<component>).
              IF sy-subrc <> 0.
                RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path
                                                             msgv1  = CONV #( component ) ).
              ENDIF.

              CREATE DATA current_ref LIKE <component>.
              ASSIGN current_ref->* TO <new_value>.
              <new_value> = <component>.

              " If we have an array index, process it immediately after getting the component
              IF idx IS NOT INITIAL.
                DATA(table_idx) = CONV i( idx ).
                FIELD-SYMBOLS <idx_table> TYPE STANDARD TABLE.
                ASSIGN current_ref->* TO <idx_table>.

                IF table_idx <= 0 OR table_idx > lines( <idx_table> ).
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                ASSIGN <idx_table>[ table_idx ] TO FIELD-SYMBOL(<table_entry>).
                IF sy-subrc <> 0.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                CREATE DATA current_ref LIKE <table_entry>.
                ASSIGN current_ref->* TO <new_value>.
                <new_value> = <table_entry>.
              ENDIF.

            WHEN cl_abap_typedescr=>kind_table.
              " TODO: variable is assigned but never used (ABAP cleaner)
              DATA(table_ref) = CAST cl_abap_tabledescr( descr ).

              FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
              ASSIGN current_ref->* TO <table>.

              IF idx IS NOT INITIAL.
                table_idx = CONV i( idx ).

                IF table_idx <= 0 OR table_idx > lines( <table> ).
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                ASSIGN <table>[ table_idx ] TO <table_entry>.
                IF sy-subrc <> 0.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                CREATE DATA current_ref LIKE <table_entry>.
                ASSIGN current_ref->* TO <new_value>.
                <new_value> = <table_entry>.
              ELSE.
                " If this is the final segment and no index specified, format table
                IF sy-tabix = lines( path_segments ).
                  result = format_table( <table> ).
                  RETURN.
                ELSE.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path
                                                               msgv1  = 'Cannot access table without index in path' ) ##NO_TEXT.
                ENDIF.
              ENDIF.

            WHEN OTHERS.
              RAISE EXCEPTION NEW zcx_llm_template_parser(
                                      textid = zcx_llm_template_parser=>unsupported_variable_type ).
          ENDCASE.

        CATCH cx_root INTO DATA(ex).
          RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                       previous = ex ).
      ENDTRY.
    ENDLOOP.

    " Convert final value to string
    ASSIGN current_ref->* TO FIELD-SYMBOL(<final_value>).
    IF <final_value> IS ASSIGNED.
      DATA(final_descr) = cl_abap_typedescr=>describe_by_data( <final_value> ).

      CASE final_descr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          FIELD-SYMBOLS <final_table> TYPE ANY TABLE.
          ASSIGN <final_value> TO <final_table>.
          result = format_table( <final_table> ).

        WHEN OTHERS.
          TRY.
              CASE final_descr->type_kind.
                WHEN cl_abap_typedescr=>typekind_char.
                  IF    final_descr->absolute_name = '\TYPE=ABAP_BOOL'
                     OR (     final_descr->length = 1
                          AND ( <final_value> = abap_true OR <final_value> = abap_false ) ).
                    result = COND #( WHEN <final_value> = 'X'
                                     THEN 'true'
                                     ELSE 'false' ).
                  ELSE.
                    result = |{ <final_value> }|.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_date.
                  DATA(date) = CONV d( <final_value> ).
                  result = |{ date DATE = USER }|.
                WHEN cl_abap_typedescr=>typekind_time.
                  DATA(time) = CONV t( <final_value> ).
                  result = |{ time TIME = USER }|.
                WHEN OTHERS.
                  result = |{ <final_value> }|.
              ENDCASE.
            CATCH cx_sy_conversion_error INTO DATA(lx_conv).
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                           previous = lx_conv ).
          ENDTRY.
      ENDCASE.
    ELSE.
      result = ''.
    ENDIF.

    " Apply filter if present
    IF filter_name IS NOT INITIAL.
      result = apply_filter( value  = result
                             filter = filter_name
                             param  = filter_param ).
    ENDIF.
  ENDMETHOD.

  METHOD format_table.
    DATA result_table TYPE string_table.

    DATA(line_type) = CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( table ) )->get_table_line_type( ).

    CASE line_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        " For elementary types (string, integer etc), join with comma
        LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
          DATA(elem_str) = |{ <line> }|.  " Convert to string first
          APPEND elem_str TO result_table.
        ENDLOOP.
        CONCATENATE LINES OF result_table INTO result SEPARATED BY `, `.

      WHEN cl_abap_typedescr=>kind_struct.
        " For structures, create a list of key-value pairs
        LOOP AT table ASSIGNING <line>.
          DATA(struct_descr) = CAST cl_abap_structdescr( line_type ).
          DATA(components) = struct_descr->get_components( ).
          DATA line_result TYPE string.

          LOOP AT components INTO DATA(component).
            ASSIGN COMPONENT component-name OF STRUCTURE <line> TO FIELD-SYMBOL(<field>).
            IF sy-subrc = 0.
              DATA field_str TYPE string.

              " Handle different component types
              CASE component-type->kind.
                WHEN cl_abap_typedescr=>kind_table.
                  field_str = '[NESTED TABLE]'.
                WHEN cl_abap_typedescr=>kind_elem.
                  IF component-type->absolute_name = '\TYPE=ABAP_BOOL'.
                    field_str = COND #( WHEN <field> = abap_true THEN 'X' ELSE ` ` ).
                  ELSE.
                    field_str = |{ <field> }|.
                  ENDIF.
                WHEN OTHERS.
                  field_str = '[COMPLEX TYPE]'.
              ENDCASE.

              line_result = |{ line_result }{ to_upper( component-name ) }: { field_str }, |.
            ENDIF.
          ENDLOOP.

          " Remove trailing comma and space
          IF strlen( line_result ) >= 2.
            line_result = substring( val = line_result
                                   len = strlen( line_result ) - 2 ).
          ENDIF.

          APPEND line_result TO result_table.
        ENDLOOP.

        DATA(joined_results) = concat_lines_of( table = result_table
                                              sep   = `; ` ).
        result = |[{ joined_results }]|.

      WHEN cl_abap_typedescr=>kind_table.
        " For nested tables, return placeholder
        result = '[NESTED TABLE]'.

      WHEN OTHERS.
        result = '[COMPLEX TABLE]'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_template_by_name.
    TRY.
        content = templates[ name = name ]-content.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_llm_template_parser
          EXPORTING
            textid = zcx_llm_template_parser=>zcx_llm_template_parser.
    ENDTRY.
  ENDMETHOD.

  METHOD apply_filter.
    CASE to_upper( filter ).
      WHEN 'UPPER'.
        result = to_upper( value ).

      WHEN 'LOWER'.
        result = to_lower( value ).

      WHEN 'CAPITALIZE'.
        IF strlen( value ) > 0.
          result = to_upper( value(1) ) && to_lower( substring( val = value
                                                                off = 1 ) ).
        ENDIF.

      WHEN 'DEFAULT'.
        IF value IS INITIAL AND param IS NOT INITIAL.
          " Remove quotes if present
          result = COND #( WHEN param CS '"' OR param CS ''''
                           THEN substring( val = param
                                           off = 1
                                           len = strlen( param ) - 2 )
                           ELSE param ).
        ELSE.
          result = value.
        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unknown_filter ).
    ENDCASE.
  ENDMETHOD.

  METHOD evaluate_condition_true.
    " Split condition into parts
    DATA operator     TYPE string.
    DATA left_value   TYPE string.
    DATA right_value  TYPE string.
    DATA left_result  TYPE string.
    DATA right_result TYPE string.

    " Handle NOT operator
    DATA(condition_text) = condense( condition ).
    DATA(is_negated) = xsdbool( strlen( condition_text ) >= 4 AND substring( val = condition_text
                                                                             len = 4 ) = `not ` ).
    IF is_negated = abap_true.
      condition_text = condense( substring( val = condition_text
                                            off = 4 ) ).
    ENDIF.

    " Check for logical operators
    IF condition_text CS ' and '.
      SPLIT condition_text AT ' and ' INTO TABLE DATA(and_conditions).
      result = abap_true.
      LOOP AT and_conditions INTO DATA(sub_condition).
        sub_condition = condense( sub_condition ).
        IF evaluate_condition_true( condition = sub_condition
                               context   = context ) = abap_false.
          result = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF is_negated = abap_true.
        result = xsdbool( result = abap_false ).
      ENDIF.
      RETURN.
    ENDIF.

    IF condition_text CS ' or '.
      SPLIT condition_text AT ' or ' INTO TABLE DATA(or_conditions).
      result = abap_false.
      LOOP AT or_conditions INTO DATA(or_sub_condition).
        or_sub_condition = condense( or_sub_condition ).
        IF evaluate_condition_true( condition = or_sub_condition
                               context   = context ) = abap_true.
          result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF is_negated = abap_true.
        result = xsdbool( result = abap_false ).
      ENDIF.
      RETURN.
    ENDIF.

    " Handle comparison operators
    FIND FIRST OCCURRENCE OF REGEX '([^<>=!]+)\s*(==|!=|>=|<=|>|<)\s*([^<>=!]+)'
         IN condition_text
         SUBMATCHES left_value operator right_value.

    IF sy-subrc = 0.
      " Clean up values
      left_value = condense( left_value ).
      right_value = condense( right_value ).

      " Handle left value
      TRY.
          IF left_value CS '"' OR left_value CS ''''.
            left_result = substring( val = left_value
                                     off = 1
                                     len = strlen( left_value ) - 2 ).
          ELSE.
            left_result = resolve_variable( variable_path = left_value
                                            context       = context ).
          ENDIF.
        CATCH zcx_llm_template_parser.
          left_result = left_value.
      ENDTRY.

      " Handle right value
      TRY.
          IF right_value CS '"' OR right_value CS ''''.
            right_result = substring( val = right_value
                                      off = 1
                                      len = strlen( right_value ) - 2 ).
          ELSE.
            right_result = resolve_variable( variable_path = right_value
                                             context       = context ).
          ENDIF.
        CATCH zcx_llm_template_parser.
          right_result = right_value.
      ENDTRY.

      " Try numeric comparison
      TRY.
          DATA lv_left_num  TYPE i.
          DATA lv_right_num TYPE i.

          " Check if both values can be converted to numbers
          lv_left_num = CONV i( left_result ).
          lv_right_num = CONV i( right_result ).

          " Perform numeric comparison
          CASE operator.
            WHEN '=='. result = xsdbool( lv_left_num = lv_right_num ).
            WHEN '!='. result = xsdbool( lv_left_num <> lv_right_num ).
            WHEN '>'. result = xsdbool( lv_left_num > lv_right_num ).
            WHEN '<'. result = xsdbool( lv_left_num < lv_right_num ).
            WHEN '>='. result = xsdbool( lv_left_num >= lv_right_num ).
            WHEN '<='. result = xsdbool( lv_left_num <= lv_right_num ).
          ENDCASE.
        CATCH cx_root.
          " If numeric comparison fails, do string comparison
          CASE operator.
            WHEN '=='. result = xsdbool( left_result = right_result ).
            WHEN '!='. result = xsdbool( left_result <> right_result ).
            WHEN '>'. result = xsdbool( left_result > right_result ).
            WHEN '<'. result = xsdbool( left_result < right_result ).
            WHEN '>='. result = xsdbool( left_result >= right_result ).
            WHEN '<='. result = xsdbool( left_result <= right_result ).
          ENDCASE.
      ENDTRY.

    ELSE.
      " Handle boolean value
      TRY.
          left_result = resolve_variable( variable_path = condition_text
                                          context       = context ).
          result = xsdbool(    left_result = 'true'
                            OR left_result = '1'
                            OR left_result = 'X'
                            OR left_result = 'x' ).
        CATCH zcx_llm_template_parser.
          result = xsdbool(    condition_text = 'true'
                            OR condition_text = '1'
                            OR condition_text = 'X'
                            OR condition_text = 'x' ).
      ENDTRY.
    ENDIF.

    IF is_negated = abap_true.
      result = xsdbool( result = abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD resolve_variable_ref.
    DATA path_segments TYPE string_table.
    DATA current_ref   TYPE REF TO data.

    " Add validation for empty path
    IF variable_path IS INITIAL.
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path
                                                   msgv1  = 'Empty variable path' ) ##NO_TEXT.
    ENDIF.

    " Add context validation
    IF context IS INITIAL.
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path
                                                   msgv1  = 'Context is initial' ) ##NO_TEXT.
    ENDIF.

    " Split path into segments
    SPLIT variable_path AT '.' INTO TABLE path_segments.

    " Start with the context
    current_ref = context.

    FIELD-SYMBOLS <current_data> TYPE any.
    ASSIGN current_ref->* TO <current_data>.

    " Navigate through the path
    LOOP AT path_segments INTO DATA(segment).
      DATA component TYPE string.

      " Handle array access if present (e.g., users[0].name)
      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\[(\d+)\])?'
           IN segment
           SUBMATCHES component DATA(index).

      TRY.
          DATA(descr) = cl_abap_typedescr=>describe_by_data( <current_data> ).

          CASE descr->kind.
            WHEN cl_abap_typedescr=>kind_struct.
              ASSIGN COMPONENT component OF STRUCTURE <current_data> TO FIELD-SYMBOL(<component>).
              IF sy-subrc <> 0.
                RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path ).
              ENDIF.

              CREATE DATA current_ref LIKE <component>.
              ASSIGN current_ref->* TO <current_data>.
              <current_data> = <component>.

            WHEN cl_abap_typedescr=>kind_table.
              DATA(table_ref) = CAST cl_abap_tabledescr( descr ).

              IF index IS NOT INITIAL.
                DATA(idx) = CONV i( index ).

                " Get line type from table descriptor
                DATA(line_type) = table_ref->get_table_line_type( ).
                DATA dref TYPE REF TO data.
                FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
                FIELD-SYMBOLS <line>  TYPE any.

                ASSIGN <current_data> TO <table>.
                CREATE DATA dref TYPE HANDLE line_type.
                ASSIGN dref->* TO <line>.

                " Read table line
                ASSIGN <table>[ idx ] TO <line>.
                IF sy-subrc <> 0.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                " Create new reference for the line
                CREATE DATA current_ref LIKE <line>.
                ASSIGN current_ref->* TO <current_data>.
                <current_data> = <line>.
              ELSE.
                " Return table reference if no index specified
                CREATE DATA current_ref LIKE <current_data>.
                ASSIGN current_ref->* TO <current_data>.
                <current_data> = <current_data>.
                result = current_ref.
                RETURN.
              ENDIF.

            WHEN OTHERS.
              RAISE EXCEPTION NEW zcx_llm_template_parser(
                                      textid = zcx_llm_template_parser=>unsupported_variable_type ).
          ENDCASE.

        CATCH cx_root INTO DATA(ex).
          RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                       previous = ex ).
      ENDTRY.
    ENDLOOP.

    result = current_ref.
  ENDMETHOD.

  METHOD process_loop_content.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN collection->* TO <table>.
    DATA(total_items) = lines( <table> ).

    " Get the structure of the original context
    DATA(orig_type) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( context ) ).
    DATA(components) = orig_type->get_components( ).

    " Get table type and line type
    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( collection ) ).
    DATA(line_type) = table_descr->get_table_line_type( ).

    " Add the loop variable component - first check if it exists
    IF NOT line_exists( components[ name = loop_var ] ).
      DATA component TYPE abap_componentdescr.
      component-name  = loop_var.
      component-type ?= line_type.
      INSERT component INTO TABLE components.
    ENDIF.

    " Add loop metadata component only if it doesn't exist
    IF NOT line_exists( components[ name = 'LOOP' ] ).
      component-name  = 'LOOP'.
      component-type ?= CAST cl_abap_typedescr(
        cl_abap_structdescr=>describe_by_data( VALUE loop_meta_type( ) ) ).
      INSERT component INTO TABLE components.
    ENDIF.

    " Create new structure type
    DATA(new_struct_type) = cl_abap_structdescr=>create( components ).

    DO total_items TIMES.
      DATA(current_index) = sy-index.

      " Create new context for this iteration
      DATA new_context TYPE REF TO data.
      CREATE DATA new_context TYPE HANDLE new_struct_type.
      ASSIGN new_context->* TO FIELD-SYMBOL(<new_ctx>).

      " Copy original context data
      ASSIGN context->* TO FIELD-SYMBOL(<ctx_data>).
      MOVE-CORRESPONDING <ctx_data> TO <new_ctx>.

      " Set loop variable
      ASSIGN <table>[ current_index ] TO FIELD-SYMBOL(<current_item>).
      ASSIGN COMPONENT loop_var OF STRUCTURE <new_ctx> TO FIELD-SYMBOL(<loop_var>).
      <loop_var> = <current_item>.

      " Set loop metadata
      ASSIGN COMPONENT 'LOOP' OF STRUCTURE <new_ctx> TO FIELD-SYMBOL(<loop_meta>).
      <loop_meta> = VALUE loop_meta_type( index = current_index
                                          first = xsdbool( current_index = 1 )
                                          last  = xsdbool( current_index = total_items ) ).

      " Process tokens with new context
      result = result && parse_tokens( tokens  = tokens
                                       context = new_context ).
    ENDDO.
  ENDMETHOD.


ENDCLASS.
