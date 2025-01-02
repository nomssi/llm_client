CLASS zcl_llm_encryption DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    INTERFACES zif_llm_encryption .
  PROTECTED SECTION.
    CLASS-DATA: subject  TYPE string,
                addrbook TYPE ssfpab.
    CONSTANTS application TYPE ssfappl VALUE 'ZLLMCT'.

    CONSTANTS bin_line TYPE i VALUE 255.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llm_encryption IMPLEMENTATION.
  METHOD zif_llm_encryption~decrypt.
    IF encrypted IS INITIAL.
      RETURN.
    ENDIF.

    DATA(total_bytes) = xstrlen( encrypted ).
    DATA ssfbin_tab TYPE STANDARD TABLE OF ssfbin.

    ssfbin_tab = VALUE #(
        FOR j = 0 THEN j + bin_line WHILE j < total_bytes (
        bindata = COND xstring(
          WHEN total_bytes - j >= bin_line
          THEN encrypted+j(bin_line)  " Fixed 255-byte chunk
          ELSE encrypted+j       " Remaining bytes
        )
      )
    ).

    DATA recipients TYPE TABLE OF ssfinfo.
    recipients = VALUE #( ( id = subject profile = addrbook ) ).

    DATA: output     TYPE STANDARD TABLE OF ssfbin,
          output_len TYPE i.

    CALL FUNCTION 'SSF_KRN_DEVELOPE'
      EXPORTING
        ostr_enveloped_data_l        = total_bytes
      IMPORTING
        ostr_output_data_l           = output_len
      TABLES
        ostr_enveloped_data          = ssfbin_tab
        recipient                    = recipients
        ostr_output_data             = output
      EXCEPTIONS
        ssf_krn_error                = 1
        ssf_krn_noop                 = 2
        ssf_krn_nomemory             = 3                " Main Memory Not Sufficient
        ssf_krn_opinv                = 4
        ssf_krn_nossflib             = 5
        ssf_krn_recipient_error      = 6                " Error Occurred During Reading of Recipient Line
        ssf_krn_input_data_error     = 7
        ssf_krn_invalid_par          = 8                " An SSF Parameter is Erroneous
        ssf_krn_invalid_parlen       = 9                " Length of an SSF Parameter is Erroneous
        ssf_fb_input_parameter_error = 10               " Function Module Parameter Error
        OTHERS                       = 11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>decryption_failed
          attr1  = CONV #( sy-subrc ).
    ENDIF.

    DATA(outdata) = REDUCE xstring(
        INIT r = VALUE xstring( )
        FOR wa IN output
        NEXT r = r && wa-bindata
    ).

    result = cl_binary_convert=>xstring_utf8_to_string( outdata ).
    result = result(output_len).
  ENDMETHOD.

  METHOD zif_llm_encryption~encrypt.

    IF unencrypted IS INITIAL.
      RETURN.
    ENDIF.

    DATA(to_encrypt) = cl_binary_convert=>string_to_xstring_utf8( unencrypted ).
    DATA(input_len) = xstrlen( to_encrypt ).
    DATA input_data TYPE STANDARD TABLE OF ssfbin.

    input_data = VALUE #(
            FOR j = 0 THEN j + bin_line WHILE j < input_len (
            bindata = COND xstring(
              WHEN input_len - j >= bin_line
              THEN to_encrypt+j(bin_line)  " Fixed 255-byte chunk
              ELSE to_encrypt+j       " Remaining bytes
            )
          )
        ).

    DATA recipients TYPE TABLE OF ssfinfo.
    recipients = VALUE #( ( id = subject ) ).

    DATA enveloped_data TYPE STANDARD TABLE OF ssfbin.

    CALL FUNCTION 'SSF_KRN_ENVELOPE'
      EXPORTING
        ostr_input_data_l            = input_len
        str_pab                      = addrbook
        str_pab_password             = ''
      TABLES
        ostr_input_data              = input_data
        recipient_list               = recipients
        ostr_enveloped_data          = enveloped_data
      EXCEPTIONS
        ssf_krn_error                = 1
        ssf_krn_noop                 = 2
        ssf_krn_nomemory             = 3                " Main Memory Not Sufficient
        ssf_krn_opinv                = 4
        ssf_krn_nossflib             = 5
        ssf_krn_recipient_list_error = 6                " Error Occurred During Reading of Recipient Line
        ssf_krn_input_data_error     = 7
        ssf_krn_invalid_par          = 8                " An SSF Parameter is Erroneous
        ssf_krn_invalid_parlen       = 9                " Length of an SSF Parameter is Erroneous
        ssf_fb_input_parameter_error = 10               " Function Module Parameter Error
        OTHERS                       = 11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>encryption_failed
          attr1  = CONV #( sy-subrc ).
    ENDIF.

    LOOP AT enveloped_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      CONCATENATE result <fs_data>-bindata INTO result IN BYTE MODE.
    ENDLOOP.
  ENDMETHOD.

  METHOD class_constructor.
    DATA profile TYPE localfile.

    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING
        applic        = application
      IMPORTING
        profile       = profile
      EXCEPTIONS
        pse_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0 OR profile IS INITIAL.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>encryption_setup
          attr1  = 'Filename'
          attr2  = CONV #( sy-subrc ) ##NO_TEXT.
    ENDIF.

    DATA certificate TYPE xstring.
    addrbook = profile.

    CALL FUNCTION 'SSFC_GET_CERTIFICATE'
      EXPORTING
        profile               = addrbook
      IMPORTING
        certificate           = certificate
      EXCEPTIONS
        ssf_krn_error         = 1
        ssf_krn_nomemory      = 2                " Main Memory Not Sufficient
        ssf_krn_nossflib      = 3
        ssf_krn_invalid_par   = 4                " An SSF Parameter is Erroneous
        ssf_krn_nocertificate = 5
        OTHERS                = 6.
    IF sy-subrc <> 0 OR certificate IS INITIAL.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>encryption_setup
          attr1  = 'GetCert'
          attr2  = CONV #( sy-subrc ).
    ENDIF.

    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = certificate
      IMPORTING
        subject             = subject
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2                " Main Memory Not Sufficient
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4                " An SSF Parameter is Erroneous
        OTHERS              = 5.
    IF sy-subrc <> 0 OR subject IS INITIAL.
      RAISE EXCEPTION TYPE zcx_llm_validation
        EXPORTING
          textid = zcx_llm_validation=>encryption_setup
          attr1  = 'ParseCert'
          attr2  = CONV #( sy-subrc ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
