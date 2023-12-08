*&---------------------------------------------------------------------*
*& Include          ZGOOG_PROCESS_PO_AUTO_IMPL
*&---------------------------------------------------------------------*

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.

    DATA: lv_msg    TYPE string,
          lo_cx_sdk TYPE REF TO /goog/cx_sdk.

    TRY.
        CREATE OBJECT mo_storage_client
          EXPORTING
            iv_key_name = p_key.
        CREATE OBJECT mo_docai_client
          EXPORTING
            iv_key_name = p_key.
        CREATE OBJECT mo_addrvaldn_client
          EXPORTING
            iv_key_name = p_key.
        CREATE OBJECT mo_translate_client
          EXPORTING
            iv_key_name = p_key.
        CREATE OBJECT mo_pubsub_client
          EXPORTING
            iv_key_name = p_key.
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        lv_msg = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD execute.

    CALL METHOD read_purchase_orders
      IMPORTING
        ex_t_po_content = DATA(lt_po_content).
    IF lt_po_content IS NOT INITIAL.
      CALL METHOD process_purchase_orders
        EXPORTING
          im_t_po_content = lt_po_content
        IMPORTING
          ex_t_output     = DATA(lt_output).
      IF lt_output IS NOT INITIAL.
        CALL METHOD display_output
          EXPORTING
            im_t_output = lt_output.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD read_purchase_orders.

    DATA:
          ls_po_content         TYPE gty_po_content.

    CALL METHOD list_bucket_objects
      IMPORTING
        ex_t_objects = DATA(lt_objects).
    IF lt_objects IS NOT INITIAL.
      LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
        CALL METHOD read_bucket_object
          EXPORTING
            iv_object_name = <ls_object>-name
          IMPORTING
            ex_mime_type   = DATA(lv_mime_type)
            es_content     = DATA(lv_content).

        ls_po_content-name      = <ls_object>-name.
        ls_po_content-mime_type = lv_mime_type.
        ls_po_content-content   = lv_content.

        APPEND ls_po_content TO ex_t_po_content.
        CLEAR ls_po_content.

      ENDLOOP.

    ENDIF.

    CALL METHOD read_gmail
      IMPORTING
        ex_t_po_content = DATA(lt_po_content).
    IF lt_po_content IS NOT INITIAL.
      APPEND LINES OF lt_po_content TO ex_t_po_content.

    ENDIF.

  ENDMETHOD.

  METHOD list_bucket_objects.

    DATA:
      ls_item   TYPE /goog/cl_storage_v1=>ty_013.

    DATA:
          lv_msg          TYPE string.

    TRY.
        CALL METHOD mo_storage_client->list_objects
          EXPORTING
            iv_p_bucket = p_bucket
            iv_q_prefix = p_prefix
          IMPORTING
            es_output   = DATA(ls_object).

        LOOP AT ls_object-items ASSIGNING FIELD-SYMBOL(<ls_item>) WHERE name NE p_prefix.
          APPEND <ls_item> TO ex_t_objects.

        ENDLOOP.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        lv_msg = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD read_bucket_object.

    DATA:
      lv_msg      TYPE string,
      lv_ret_code TYPE i,
      lv_err_text TYPE string,
      ls_err_resp TYPE /goog/err_resp,
      ls_output   TYPE /goog/cl_storage_v1=>ty_013.

    DATA:
          lo_cx_sdk    TYPE REF TO /goog/cx_sdk.

    TRY.
        mo_storage_client->get_objects(
          EXPORTING
            iv_p_bucket = p_bucket
            iv_p_object = iv_object_name
          IMPORTING
            es_output   = ls_output
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp
            es_raw      = es_content ).
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        lv_msg = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

    IF ls_output-content_type IS NOT INITIAL.
      ex_mime_type = ls_output-content_type.
    ELSE.
      ex_mime_type = 'application/pdf'.

    ENDIF.

    mo_storage_client->add_common_qparam( iv_name  = 'alt'
                                          iv_value = 'media' ).

    TRY.
        mo_storage_client->get_objects(
          EXPORTING
            iv_p_bucket = p_bucket
            iv_p_object = iv_object_name
          IMPORTING
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp
            es_raw      = es_content ).
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        lv_msg = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD process_purchase_orders.

    DATA:
          lt_properties         TYPE /goog/cl_documentai_v1=>ty_t_023.

    DATA:
      ls_documentdata  TYPE gty_documentdata,
      ls_document_item TYPE gty_documentitemdata,
      ls_input         TYPE /goog/cl_documentai_v1=>ty_084,
      ls_output        TYPE /goog/cl_documentai_v1=>ty_085,
      ls_output_data   TYPE gty_output.

    DATA:
      lv_item_id    TYPE sy-tabix,
      lv_project_id TYPE string,
      lv_msg        TYPE string,
      lv_xstring    TYPE xstring.

    DATA:
          lo_exception          TYPE REF TO /goog/cx_sdk.

    FIELD-SYMBOLS:
                   <lt_lines> TYPE data.

    LOOP AT im_t_po_content ASSIGNING FIELD-SYMBOL(<ls_po_content>).
      CALL METHOD parse_purchase_order
        EXPORTING
          im_s_po_content = <ls_po_content>
        IMPORTING
          ex_ret_code     = DATA(lv_ret_code)
          ex_err_text     = DATA(lv_err_text)
          ex_t_po_output  = DATA(lt_po_output).
      ls_documentdata-documentname = <ls_po_content>-name.
      LOOP AT lt_po_output-document-entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
        CASE <ls_entity>-type.
          WHEN 'purchase_order'.
            ls_documentdata-purchaseordernumber = <ls_entity>-mention_text.
          WHEN 'delivery_date'.
            ls_documentdata-requesteddeliverydate = <ls_entity>-mention_text.
          WHEN 'receiver_name'.
            ls_documentdata-soldtoname = <ls_entity>-mention_text.
          WHEN 'receiver_address'.
            ls_documentdata-soldtoaddress = <ls_entity>-mention_text.
          WHEN 'ship_to_name'.
            ls_documentdata-shiptoname = <ls_entity>-mention_text.
          WHEN 'ship_to_address'.
            ls_documentdata-shiptoaddress = <ls_entity>-mention_text.
          WHEN 'line_item'.
            ASSIGN <ls_entity>-properties->* TO <lt_lines>.
            DATA(lv_json) = /goog/cl_json_util=>serialize_json( is_data = <lt_lines> ).
            /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_json
                                                            iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                  IMPORTING es_data        = lt_properties ).
            CLEAR ls_document_item.
            ls_document_item-documentname = <ls_po_content>-name.
            ls_document_item-itemid = sy-tabix.
            LOOP AT lt_properties INTO DATA(lw_prop).
              CASE lw_prop-type.
                WHEN 'line_item/product_code'.
                  ls_document_item-customermaterialnumber = lw_prop-mention_text.
                WHEN 'line_item/quantity'.
                  ls_document_item-quantity = lw_prop-mention_text.
              ENDCASE.
            ENDLOOP.
            lv_item_id = lv_item_id + 1.
            ls_document_item-itemid = lv_item_id.
            APPEND ls_document_item TO ls_documentdata-documentdataitemset.
            CLEAR ls_document_item.

        ENDCASE.

      ENDLOOP.

      IF mo_docai_client->is_success( lv_ret_code ) NE abap_true.
        CONTINUE.

      ENDIF.

      IF ls_documentdata-soldtoaddress IS NOT INITIAL.
        CALL METHOD get_customer_from_address
          EXPORTING
            im_address           = ls_documentdata-soldtoaddress
          IMPORTING
            ex_formatted_address = ls_documentdata-soldtoaddress
            ex_customer_number   = ls_documentdata-soldtonumber.

      ENDIF.

      IF ls_documentdata-shiptoaddress IS NOT INITIAL.
        CALL METHOD get_customer_from_address
          EXPORTING
            im_address           = ls_documentdata-shiptoaddress
          IMPORTING
            ex_formatted_address = ls_documentdata-shiptoaddress
            ex_customer_number   = ls_documentdata-shiptonumber.

      ENDIF.

      CLEAR:  ls_output.

      IF <ls_po_content>-mail_content IS INITIAL.
        DATA(lv_content_b64) = cl_http_utility=>encode_x_base64( <ls_po_content>-content ).
        ls_input-raw_document-content = lv_content_b64.
      ELSE.
        ls_input-raw_document-content = <ls_po_content>-mail_content.

      ENDIF.

      ls_input-raw_document-mime_type = <ls_po_content>-mime_type.
      lv_project_id = mo_docai_client->gv_project_id.

      TRY.
          CALL METHOD mo_docai_client->process_processors
            EXPORTING
              iv_p_projects_id   = lv_project_id
              iv_p_locations_id  = p_loca
              iv_p_processors_id = 'ea91f8d5f8195c9'
              is_input           = ls_input
            IMPORTING
              es_output          = ls_output.

          READ TABLE ls_output-document-pages INTO DATA(ls_page) INDEX 1.
          IF sy-subrc EQ 0.
            LOOP AT ls_page-form_fields INTO DATA(ls_form_field).
              IF ls_form_field-field_name-text_anchor-content = 'Purchase Order Date:'.
                ls_documentdata-purchaseorderdate = ls_form_field-field_value-text_anchor-content.

              ENDIF.

            ENDLOOP.

          ENDIF.

          DATA: lv_tmp TYPE string.

          IF ls_output-document-text IS NOT INITIAL.
            SPLIT ls_output-document-text AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_text_lines).
            DATA: lv_comment_index TYPE sy-tabix.
            LOOP AT lt_text_lines INTO DATA(lv_text_line).
              IF lv_text_line CS 'Purchase Order Date:'.
                SPLIT lv_text_line AT ':' INTO lv_tmp ls_documentdata-purchaseorderdate.
                CONDENSE ls_documentdata-purchaseorderdate NO-GAPS.
              ENDIF.
              IF lv_text_line EQ 'Delivery Comments'.
                lv_comment_index = sy-tabix + 1.
                EXIT.
              ENDIF.
            ENDLOOP.

            IF lv_comment_index > 0.
              LOOP AT lt_text_lines INTO lv_text_line FROM  lv_comment_index.
                CONCATENATE ls_documentdata-deliveryinstructions lv_text_line INTO ls_documentdata-deliveryinstructions.
              ENDLOOP.
            ENDIF.

            ls_documentdata-deliveryinstructionsoriginal = ls_documentdata-deliveryinstructions.
            CALL METHOD translate_delivery_comments
              IMPORTING
                im_source_language = ls_documentdata-deliveryinstructionsoriginalla
              CHANGING
                ch_comments        = ls_documentdata-deliveryinstructions.

          ENDIF.

          CALL METHOD create_sales_order
            EXPORTING
              im_documentdata = ls_documentdata
            IMPORTING
              ex_error        = DATA(lv_error)
              ex_vbeln        = DATA(lv_vbeln).
          IF lv_error IS INITIAL.
            ls_output_data-file_name = <ls_po_content>-name.
            ls_output_data-ebeln     = ls_documentdata-purchaseordernumber.
            ls_output_data-remarks   = 'Sales Order created successfully'.
            ls_output_data-vbeln     = lv_vbeln.

            IF <ls_po_content>-mail_content IS INITIAL.
              ls_output_data-source = 'Cloud Storage'.
            ELSE.
              ls_output_data-source = 'Gmail'.

            ENDIF.

            APPEND ls_output_data TO ex_t_output.
            CLEAR ls_output_data.
          ELSE.
            ls_output_data-file_name = <ls_po_content>-name.
            ls_output_data-ebeln     = ls_documentdata-purchaseordernumber.
            ls_output_data-remarks   = lv_msg.

            IF <ls_po_content>-mail_content IS INITIAL.
              ls_output_data-source = 'Cloud Storage'.
            ELSE.
              ls_output_data-source = 'Gmail'.

            ENDIF.

            APPEND ls_output_data TO ex_t_output.
            CLEAR ls_output_data.

          ENDIF.

          CLEAR ls_documentdata.

        CATCH /goog/cx_sdk INTO lo_exception.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD parse_purchase_order.

    DATA:
      ls_input    TYPE /goog/cl_documentai_v1=>ty_084,
      ls_err_resp TYPE /goog/err_resp.

    DATA:
      lv_output     TYPE string,
      lv_msg        TYPE string,
      lv_project_id TYPE string.

    DATA:
          lo_exception    TYPE REF TO /goog/cx_sdk.

    IF im_s_po_content-mail_content IS INITIAL.
      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = im_s_po_content-content
        IMPORTING
          output = lv_output.
    ELSE.
      lv_output = im_s_po_content-mail_content.

    ENDIF.

    TRY.
        ls_input-raw_document-content   = lv_output.
        ls_input-raw_document-mime_type = im_s_po_content-mime_type.
        lv_project_id = mo_docai_client->gv_project_id.

        CALL METHOD mo_docai_client->process_processors
          EXPORTING
            iv_p_projects_id   = lv_project_id
            iv_p_locations_id  = p_loca
            iv_p_processors_id = p_procid
            is_input           = ls_input
          IMPORTING
            es_output          = ex_t_po_output
            ev_ret_code        = ex_ret_code
            ev_err_text        = ex_err_text
            es_err_resp        = ls_err_resp.
      CATCH /goog/cx_sdk INTO lo_exception.
        lv_msg = lo_exception->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD get_customer_from_address.
    DATA: lo_address_validator TYPE REF TO /goog/cl_addrvaldn_v1,
          lv_err_text          TYPE string,
          lv_ret_code          TYPE i,
          ls_input             TYPE /goog/cl_addrvaldn_v1=>ty_012,
          ls_output            TYPE /goog/cl_addrvaldn_v1=>ty_013,
          ls_err_resp          TYPE /goog/err_resp,
          lo_exception         TYPE REF TO /goog/cx_sdk.

    DATA: lv_stras TYPE kna1-stras,
          lv_ort01 TYPE kna1-ort01,
          lv_pstlz TYPE kna1-pstlz.

    TRY.
        APPEND im_address TO ls_input-address-address_lines.

        CALL METHOD mo_addrvaldn_client->validate_address
          EXPORTING
            is_input    = ls_input
          IMPORTING
            es_output   = ls_output
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp.


        DATA(standardized_address) = ls_output-result-usps_data-standardized_address.
        DATA(postal_address) = ls_output-result-address-postal_address.

        lv_stras = standardized_address-first_address_line.
        lv_ort01 = standardized_address-city.
        lv_pstlz = standardized_address-zip_code.

        CONCATENATE standardized_address-first_address_line
                    standardized_address-city_state_zip_address_line
               INTO ex_formatted_address SEPARATED BY ', '.

        SELECT SINGLE kunnr
          FROM kna1
          INTO ex_customer_number
          WHERE stras = lv_stras AND
                ort01 = lv_ort01 AND
                pstlz = lv_pstlz.
      CATCH /goog/cx_sdk INTO lo_exception.

    ENDTRY.

  ENDMETHOD.

  METHOD translate_delivery_comments.

    DATA:
      lt_inp    TYPE STANDARD TABLE OF string,
      ls_treq   TYPE /goog/cl_translation_v2=>ty_006,
      ls_tresp  TYPE /goog/cl_translation_v2=>ty_007,
      lo_cx_sdk TYPE REF TO /goog/cx_sdk.

    TRY.
        DATA(lv_comments_language) = detect_comments_language( ch_comments ).

        im_source_language = lv_comments_language.

        IF lv_comments_language IS INITIAL OR lv_comments_language EQ 'en'.
          RETURN.

        ENDIF.

        APPEND ch_comments TO lt_inp.

        ls_treq-q = lt_inp.
        ls_treq-format = 'text'.

        ls_treq-target = 'en'.
        ls_treq-source = lv_comments_language.

        mo_translate_client->translate_translations(
          EXPORTING
            is_input  = ls_treq
          IMPORTING
            es_output = ls_tresp ).
        DATA(ls_translated_text) = ls_tresp-data-translations[ 1 ].
        ch_comments = ls_translated_text-translated_text.
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        "todo: error handle

    ENDTRY.

  ENDMETHOD.

  METHOD detect_comments_language.

    DATA:
      ls_inp        TYPE /goog/cl_translation_v2=>ty_001,
      lt_inp        TYPE /goog/cl_translation_v2=>ty_t_string,
      ls_tresp      TYPE /goog/cl_translation_v2=>ty_002,
      ls_detections TYPE /goog/cl_translation_v2=>ty_t_003,
      ls_det_resp   TYPE /goog/cl_translation_v2=>ty_003,
      mv_ret_code   TYPE i,
      mv_err_text   TYPE string,
      mv_err_resp   TYPE /goog/err_resp,
      lo_cx_sdk     TYPE REF TO /goog/cx_sdk.

    TRY.
        APPEND im_comments TO ls_inp-q.

        mo_translate_client->detect_detections(
          EXPORTING
            is_input    = ls_inp
          IMPORTING
            es_output   = ls_tresp
            ev_ret_code = mv_ret_code
            ev_err_text = mv_err_text
            es_err_resp = mv_err_resp
        ).

        ls_detections = ls_tresp-data-detections[ 1 ].
        rv_language = ls_detections[ 1 ]-language.

      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        "todo: error handle

    ENDTRY.

  ENDMETHOD.

  METHOD create_sales_order.

    " Get the sales area first
    DATA: ls_knvv TYPE knvv.
    SELECT SINGLE * FROM knvv INTO @ls_knvv WHERE kunnr = @im_documentdata-soldtonumber.


    "Header
    DATA: lw_order_header_in TYPE  bapisdhd1.
    lw_order_header_in-doc_type = 'ZOR'.
    lw_order_header_in-sales_org = ls_knvv-vkorg.
    lw_order_header_in-distr_chan = ls_knvv-vtweg.
    lw_order_header_in-division = ls_knvv-spart.
    lw_order_header_in-purch_no_c = im_documentdata-purchaseordernumber.
    lw_order_header_in-purch_date = convert_date( im_documentdata-purchaseorderdate ).

    "Partner
    DATA: lw_order_partner TYPE  bapiparnr,
          lt_order_partner TYPE TABLE OF bapiparnr.

    lw_order_partner-partn_role = 'AG'.
    lw_order_partner-partn_numb = im_documentdata-soldtonumber.
    APPEND lw_order_partner TO lt_order_partner.

    lw_order_partner-partn_role = 'WE'.
    lw_order_partner-partn_numb = im_documentdata-shiptonumber.
    APPEND lw_order_partner TO lt_order_partner.


    "Header text
    DATA: lt_text  TYPE TABLE OF  bapisdtext,
          lw_t002  TYPE t002,
          lv_laiso TYPE t002-laiso.

    get_order_header_text( EXPORTING iv_text  = im_documentdata-deliveryinstructions
                                     iv_langu = 'en'
                           CHANGING  ex_t_text  = lt_text ).

    lv_laiso = im_documentdata-deliveryinstructionsoriginalla .
    TRANSLATE lv_laiso TO UPPER CASE.

    SELECT SINGLE * FROM t002 INTO @lw_t002 WHERE laiso =  @lv_laiso.
    IF sy-subrc EQ 0.
      get_order_header_text( EXPORTING iv_text  = im_documentdata-deliveryinstructionsoriginal
                                       iv_langu = im_documentdata-deliveryinstructionsoriginalla
                             CHANGING  ex_t_text  = lt_text ).
    ENDIF.

    "Items
    DATA: lw_order_item     TYPE  bapisditm,
          lt_order_items    TYPE  TABLE OF bapisditm,
          lw_order_schedule TYPE bapischdl,
          lt_order_schedule TYPE TABLE OF bapischdl.

    LOOP AT im_documentdata-documentdataitemset INTO DATA(lw_item).
      lw_order_item-itm_number = lw_order_schedule-itm_number = lw_order_schedule-sched_line = sy-tabix.

      lw_order_item-material = lw_item-customermaterialnumber.
      lw_order_item-target_qty = lw_order_schedule-req_qty =  lw_item-quantity.

      lw_order_schedule-req_date = convert_date( im_documentdata-requesteddeliverydate ).

      APPEND lw_order_item TO lt_order_items.
      APPEND lw_order_schedule TO lt_order_schedule.
    ENDLOOP.

    DATA:  lt_return TYPE TABLE OF  bapiret2.

    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
        order_header_in       = lw_order_header_in
        int_number_assignment = abap_true
      IMPORTING
        salesdocument         = ex_vbeln
      TABLES
        return                = lt_return
        order_items_in        = lt_order_items
        order_partners        = lt_order_partner
        order_schedules_in    = lt_order_schedule
        order_text            = lt_text.

    COMMIT WORK AND WAIT.

    " Send to pubsub
    send_to_pubsub( ex_vbeln ).


  ENDMETHOD.

  METHOD convert_date.

    CONCATENATE iv_in_date+6(4)
                iv_in_date+0(2)
                iv_in_date+3(2)
                INTO rv_out_date.

  ENDMETHOD.

  METHOD get_order_header_text.

    "Header text
    DATA: lv_text TYPE string,
          lw_text TYPE  bapisdtext.

    lv_text = iv_text.
    DATA(lv_len) = strlen( lv_text ).

    lw_text-text_id = '0002'.
    lw_text-itm_number = '000000'.
    lw_text-langu_iso = iv_langu.

    WHILE lv_len > 0.
      IF lv_len > 132.
        lw_text-text_line = lv_text+0(132).
        APPEND lw_text TO ex_t_text.
        lv_len = lv_len - 132.
        lv_text = lv_text+132(lv_len).
      ELSE.
        lw_text-text_line = lv_text+0(lv_len).
        APPEND lw_text TO ex_t_text.
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD send_to_pubsub.
    DATA ls_input      TYPE /goog/cl_pubsub_v1=>ty_023.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA ls_output TYPE /goog/cl_pubsub_v1=>ty_024.
    DATA ls_message    TYPE /goog/cl_pubsub_v1=>ty_025.
    DATA lv_project_id TYPE string.
    DATA lv_topic_name TYPE string.

    TRY.
        lv_topic_name = p_topic.
        lv_project_id = mo_pubsub_client->gv_project_id.

        DATA(lv_msg) = | Sales Order { iv_sales_order_number } successfully created |.

        ls_message-data = cl_http_utility=>encode_base64( unencoded = lv_msg ).
        APPEND ls_message TO ls_input-messages.

        mo_pubsub_client->publish_topics( EXPORTING iv_p_projects_id = lv_project_id
                                                    iv_p_topics_id   = lv_topic_name
                                                    is_input         = ls_input
                                          IMPORTING es_output        = ls_output ).

      CATCH /goog/cx_sdk.
        " todo: error handling.
    ENDTRY.
  ENDMETHOD.

  METHOD display_output.

    DATA:
          lo_column     TYPE REF TO cl_salv_column_table.

    DATA(lt_output) = im_t_output.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = lt_output.
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(lo_functions) = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

    DATA(lo_columns) = lo_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).

    TRY.
        lo_column ?= lo_columns->get_column( 'FILE_NAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'File Name' ).
        lo_column->set_medium_text( 'File Name' ).
        lo_column->set_short_text( 'File Name' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REMARKS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Remarks' ).
        lo_column->set_medium_text( 'Remarks' ).
        lo_column->set_short_text( 'Remarks' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'SOURCE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Source' ).
        lo_column->set_medium_text( 'Source' ).
        lo_column->set_short_text( 'Source' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_alv->display( ).

  ENDMETHOD.

  METHOD close_connection.
    IF mo_storage_client IS BOUND.
      mo_storage_client->close( ).

    ENDIF.

    IF mo_docai_client IS BOUND.
      mo_docai_client->close( ).

    ENDIF.

    IF mo_addrvaldn_client IS BOUND.
      mo_addrvaldn_client->close( ).

    ENDIF.

    IF mo_translate_client IS BOUND.
      mo_translate_client->close( ).

    ENDIF.

    IF mo_pubsub_client IS BOUND.
      mo_pubsub_client->close( ).

    ENDIF.

  ENDMETHOD.

  METHOD read_gmail.

* Data declarations
    DATA:
      lt_message_parts     TYPE /goog/cl_gmail_v1=>ty_t_038,
      lt_message_sub_parts TYPE /goog/cl_gmail_v1=>ty_t_038,
      ls_po_content        TYPE gty_po_content,
      lv_q_labelids        TYPE string,
      lv_p_user_id         TYPE string,
      lv_xfile             TYPE xstring,
      lv_email_text        TYPE string.

    FIELD-SYMBOLS:
               <lt_message_parts> TYPE data.

    TRY.

* Open HTTP Connection
        DATA(lo_client) = NEW /goog/cl_gmail_v1( iv_key_name = 'TEST_GMAIL' ).

* Populate relevant parameters
        lv_q_labelids = 'Label_1925640816572369056'.
        lv_p_user_id = 'me'.

* Call API method: gmail.users.messages.list
        CALL METHOD lo_client->list_messages
          EXPORTING
            iv_q_labelids = lv_q_labelids
            iv_p_user_id  = lv_p_user_id
          IMPORTING
            es_output     = DATA(ls_output)
            ev_ret_code   = DATA(lv_ret_code)
            ev_err_text   = DATA(lv_err_text)
            es_err_resp   = DATA(ls_err_resp).

        IF lo_client->is_success( lv_ret_code ).
          LOOP AT ls_output-messages ASSIGNING FIELD-SYMBOL(<ls_message>).
            CALL METHOD lo_client->get_messages
              EXPORTING
                iv_p_id      = <ls_message>-id
                iv_p_user_id = lv_p_user_id
              IMPORTING
                es_output    = DATA(ls_message)
                ev_ret_code  = lv_ret_code
                ev_err_text  = lv_err_text
                es_err_resp  = ls_err_resp.
            IF lo_client->is_success( lv_ret_code ).
* Post processing to extract the response data reference to ABAP Type shipped with the SDK
              ASSIGN ls_message-payload-parts->* TO <lt_message_parts>.
              DATA(lv_json) = /goog/cl_json_util=>serialize_json( is_data = <lt_message_parts> ).
              /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_json
                                                              iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                    IMPORTING es_data        = lt_message_parts ).
* Iterate internal table to browse through the message parts which have attachments
              LOOP AT lt_message_parts ASSIGNING FIELD-SYMBOL(<ls_message_part>) WHERE filename IS NOT INITIAL.
* Call API method: gmail.users.messages.attachments.get
                CALL METHOD lo_client->get_attachments
                  EXPORTING
                    iv_p_id         = <ls_message_part>-body-attachment_id
                    iv_p_message_id = <ls_message>-id
                    iv_p_user_id    = lv_p_user_id
                  IMPORTING
                    es_output       = DATA(ls_output_attachment)
                    ev_ret_code     = lv_ret_code
                    ev_err_text     = lv_err_text
                    es_err_resp     = ls_err_resp.
                IF lo_client->is_success( lv_ret_code ).
                  ls_po_content-name         = <ls_message_part>-filename.
                  ls_po_content-mime_type    = <ls_message_part>-mime_type.
                  ls_po_content-mail_content = ls_output_attachment-data.

                  APPEND ls_po_content TO ex_t_po_content.
                  CLEAR ls_po_content.
                ELSE.
                  MESSAGE lv_err_text TYPE 'E'.

                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDLOOP.
        ELSE.
          MESSAGE lv_err_text TYPE 'E'.

        ENDIF.

* Close HTTP Connection
        lo_client->close( ).

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        MESSAGE lo_exception->get_text( ) TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
