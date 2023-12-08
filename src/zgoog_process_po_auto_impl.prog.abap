*&---------------------------------------------------------------------*
*& Include          ZGOOG_PROCESS_PO_AUTO_IMPL
*&---------------------------------------------------------------------*

CLASS lcl_main IMPLEMENTATION.
  METHOD constructor.
    DATA lv_msg    TYPE string.
    DATA lo_cx_sdk TYPE REF TO /goog/cx_sdk.

    TRY.
        mo_storage_client = NEW #( iv_key_name = p_key ).
        mo_docai_client = NEW #( iv_key_name = p_key ).
        mo_addrvaldn_client = NEW #( iv_key_name = p_key ).
        mo_translate_client = NEW #( iv_key_name = p_key ).
        mo_pubsub_client = NEW #( iv_key_name = p_key ).
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        lv_msg = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.
  ENDMETHOD.

  METHOD execute.

* Call method to read Purchase Orders
    CALL METHOD read_purchase_orders
      IMPORTING
        ex_t_po_content = DATA(lt_po_content).
    IF lt_po_content IS NOT INITIAL.
* Call method to process Purchase Orders
      CALL METHOD process_purchase_orders
        EXPORTING
          im_t_po_content = lt_po_content
        IMPORTING
          ex_t_output     = DATA(lt_output).
      IF lt_output IS NOT INITIAL.
* Call method to display processed POs in an ALV
        CALL METHOD display_output
          EXPORTING
            im_t_output = lt_output.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD read_purchase_orders.

* Structure declarations
    DATA:
          ls_po_content         TYPE gty_po_content.

* Call method to invoke ABAP SDK for Google Cloud to list POs from a GCS bucket
    CALL METHOD list_bucket_objects
      IMPORTING
        ex_t_objects = DATA(lt_objects).
    IF lt_objects IS NOT INITIAL.
      LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
* Call method to invoke ABAP SDK for Google Cloud to read raw data of a PO from GCS bucket
        CALL METHOD read_bucket_object
          EXPORTING
            iv_object_name = <ls_object>-name
          IMPORTING
            ex_mime_type   = DATA(lv_mime_type)
            es_content     = DATA(lv_content).

        ls_po_content-name      = <ls_object>-name.
        ls_po_content-mime_type = lv_mime_type.
        ls_po_content-content   = lv_content.

* Prepare exporting table of read POs
        APPEND ls_po_content TO ex_t_po_content.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD list_bucket_objects.

    TRY.
* Call method of GCS API client class to list POs
        CALL METHOD mo_storage_client->list_objects
          EXPORTING
            iv_p_bucket = p_bucket
            iv_q_prefix = p_prefix
          IMPORTING
            es_output   = DATA(ls_object).
* Iterate internal table to prepare prepare list of read items
        LOOP AT ls_object-items ASSIGNING FIELD-SYMBOL(<ls_item>) WHERE name NE p_prefix.
          APPEND <ls_item> TO ex_t_objects.

        ENDLOOP.
* Catch SDK exceptions
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        DATA(lv_msg) = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD read_bucket_object.

    TRY.
* Call method of GCS API client class to get mime type of the PO document
        mo_storage_client->get_objects(
          EXPORTING
            iv_p_bucket = p_bucket
            iv_p_object = iv_object_name
          IMPORTING
            es_output   = DATA(ls_output)
            ev_ret_code = DATA(lv_ret_code)
            ev_err_text = DATA(lv_err_text)
            es_err_resp = DATA(ls_err_resp)
            es_raw      = es_content ).
* Catch SDK exceptions
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        DATA(lv_msg) = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

* Set content type of the PO document
    IF ls_output-content_type IS NOT INITIAL.
      ex_mime_type = ls_output-content_type.
    ELSE.
      ex_mime_type = 'application/pdf'.

    ENDIF.

* Prepare query parameters for the GCS API client method to get PO raw data
    mo_storage_client->add_common_qparam( iv_name  = 'alt'
                                          iv_value = 'media' ).

    TRY.
* Call method of GCS API client class to get raw data of a PO
        mo_storage_client->get_objects(
          EXPORTING
            iv_p_bucket = p_bucket
            iv_p_object = iv_object_name
          IMPORTING
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp
            es_raw      = es_content ).
* Catch SDK exceptions
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        lv_msg = lo_cx_sdk->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD process_purchase_orders.

* Internal table declarations
    DATA:
          lt_properties         TYPE /goog/cl_documentai_v1=>ty_t_023.

* Structure declarations
    DATA:
      ls_documentdata  TYPE gty_documentdata,
      ls_document_item TYPE gty_documentitemdata,
      ls_input         TYPE /goog/cl_documentai_v1=>ty_084,
      ls_output        TYPE /goog/cl_documentai_v1=>ty_085,
      ls_output_data   TYPE gty_output.

* Local variable declarations
    DATA:
      lv_item_id    TYPE sy-tabix,
      lv_project_id TYPE string,
      lv_msg        TYPE string.

* Field symbol declarations
    FIELD-SYMBOLS:
                   <lt_lines> TYPE data.

* Iterate internal table to parse POs, validate and process read items, create Sales Orders and send
* Order Confirmation message
    LOOP AT im_t_po_content ASSIGNING FIELD-SYMBOL(<ls_po_content>).
      TRY.
* Call method of Document AI API client class to parse POs
          CALL METHOD parse_purchase_order
            EXPORTING
              im_s_po_content = <ls_po_content>
            IMPORTING
              ex_ret_code     = DATA(lv_ret_code)
              ex_err_text     = DATA(lv_err_text)
              ex_t_po_output  = DATA(lt_po_output).
          ls_documentdata-documentname = <ls_po_content>-name.
* Iterate internal table prepare Sales Order data from parse PO entities
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
* Get line item data from parsed PO content in ABAP type shipped with the SDK
* Developers need not create local or DDIC ABAP Types to get parsed PO entities
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
* Call method of Address Validation API client class to validate and correct Sold-to Party address
            CALL METHOD get_customer_from_address
              EXPORTING
                im_address           = ls_documentdata-soldtoaddress
              IMPORTING
                ex_formatted_address = ls_documentdata-soldtoaddress
                ex_customer_number   = ls_documentdata-soldtonumber.

          ENDIF.

          IF ls_documentdata-shiptoaddress IS NOT INITIAL.
* Call method of Address Validation API client class to validate and correct Ship-to Party address
            CALL METHOD get_customer_from_address
              EXPORTING
                im_address           = ls_documentdata-shiptoaddress
              IMPORTING
                ex_formatted_address = ls_documentdata-shiptoaddress
                ex_customer_number   = ls_documentdata-shiptonumber.

          ENDIF.

          CLEAR ls_output.
          DATA(lv_content_b64) = cl_http_utility=>encode_x_base64( <ls_po_content>-content ).
          ls_input-raw_document-content   = lv_content_b64.
          ls_input-raw_document-mime_type = <ls_po_content>-mime_type.
          lv_project_id = mo_docai_client->gv_project_id.
* Call class method to determine "Purchase Order Date" using an OCR parser,
* Or, skip below by training the PO parser to annotate and read the "Purchase Order Date"
          CALL METHOD mo_docai_client->process_processors
            EXPORTING
              iv_p_projects_id   = lv_project_id
              iv_p_locations_id  = p_loca
              iv_p_processors_id = 'ea91f8d5f8195c9'  "Processor ID of an OCR parser from Document AI Workbench
              is_input           = ls_input
            IMPORTING
              es_output          = ls_output.
* Read pages to determine Order Date
          READ TABLE ls_output-document-pages INTO DATA(ls_page) INDEX 1.
          IF sy-subrc EQ 0.
            LOOP AT ls_page-form_fields INTO DATA(ls_form_field).
              IF ls_form_field-field_name-text_anchor-content = 'Purchase Order Date:'.
                ls_documentdata-purchaseorderdate = ls_form_field-field_value-text_anchor-content.

              ENDIF.

            ENDLOOP.

          ENDIF.

          DATA: lv_tmp TYPE string.

* Read Delivery Instructions
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
* Call method of Translate API client class to translate delivery instructions to English language
            CALL METHOD translate_delivery_comments
              IMPORTING
                im_source_language = ls_documentdata-deliveryinstructionsoriginalla
              CHANGING
                ch_comments        = ls_documentdata-deliveryinstructions.

          ENDIF.

* Call method to create Sales Order from processed PO data
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

            APPEND ls_output_data TO ex_t_output.
            CLEAR ls_output_data.
          ELSE.
            ls_output_data-file_name = <ls_po_content>-name.
            ls_output_data-ebeln     = ls_documentdata-purchaseordernumber.
            ls_output_data-remarks   = lv_error.

            APPEND ls_output_data TO ex_t_output.
            CLEAR ls_output_data.

          ENDIF.

          CLEAR ls_documentdata.

* Catch SDK exceptions
        CATCH /goog/cx_sdk INTO DATA(lo_exception).

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD parse_purchase_order.

* Structure declarations
    DATA:
      ls_input    TYPE /goog/cl_documentai_v1=>ty_084.

* Local variable declarations
    DATA:
      lv_output     TYPE string,
      lv_project_id TYPE string.

* Reference declarations
    DATA:
          lo_exception    TYPE REF TO /goog/cx_sdk.

* Call function module to encode the PO content in Base64 encoded string
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = im_s_po_content-content
      IMPORTING
        output = lv_output.

    TRY.
        ls_input-raw_document-content   = lv_output.
        ls_input-raw_document-mime_type = im_s_po_content-mime_type.
        lv_project_id = mo_docai_client->gv_project_id.
* Call method of Document AI API client class to get parse PO content
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
            es_err_resp        = DATA(ls_err_resp).
* Cathc SDK exceptions
      CATCH /goog/cx_sdk INTO lo_exception.
        DATA(lv_msg) = lo_exception->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD get_customer_from_address.
    DATA:
          ls_input             TYPE /goog/cl_addrvaldn_v1=>ty_012.

    DATA: lv_stras TYPE kna1-stras,
          lv_ort01 TYPE kna1-ort01,
          lv_pstlz TYPE kna1-pstlz.

    TRY.
        APPEND im_address TO ls_input-address-address_lines.
* Call method of Address Validation API client class to validate address
        CALL METHOD mo_addrvaldn_client->validate_address
          EXPORTING
            is_input    = ls_input
          IMPORTING
            es_output   = DATA(ls_output)
            ev_ret_code = DATA(lv_ret_code)
            ev_err_text = DATA(lv_err_text)
            es_err_resp = DATA(ls_err_resp).
* Prepare address from the API response
        DATA(lv_standardized_address) = ls_output-result-usps_data-standardized_address.
        DATA(lv_postal_address) = ls_output-result-address-postal_address.

        lv_stras = lv_standardized_address-first_address_line.
        lv_ort01 = lv_standardized_address-city.
        lv_pstlz = lv_standardized_address-zip_code.

        CONCATENATE lv_standardized_address-first_address_line
                    lv_standardized_address-city_state_zip_address_line
               INTO ex_formatted_address SEPARATED BY ', '.

* Fetch the SAP customer number for the validated address
        SELECT SINGLE kunnr
          FROM kna1
          INTO ex_customer_number
          WHERE stras = lv_stras AND
                ort01 = lv_ort01 AND
                pstlz = lv_pstlz.
      CATCH /goog/cx_sdk INTO DATA(lo_exception).

    ENDTRY.

  ENDMETHOD.

  METHOD translate_delivery_comments.

    DATA:
      lt_inp    TYPE STANDARD TABLE OF string,
      ls_treq   TYPE /goog/cl_translation_v2=>ty_006,
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
* Call method of Translation API client class to translate Delivery Instructions
        mo_translate_client->translate_translations(
          EXPORTING
            is_input  = ls_treq
          IMPORTING
            es_output = DATA(ls_tresp) ).
        DATA(ls_translated_text) = ls_tresp-data-translations[ 1 ].
        ch_comments = ls_translated_text-translated_text.
      CATCH /goog/cx_sdk INTO lo_cx_sdk.
        "todo: error handle

    ENDTRY.

  ENDMETHOD.

  METHOD detect_comments_language.

* Data declarations
    DATA:
      ls_inp        TYPE /goog/cl_translation_v2=>ty_001,
      ls_detections TYPE /goog/cl_translation_v2=>ty_t_003.

    TRY.
        APPEND im_comments TO ls_inp-q.
* Call method of Translation API client class to detect language of delivery instructions
        mo_translate_client->detect_detections(
          EXPORTING
            is_input    = ls_inp
          IMPORTING
            es_output   = DATA(ls_tresp)
            ev_ret_code = DATA(lv_ret_code)
            ev_err_text = DATA(lv_err_text)
            es_err_resp = DATA(lv_err_resp)
        ).

        ls_detections = ls_tresp-data-detections[ 1 ].
        rv_language = ls_detections[ 1 ]-language.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
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
    DATA: lt_text       TYPE TABLE OF  bapisdtext.

    get_order_header_text( EXPORTING iv_text  = im_documentdata-deliveryinstructions
                                     iv_langu = 'en'
                           CHANGING  ex_t_text  = lt_text ).

    DATA: lw_t002  TYPE t002,
          lv_laiso TYPE t002-laiso.

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
* Call BAPI to create Sales Order
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

* Pass the Text ID of the Header Text (can be different in different SAP systems)
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
    DATA ls_message    TYPE /goog/cl_pubsub_v1=>ty_025.
    DATA lv_project_id TYPE string.
    DATA lv_topic_name TYPE string.

    TRY.
        lv_topic_name = p_topic.
        lv_project_id = mo_pubsub_client->gv_project_id.

        DATA(lv_msg) = | Sales Order { iv_sales_order_number } successfully created |.

        ls_message-data = cl_http_utility=>encode_base64( unencoded = lv_msg ).
        APPEND ls_message TO ls_input-messages.

* Call method of Cloud Pub/Sub API client class to publish order confirmation notification
        mo_pubsub_client->publish_topics( EXPORTING iv_p_projects_id = lv_project_id
                                                    iv_p_topics_id   = lv_topic_name
                                                    is_input         = ls_input
                                          IMPORTING es_output        = DATA(ls_output) ).

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

ENDCLASS.
