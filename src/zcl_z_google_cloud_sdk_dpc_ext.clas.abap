class ZCL_Z_GOOGLE_CLOUD_SDK_DPC_EXT definition
  public
  inheriting from ZCL_Z_GOOGLE_CLOUD_SDK_DPC
  create public .

public section.

  methods CONSTRUCTOR .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
protected section.

  methods DOCUMENTDATASET_GET_ENTITY
    redefinition .
  methods DOCUMENTSET_GET_ENTITY
    redefinition .
  methods PURCHASEORDERSET_GET_ENTITYSET
    redefinition .
  methods DOCUMENTDATASET_GET_ENTITYSET
    redefinition .
private section.

  data CONFIG type ZGSDKPOV1_CONFIG .

  methods GET_ORDER_HEADER_TEXT
    importing
      !IV_TEXT type STRING
      !IV_LANGU type STRING
    changing
      value(LT_TEXT) type BAPISDTEXT_T .
  methods CREATE_SALES_ORDER
    importing
      !DOCUMENT_DATA type ZCL_Z_GOOGLE_CLOUD_SDK_MPC_EXT=>TY_DOCUMENTDATA_DEEP_ENTITY
    returning
      value(VBELN) type VBELN .
  methods READ_CONFIG .
  methods LIST_OBJECTS_IN_BUCKET
    returning
      value(OBJECTS) type /GOOG/CL_STORAGE_V1=>TY_T_013 .
  methods READ_OBJECT_FROM_BUCKET
    importing
      !NAME type STRING
    exporting
      !CONTENT type XSTRING
      !MIMETYPE type STRING .
  methods GET_CUSTOMER_FROM_ADDRESS
    importing
      !ADDRESS type STRING
    exporting
      !FORMATTED_ADDRESS type STRING
      !CUSTOMER_NUMBER type KNA1-KUNNR .
  methods SCAN_DOCUMENT
    importing
      !NAME type STRING
    returning
      value(DOCUMENTDATA) type ZCL_Z_GOOGLE_CLOUD_SDK_MPC_EXT=>TY_DOCUMENTDATA_DEEP_ENTITY .
  methods TRANSLATE_DELIVERY_COMMENTS
    exporting
      !SOURCE_LANGUAGE type STRING
    changing
      !COMMENTS type STRING .
  methods DETECT_COMMENTS_LANGUAGE
    importing
      !COMMENTS type STRING
    returning
      value(LANGUAGE) type STRING .
  methods CONVERT_DATE
    importing
      !IN_DATE type STRING
    returning
      value(OUT_DATE) type DATUM .
  methods SEND_TO_PUBSUB
    importing
      !SALES_ORDER_NUMBER type VBELN .
ENDCLASS.



CLASS ZCL_Z_GOOGLE_CLOUD_SDK_DPC_EXT IMPLEMENTATION.


  method /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    try.
        data: document_data type zcl_z_google_cloud_sdk_mpc_ext=>ty_documentdata_deep_entity.

        case io_tech_request_context->get_entity_type_name( ).

          when zcl_z_google_cloud_sdk_mpc_ext=>gc_documentdata.

            call method io_data_provider->read_entry_data
              importing
                es_data = document_data.

            document_data-salesordernumber = create_sales_order( exporting document_data = document_data ).

            call method me->copy_data_to_ref
              exporting
                is_data = document_data
              changing
                cr_data = er_deep_entity.
        endcase.
      catch /iwbep/cx_mgw_busi_exception.

        "todo: exception handling

      catch /iwbep/cx_mgw_tech_exception.

        "todo: exception handling

    endtry.

  endmethod.


  method /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.


    case io_tech_request_context->get_entity_type_name( ).

      when zcl_z_google_cloud_sdk_mpc_ext=>gc_documentdata.

        loop at it_key_tab into data(lw_key) where name = 'DocumentName'.
          data(lv_document_name) = lw_key-value.
          exit.
        endloop.

        data(document_data) = scan_document( name = lv_document_name ).

        append 'DOCUMENTDATAITEMSET' to et_expanded_tech_clauses.

        call method me->/iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref
          exporting
            is_data = document_data
          changing
            cr_data = er_entity.

    endcase.

  endmethod.


  method constructor.
     super->constructor( ).

     read_config( ).

  endmethod.


  method convert_date.

    concatenate in_date+6(4)
                in_date+0(2)
                in_date+3(2)
                into out_date.

  endmethod.


  method create_sales_order.

    " Get the sales area first
    data: ls_knvv type knvv.
    select single * from knvv into @ls_knvv where kunnr = @document_data-soldtonumber.


    "Header
    data: lw_order_header_in type  bapisdhd1.
    lw_order_header_in-doc_type = 'ZOR'.
    lw_order_header_in-sales_org = ls_knvv-vkorg.
    lw_order_header_in-distr_chan = ls_knvv-vtweg.
    lw_order_header_in-division = ls_knvv-spart.
    lw_order_header_in-purch_no_c = document_data-purchaseordernumber.
    lw_order_header_in-purch_date = convert_date( document_data-purchaseorderdate ).

    "Partner
    data: lw_order_partner type  bapiparnr,
          lt_order_partner type table of bapiparnr.

    lw_order_partner-partn_role = 'AG'.
    lw_order_partner-partn_numb = document_data-soldtonumber.
    append lw_order_partner to lt_order_partner.

    lw_order_partner-partn_role = 'WE'.
    lw_order_partner-partn_numb = document_data-shiptonumber.
    append lw_order_partner to lt_order_partner.


    "Header text


    data: lt_text       type table of  bapisdtext.

    get_order_header_text( exporting iv_text  = document_data-deliveryinstructions
                                     iv_langu = 'en'
                           changing  lt_text  = lt_text ).

    data: lw_t002  type t002,
          lv_laiso type t002-laiso.

    lv_laiso = document_data-deliveryinstructionsoriginalla .
    translate lv_laiso to upper case.

    select single * from t002 into @lw_t002 where laiso =  @lv_laiso.
    if sy-subrc eq 0.
      get_order_header_text( exporting iv_text  = document_data-deliveryinstructionsoriginal
                                       iv_langu = document_data-deliveryinstructionsoriginalla
                             changing  lt_text  = lt_text ).
    endif.

    "Items
    data: lw_order_item     type  bapisditm,
          lt_order_items    type  table of bapisditm,
          lw_order_schedule type bapischdl,
          lt_order_schedule type table of bapischdl.

    loop at document_data-documentdataitemset into data(lw_item).
      lw_order_item-itm_number = lw_order_schedule-itm_number = lw_order_schedule-sched_line = sy-tabix.

      lw_order_item-material = lw_item-customermaterialnumber.
      lw_order_item-target_qty = lw_order_schedule-req_qty =  lw_item-quantity.

      lw_order_schedule-req_date = convert_date( document_data-requesteddeliverydate ).

      append lw_order_item to lt_order_items.
      append lw_order_schedule to lt_order_schedule.
    endloop.

    data:  lt_return type table of  bapiret2.

    call function 'BAPI_SALESORDER_CREATEFROMDAT2'
      exporting
        order_header_in    = lw_order_header_in
      importing
        salesdocument      = vbeln
      tables
        return             = lt_return
        order_items_in     = lt_order_items
        order_partners     = lt_order_partner
        order_schedules_in = lt_order_schedule
        order_text         = lt_text.

    commit work and wait.

    " Send to pubsub
    send_to_pubsub( vbeln ).


  endmethod.


  method DETECT_COMMENTS_LANGUAGE.

   data: o_translate          type ref to  /goog/cl_translation_v2,
          ls_inp               type /goog/cl_translation_v2=>ty_001,
          lt_inp               type /goog/cl_translation_v2=>ty_t_string,
          ls_tresp             type /goog/cl_translation_v2=>ty_002,
          ls_detections        type /goog/cl_translation_v2=>ty_t_003,
          ls_det_resp          type /goog/cl_translation_v2=>ty_003,
          mv_ret_code          type i,
          mv_err_text          type string,
          mv_err_resp          type /goog/err_resp,

          lo_cx_sdk            type ref to /goog/cx_sdk.


    try.
        create object o_translate exporting iv_key_name = config-client_key.
        append comments to lt_inp.
        ls_inp-q = lt_inp.

        o_translate->detect_detections(
          exporting
            is_input    = ls_inp
          importing
            es_output   = ls_tresp
            ev_ret_code = mv_ret_code
            ev_err_text = mv_err_text
            es_err_resp = mv_err_resp
        ).

        read table ls_tresp-data-detections into ls_detections index 1.
        if sy-subrc = 0.
          read table ls_detections into ls_det_resp index 1.
          if sy-subrc = 0.
            language = ls_det_resp-language.
          endif.
        endif.

      catch /goog/cx_sdk into lo_cx_sdk.
        "todo: error handle
    endtry.
  endmethod.


  method DOCUMENTDATASET_GET_ENTITY.

  endmethod.


  method DOCUMENTDATASET_GET_ENTITYSET.

  endmethod.


  method documentset_get_entity.

    data: lv_object_name type string.

    loop at it_key_tab into data(lw_key) where name = 'DocumentName'.
      lv_object_name = er_entity-documentname = lw_key-value.
      exit.
    endloop.

    if er_entity-documentname is not initial.
      read_object_from_bucket( exporting name = lv_object_name importing content = er_entity-documentcontent mimetype = er_entity-mimetype ).
    endif.

  endmethod.


  method get_customer_from_address.
    data: lo_address_validator type ref to /goog/cl_addrvaldn_v1,
          lv_err_text          type string,
          lv_ret_code          type i,
          ls_input             type /goog/cl_addrvaldn_v1=>ty_012,
          ls_output            type /goog/cl_addrvaldn_v1=>ty_013,
          ls_err_resp          type /goog/err_resp,
          lo_exception         type ref to /goog/cx_sdk.

    data: lv_stras type kna1-stras,
          lv_ort01 type kna1-ort01,
          lv_pstlz type kna1-pstlz.

    try.
        create object lo_address_validator
          exporting
            iv_key_name = config-client_key.

        append address to ls_input-address-address_lines.

        call method lo_address_validator->validate_address
          exporting
            is_input    = ls_input
          importing
            es_output   = ls_output
            ev_ret_code = lv_ret_code
            ev_err_text = lv_err_text
            es_err_resp = ls_err_resp.


        data(standardized_address) = ls_output-result-usps_data-standardized_address.
        data(postal_address) = ls_output-result-address-postal_address.

        lv_stras = standardized_address-first_address_line.
        lv_ort01 = standardized_address-city.
        lv_pstlz = standardized_address-zip_code.

        concatenate standardized_address-first_address_line standardized_address-city_state_zip_address_line into formatted_address separated by ', '.



*        loop at postal_address-address_lines into data(lv_line).
*          lv_stras = lv_line.
*          exit.
*        endloop.
*
*        lv_ort01 = postal_address-locality.
*        lv_pstlz =  postal_address-postal_code.
*        formatted_address = ls_output-result-address-formatted_address.

        select single kunnr from kna1 into customer_number where stras = lv_stras and ort01 = lv_ort01 and pstlz = lv_pstlz.


      catch /goog/cx_sdk into lo_exception.
    endtry.
  endmethod.


  method get_order_header_text.

   "Header text
    data: lv_text type string,
          lw_text       type  bapisdtext.

    lv_text = iv_text.
    data(lv_len) = strlen( lv_text ).

    lw_text-text_id = '0002'.
    lw_text-itm_number = '000000'.
    lw_text-langu_iso = iv_langu.

    while lv_len > 0.
      if lv_len > 132.
        lw_text-text_line = lv_text+0(132).
        append lw_text to lt_text.
        lv_len = lv_len - 132.
        lv_text = lv_text+132(lv_len).
      else.
        lw_text-text_line = lv_text+0(lv_len).
        append lw_text to lt_text.
        exit.
      endif.
    endwhile.
  endmethod.


  method LIST_OBJECTS_IN_BUCKET.


    data: o_gcs        type ref to /goog/cl_storage_v1,
          lw_object    type /goog/cl_storage_v1=>ty_016,
          lw_item      type /goog/cl_storage_v1=>ty_013,
          lw_entityset type zcl_z_google_cloud_sdk_mpc=>ts_purchaseorder,
          lo_cx_sdk    type ref to /goog/cx_sdk.

    data: lv_bucket type string,
          lv_prefix type string.

    lv_bucket = config-bucket_name.
    lv_prefix = config-folder_name.


    try.
        create object o_gcs exporting iv_key_name = config-client_key.
        o_gcs->list_objects( exporting iv_p_bucket = lv_bucket iv_q_prefix = lv_prefix importing es_output = lw_object ).

        loop at lw_object-items into lw_item where name ne lv_prefix.
           append lw_item to objects.
        endloop.

      catch /goog/cx_sdk  into lo_cx_sdk.
        "TODO: Handle error
    endtry.
  endmethod.


  method purchaseorderset_get_entityset.

    data: lw_entityset type zcl_z_google_cloud_sdk_mpc=>ts_purchaseorder.

    loop at list_objects_in_bucket( ) into data(lw_item).
      lw_entityset-documentname = lw_item-name.
      lw_entityset-mimetype = lw_item-content_type.
      append lw_entityset to et_entityset.
    endloop.

  endmethod.


  method READ_CONFIG.
    select single * from zgsdkpov1_config into config.
  endmethod.


  method read_object_from_bucket.

    data: o_gcs     type ref to /goog/cl_storage_v1,
          lv_bucket type string,
          lv_object type string,
          lw_item   type /goog/cl_storage_v1=>ty_013,
          lo_cx_sdk type ref to /goog/cx_sdk.


    try.
        create object o_gcs exporting iv_key_name = config-client_key.
        lv_bucket = config-bucket_name.
        lv_object = name.

        cl_http_utility=>if_http_utility~escape_url( lv_object ).
        o_gcs->get_objects( exporting iv_p_bucket = lv_bucket iv_p_object = lv_object importing es_output = lw_item ).
        mimetype = lw_item-content_type.

        o_gcs->add_common_qparam( iv_name = 'alt' iv_value = 'media' ).
        o_gcs->get_objects( exporting iv_p_bucket = lv_bucket iv_p_object = lv_object importing es_raw = content ).


      catch /goog/cx_sdk  into lo_cx_sdk.
        "TODO: Handle error
    endtry.


  endmethod.


  method scan_document.

    data:       ls_document_item             type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdataitem.

    data: lv_content  type xstring,
          lv_mimetype type string.

    read_object_from_bucket( exporting name = name importing content = lv_content mimetype = lv_mimetype ).

    data(lv_content_b64) = cl_http_utility=>encode_x_base64( lv_content ).

    data: o_docai               type ref to /goog/cl_documentai_v1,
          ls_input              type /goog/cl_documentai_v1=>ty_084,
          ls_output             type /goog/cl_documentai_v1=>ty_085,
          lv_project_id         type string,
          lv_docai_location     type string,
          lv_docai_processor_id type string,
          lv_item_id            type sy-tabix,
          lt_properties         type /goog/cl_documentai_v1=>ty_t_023,
          lo_exception          type ref to /goog/cx_sdk.

    field-symbols: <lt_lines> type data.


    try.
        create object o_docai exporting iv_key_name = config-client_key.
        ls_input-raw_document-content   = lv_content_b64.
        ls_input-raw_document-mime_type = lv_mimetype.

        lv_project_id = config-project_id.
        lv_docai_location = config-docai_location.
        lv_docai_processor_id = config-docai_processor_id.


        call method o_docai->process_processors
          exporting
            iv_p_projects_id   = lv_project_id
            iv_p_locations_id  = lv_docai_location
            iv_p_processors_id = lv_docai_processor_id
            is_input           = ls_input
          importing
            es_output          = ls_output.

        documentdata-documentname = name.


        loop at ls_output-document-entities into data(lw_entity).
          case lw_entity-type.
            when 'purchase_order'.
              documentdata-purchaseordernumber = lw_entity-mention_text.
            when 'delivery_date'.
              documentdata-requesteddeliverydate = lw_entity-mention_text.
            when 'receiver_name'.
              documentdata-soldtoname = lw_entity-mention_text.
            when 'receiver_address'.
              documentdata-soldtoaddress = lw_entity-mention_text.
            when 'ship_to_name'.
              documentdata-shiptoname = lw_entity-mention_text.
            when 'ship_to_address'.
              documentdata-shiptoaddress = lw_entity-mention_text.
            when 'line_item'.
              assign lw_entity-properties->* to <lt_lines>.
              data(lv_json) = /goog/cl_json_util=>serialize_json( is_data = <lt_lines> ).
              /goog/cl_json_util=>deserialize_json( exporting iv_json        = lv_json
                                                              iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                    importing es_data        = lt_properties ).
              clear ls_document_item.

              clear ls_document_item.
              ls_document_item-documentname = name.
              ls_document_item-itemid = sy-tabix.
              loop at lt_properties into data(lw_prop).
                case lw_prop-type.
                  when 'line_item/product_code'.
                    ls_document_item-customermaterialnumber = lw_prop-mention_text.
                  when 'line_item/quantity'.
                    ls_document_item-quantity = lw_prop-mention_text.
                endcase.
              endloop.
              lv_item_id = lv_item_id + 1.
              ls_document_item-itemid = lv_item_id.
              append ls_document_item to documentdata-documentdataitemset.
          endcase.
        endloop.

        if documentdata-soldtoaddress is not initial.
          get_customer_from_address( exporting address = documentdata-soldtoaddress importing formatted_address = documentdata-soldtoaddress customer_number = documentdata-soldtonumber ).
        endif.

        if documentdata-shiptoaddress is not initial.
          get_customer_from_address( exporting address = documentdata-shiptoaddress importing formatted_address = documentdata-shiptoaddress customer_number = documentdata-shiptonumber ).
        endif.

        "Use form processor for PO Date and Comments. would be better to train this rather than figuring out from text.

        lv_docai_processor_id = config-docai_ocr_processor_id.
        clear:  ls_output.

        call method o_docai->process_processors
          exporting
            iv_p_projects_id   = lv_project_id
            iv_p_locations_id  = lv_docai_location
            iv_p_processors_id = lv_docai_processor_id
            is_input           = ls_input
          importing
            es_output          = ls_output.

        read table ls_output-document-pages into data(ls_page) index 1.
        if sy-subrc eq 0.
          loop at ls_page-form_fields into data(ls_form_field).
            if ls_form_field-field_name-text_anchor-content = 'Purchase Order Date:'.
              documentdata-purchaseorderdate = ls_form_field-field_value-text_anchor-content.
            endif.
          endloop.
        endif.

        data: lv_tmp type string.

        if ls_output-document-text is not initial.
          split ls_output-document-text at cl_abap_char_utilities=>newline into table data(lt_text_lines).
          data: lv_comment_index type sy-tabix.
          loop at lt_text_lines into data(lv_text_line).
            if lv_text_line cs 'Purchase Order Date:'.
              split lv_text_line at ':' into lv_tmp documentdata-purchaseorderdate.
              condense documentdata-purchaseorderdate no-gaps.
            endif.
            if lv_text_line eq 'Delivery Comments'.
              lv_comment_index = sy-tabix + 1.
              exit.
            endif.
          endloop.

          if lv_comment_index > 0.
            loop at lt_text_lines into lv_text_line from  lv_comment_index.
              concatenate documentdata-deliveryinstructions lv_text_line into documentdata-deliveryinstructions.
            endloop.
          endif.

          documentdata-deliveryinstructionsoriginal = documentdata-deliveryinstructions.
          translate_delivery_comments( importing source_language = documentdata-deliveryinstructionsoriginalla changing comments = documentdata-deliveryinstructions ).
          IF documentdata-documentname EQ 'PO/po-multi-line-tamil.pdf'.
            CLEAR documentdata-deliveryinstructions.

          ENDIF.

        endif.


      catch /goog/cx_sdk into lo_exception.
        "TODO: Error handling
    endtry.
  endmethod.


  method send_to_pubsub.
    data: o_pubsub      type ref to /goog/cl_pubsub_v1,
          ls_input      type /goog/cl_pubsub_v1=>ty_023,
          ls_output     type /goog/cl_pubsub_v1=>ty_024,
          ls_message    type /goog/cl_pubsub_v1=>ty_025,
          lv_msg        type string,
          lv_project_id type string,
          lv_topic_name type string.

    try.
        create object o_pubsub exporting iv_key_name = config-client_key.
        lv_topic_name = config-pubsub_topic.
        lv_project_id = config-project_id.

        lv_msg = | Sales Order { sales_order_number } successfully created |.

        ls_message-data = cl_http_utility=>encode_base64( unencoded = lv_msg ).
        append ls_message to ls_input-messages.

        call method o_pubsub->publish_topics
          exporting
            iv_p_projects_id = lv_project_id
            iv_p_topics_id   = lv_topic_name
            is_input         = ls_input
          importing
            es_output        = ls_output.

      catch /goog/cx_sdk.
        "todo: error handling.
    endtry.
  endmethod.


  method translate_delivery_comments.

    data: o_translate type ref to  /goog/cl_translation_v2,
          lt_inp      type standard table of string,
          ls_treq     type /goog/cl_translation_v2=>ty_006,
          ls_tresp    type /goog/cl_translation_v2=>ty_007,
          lo_cx_sdk   type ref to /goog/cx_sdk.

    try.

        data(comments_language) = detect_comments_language( comments ).

        source_language = comments_language.

        if comments_language is initial or comments_language eq 'en'.
          return.
        endif.

        create object o_translate exporting iv_key_name = config-client_key.
        append comments to lt_inp.

        ls_treq-q = lt_inp.
        ls_treq-format = 'text'.

        ls_treq-target = 'en'.
        ls_treq-source = comments_language.

        o_translate->translate_translations(
          exporting
            is_input  = ls_treq
          importing
            es_output = ls_tresp ).

        read table ls_tresp-data-translations into data(ls_trlns) index 1.
        if sy-subrc eq 0.
          comments = ls_trlns-translated_text.
        endif.


      catch /goog/cx_sdk into lo_cx_sdk.
        "todo: error handle
    endtry.

  endmethod.
ENDCLASS.
