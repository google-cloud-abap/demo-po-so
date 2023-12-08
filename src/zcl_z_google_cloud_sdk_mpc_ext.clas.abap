class zcl_z_google_cloud_sdk_mpc_ext definition
  public
  inheriting from zcl_z_google_cloud_sdk_mpc
  create public .

  public section.

    types:
      begin of ty_documentdata_deep_entity,
        documentname                   type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-documentname,
        purchaseordernumber            type  zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-purchaseordernumber,
        purchaseorderdate              type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-purchaseorderdate,
        requesteddeliverydate          type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-requesteddeliverydate,
        soldtonumber                   type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-soldtonumber,
        soldtoname                     type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-soldtoname,
        soldtoaddress                  type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-soldtoaddress,
        shiptonumber                   type  zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-shiptonumber,
        shiptoname                     type  zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-shiptoname,
        shiptoaddress                  type  zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-shiptoaddress,
        deliveryinstructions           type  zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-deliveryinstructions,
        deliveryinstructionsoriginal   type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-deliveryinstructionsoriginal,
        deliveryinstructionsoriginalla type zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-deliveryinstructionsoriginalla,
        salesordernumber               type  zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdata-salesordernumber,
        documentdataitemset            type table of zcl_z_google_cloud_sdk_mpc_ext=>ts_documentdataitem with default key,
      end of ty_documentdata_deep_entity .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_GOOGLE_CLOUD_SDK_MPC_EXT IMPLEMENTATION.
ENDCLASS.
