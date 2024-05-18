# Purchase Order to Sales Order Demo
[ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/overview) is a Google Cloud supported product that brings the power of Google Cloud to SAP developers in the programming language of their choice - ABAP. The SDK is available as a set of client libraries in the form of ABAP classes. Using these classes, ABAP developers can connect to and use Google Cloud APIs. To run this demo, please [download the latest version](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/install-config#abap-sdk-install) of ABAP SDK for Google Cloud as a pre-requisite step.

This GitHub repository holds demo artifacts to showcase capabilities of ABAP SDK to invoke Google Cloud AI APIs to solve a common business problem for SAP customers, which is to create a Sales Order from a Purchase Order coming in physical documents or PDF files. In this demo, ABAP SDK would be used to invoke below APIs.
1. [Google Cloud Storage](https://cloud.google.com/storage?hl=en) (to store Purchase Orders in PDF format)
2. [Google Cloud Document AI](https://cloud.google.com/document-ai?hl=en) (to parse the PDFs into structured content)
3. [Address Validation](https://mapsplatform.google.com/maps-products/address-validation/?utm_source=Website&utm_medium=Blog&utm_campaign=Maps-global-demandgen-website-cs-GMP_UTM_Campaign_Blog&utm_content=address_validation_ga) (Maps platform API to validate and correct the Sold-to Party and Ship-to Party addresses in the PDFs)
4. [Translations API](https://cloud.google.com/translate?hl=en) (to translate the delivery instructions read from the PO from other languages to English)
5. [Cloud Pub/Sub API](https://cloud.google.com/pubsub?hl=en) (to send event based notifications)

**For more details on the use case and solution, please refer to our blog - [Link](https://medium.com/google-cloud/transforming-sap-use-cases-with-googles-vertex-ai-from-within-abap-5911713ed62d).**

**You'll get below SAP development objects once you install the demo.**
1. A Fiori application to,
*  Preview the PO PDF
*  Scan the PDF
*  Show corrected addresses
*  Translate delivery instructions to English
*  Create a Sales Order
*  Send an order confirmation message to Cloud Pub/Sub
2. A configuration table to configure Google Cloud artifacts to drive the demo.
3. An SAP automation program that can be scheduled in a background job to automate above steps.
4. An SAP automation program that can be scheduled in a background job to automate above steps plus also take into account POs coming in as email attachments in Gmail.

**Later in this Readme you would also find how to setup your Google Cloud and SAP to be able to run this demo end to end.**

**Steps to run this demo end to end after installation can be referenced from the YouTube video - [Link](https://www.youtube.com/watch?v=DLPJdrh-pn8&list=PLbUEAA0lFNT43Yy87JnZ3Q0WuxUCIV7XN&index=2)**

Below is a high level architecture of the demo flow.
<img width="1503" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/df91707c-3438-48e7-8aa9-5fc1164983df">

## Steps to setup the demo
### Setup ABAP Git
1. Create a SE38 program in the SAP system by copy pasting code from the link [here](https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap).
2. Save and activate.
3. Import the GitHub repo through ABAP Git.
<img width="928" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/b5cfd614-1b3e-4ed5-83d0-a9d38f0ab983">
<img width="691" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/d9dc1ef3-f93d-400a-ab8b-b51dfbb62415">

### Setup SICF node for the Fiori app to showcase the demo
1. Activate following SICF nodes
  * /sap/bc/ui2/start_up
  * /sap/bc/ui2/nwbc/
  * /sap/bc/ui5_ui5/ui2/ushell
  * /sap/bc/ui5_ui5/sap/arsrvc_upb_admn
  * /sap/bc/ui5_ui5/sap/ar_srvc_news
  * /sap/public/bc/ui5_ui5/
  * /sap/public/bc/ui2/
  * /sap/public/bc/icf/logoff
  * /sap/bc/ui2/flp
  * /sap/public/bc/ur
2. Add host entry to host file.
3. Activate SAP Gateway - [Follow Link](https://help.sap.com/doc/saphelp_nw74/7.4.16/en-US/db/f62651c294256ee10000000a445394/content.htm?no_cache=true).

### Setup Google Cloud artifacts
Enable Cloud Storage, Document AI, Cloud Translation, Address Validation and Cloud Pub/Sub APIs for the GCP project.

#### Setup Google Cloud Document AI
1. Create an Invoice Processor and copy the processor id.
  <img width="839" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/988ae674-1f8a-4956-b99a-3d4b6f08ed7c">
2. Create an OCR Process and copy the processor id.
  <img width="842" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/62a93eda-0cee-4997-b180-e7d1f13328cd">

#### Setup Google Cloud Storage
1. Create Cloud Storage folders with POs in the GCP project with folder hierarchy as in screenshot below.
   <img width="1138" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/39750f1c-f19e-49ae-9c12-ff7a26f2ef18">
2. Upload the PO files from the folder here.

#### Setup Google Cloud Pub/Sub
1. Create a Topic for sending “Order Confirmation”.
  <img width="1017" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/032d32e2-4840-4306-bfad-2b0121d1d5c9">
2. Create a Subscription for the topic.
  <img width="995" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/0f038469-b214-4dff-a3bf-691476f526e7">

### Setup SAP artifacts for the demo
#### ABAP SDK Client Key setup
Create a Client Key for the setup as below in table /GOOG/CLIENT_KEY, give the Service Account permissions for accessing the Cloud Storage, Document AI, Cloud Translation, Address Validation and Cloud Pub/Sub APIs.
<img width="1245" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/255ae906-ce88-4a0f-afb2-a6fc60753432">

#### Configuration table setup for the demo
1. Maintain table “ZGSDKPOV1_CONFIG” in the SAP system with the Client Key, Document AI Processor IDs, Cloud Storage folder and Cloud Pub/Sub topic through TCode SE16.
  <img width="773" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/b96cad86-7960-4515-9035-de634ab33107">
2. Add Customizing system alias in TCode “/IWFND/MAINT_SERVICES” for the ODATA service.
  <img width="1246" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/f0eab926-940d-4b66-a9ca-c970cee90fef">
  <img width="1247" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/9896e90c-38fe-4260-9035-fb18d3ead1ca">

#### Activate SICF nodes
1. For the ODATA service - sap/opu/odata/sap/Z_GOOGLE_CLOUD_SDK_DEMO_PO_V1_SRV.
  <img width="884" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/03e56cf3-2085-4b13-82a0-725c4865c13a">
2. For the Fiori app - sap/bc/ui5_ui5/sap/zgcpabapsdkdemo.
  <img width="795" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/bb04f563-7e30-46cd-a4dd-bfb74b243e2a">

#### Master Data Setup in SAP
Create Sold-to Party and Ship-to Party with Sales Area Data through TCode - BP, choose BP Role as "Sold-To Party", and maintain General Data and Sales and Distribution Data. Enter below details.

**Sold-to Party -**
Name - Model Company XYZ
Street - 100 BOUNDRY RD
Postal Code - 70363
City - HOUMA
Country - US
Region - LA
<img width="926" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/10f70883-8658-450e-8c2f-679e899a269e">
<img width="741" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/990016cc-9218-4559-a8f9-cc4f975a453f">
<img width="641" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/80e9db96-0427-4608-ad1c-689244440eb9">
<img width="665" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/a33a019f-3dcc-4f8e-8f3c-2462c63e475e">
<img width="662" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/83e9e38b-b147-4b52-bc2d-213d62386587

**Ship-to Party -**
Name - Google Cloud for SAP
Street - 1 E 24TH ST
Postal Code - 68847
City - KEARNEY
Country - US
Region - NE
<img width="668" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/bdf9f536-00a8-4106-85b3-88b82131f4ec">
<img width="646" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/8681f2e8-566d-43a8-b363-761e56a59954">
<img width="675" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/67a5c3d8-4f38-4c5f-ba92-acb5125be874">
<img width="679" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/6bb96e1b-c2f2-4b3b-84dd-fa359653b374">
<img width="682" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/98791ed3-042a-472a-9fc3-da118ffc25e7">

**Materials Setup**
Create below materials (TCode - MM01)
*  TG11 - Traditional Item 11,
*  TG12 - Traditional Item 12, and
*  TG13 - Traditional Item 13
<img width="715" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/5ec2265f-abc5-4d1d-b071-ef2bfdb732c2">
<img width="715" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/b24715b2-6e67-4724-8b68-f8eb430ab271">
<img width="719" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/7cd8601e-79de-4676-82f0-000d9d69db49">

Create TG12 and TG13 with reference of TG11.

**Assign credit segment to control area**
Follow this [link](https://answers.sap.com/questions/8640766/no-credit-segment-assigned-to-credit-control-area.html) to assign credit segment to the control area.

**Fiori app setup**
The URL to access the Fiori application would be - https://<ICM Host Name>/sap/bc/ui5_ui5/sap/zgcpabapsdkdemo/index.html?sap-client=<SAP Client>

Take the ICM Host name from TCode SMICM from under HTTPS line item.
<img width="937" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/db927416-849a-4bc6-9651-5b7e0a1f2c42">

The path and the service name is from the activated SICF node.
<img width="919" alt="image" src="https://github.com/google-cloud-abap/demo-po-so/assets/141922013/62b77b85-1dba-4cce-b796-1f41de9b3035">

Voilà....your demo is setup and ready to use.

**Please write to us in case you face any issues through our [Community Channel](https://www.googlecloudcommunity.com/gc/forums/filteredbylabelpage/board-id/cloud-developer-tools/label-name/abap%20sdk) by starting a conversation and we would be happy to respond.**



























 
