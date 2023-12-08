*&---------------------------------------------------------------------*
*& Include          ZGOOG_PROCESS_PO_AUTO_SEL
*&---------------------------------------------------------------------*

DATA:
      go_process_po_auto TYPE REF TO lcl_main.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
    p_procid TYPE string OBLIGATORY LOWER CASE,
    p_loca   TYPE string OBLIGATORY LOWER CASE,
    p_bucket TYPE string OBLIGATORY LOWER CASE,
    p_prefix TYPE string OBLIGATORY LOWER CASE,
    p_topic  TYPE string OBLIGATORY LOWER CASE.

SELECTION-SCREEN END OF BLOCK b1.
