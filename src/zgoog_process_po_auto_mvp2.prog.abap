*&---------------------------------------------------------------------*
*& Report ZGOOG_PROCESS_PO_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_process_po_auto_mvp2.

INCLUDE zgoog_process_po_mvp2_def.
INCLUDE zgoog_process_po_mvp2_sel.
INCLUDE zgoog_process_po_mvp2_impl.

START-OF-SELECTION.
  CREATE OBJECT go_process_po_auto.
  go_process_po_auto->execute( ).

END-OF-SELECTION.
  go_process_po_auto->close_connection( ).
