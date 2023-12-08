*&---------------------------------------------------------------------*
*& Report ZGOOG_PROCESS_PO_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_process_po_auto.

INCLUDE zgoog_process_po_auto_def.
INCLUDE zgoog_process_po_auto_sel.
INCLUDE zgoog_process_po_auto_impl.

START-OF-SELECTION.
  DATA(go_process_po_auto) = NEW lcl_main(  ).
  go_process_po_auto->execute( ).

END-OF-SELECTION.
  go_process_po_auto->close_connection( ).
