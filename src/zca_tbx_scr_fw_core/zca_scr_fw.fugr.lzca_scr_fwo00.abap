*&---------------------------------------------------------------------*
*&  Include           LZCA_SCR_FWO00
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  d0000_pbo_set_status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pbo_set_status OUTPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_handle_event( iv_repid = sy-repid
                                           iv_event = zcl_ca_c_scr_fw=>event-pfstatus ).
ENDMODULE.                 "d0000_pbo_set_status  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  d0000_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pbo OUTPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_handle_event( iv_repid = sy-repid
                                           iv_event = zcl_ca_c_scr_fw=>event-pbo ).
ENDMODULE.                 "d0000_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  d0000_pbo_get_subscreen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pbo_get_subscreen OUTPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_get_subscreen( sy-repid ).
ENDMODULE.                 "d0000_pbo_get_subscreen  OUTPUT
