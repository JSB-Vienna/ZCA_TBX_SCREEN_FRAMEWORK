*&---------------------------------------------------------------------*
*&  Include           LZBC_SCREEN_FRAMEWORKI00
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  d0000_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pai INPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_handle_event( iv_repid = sy-repid
                                           iv_event = zcl_ca_c_scr_fw=>event-pai ).
ENDMODULE.                 "d0000_pai  INPUT

*&---------------------------------------------------------------------*
*&      Module  d0000_pai_fcode  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pai_fcode INPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_handle_event( iv_repid = sy-repid
                                           iv_event = zcl_ca_c_scr_fw=>event-fcode ).
ENDMODULE.                 "d0000_pai_fcode  INPUT

*&---------------------------------------------------------------------*
*&      Module  d0000_pai_get_cursor  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pai_get_cursor INPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_get_cursor( sy-repid ).
ENDMODULE.                 "d0000_pai_get_cursor  INPUT

*&---------------------------------------------------------------------*
*&      Module  d0000_pov  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0000_pov INPUT.
  zcl_ca_scr_fw_ctlr=>dynpro_handle_event( iv_repid = sy-repid
                                           iv_event = zcl_ca_c_scr_fw=>event-pov ).
ENDMODULE.                 "d0000_pov  INPUT
