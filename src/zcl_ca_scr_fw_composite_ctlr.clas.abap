"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro fw - Composite scr controller</p>
CLASS zcl_ca_scr_fw_composite_ctlr DEFINITION PUBLIC
                                              INHERITING FROM zcl_ca_scr_fw_screen_ctlr
                                              CREATE PUBLIC
                                              ABSTRACT.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      create_screen REDEFINITION.
ENDCLASS.



CLASS ZCL_CA_SCR_FW_COMPOSITE_CTLR IMPLEMENTATION.


  METHOD create_screen.
    "-----------------------------------------------------------------*
    "   Create screen instance
    "-----------------------------------------------------------------*
    IF mo_screen IS NOT BOUND.
      mo_screen = NEW zcl_ca_scr_fw_composite( io_screen_ctlr = me
                                               iv_repid       = mv_repid
                                               iv_dynnr       = mv_dynnr
                                               iv_screen_name = mv_screen_name ).

      SET HANDLER: on_process_event FOR mo_screen,
                   on_process_fcode FOR mo_screen,
                   on_closed        FOR mo_screen.
    ENDIF.
  ENDMETHOD.                    "create_screen
ENDCLASS.
