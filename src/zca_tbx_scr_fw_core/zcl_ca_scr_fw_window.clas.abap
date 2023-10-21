"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro framework - Window / Dynpro</p>
CLASS zcl_ca_scr_fw_window DEFINITION PUBLIC
                                      INHERITING FROM zcl_ca_scr_fw_composite
                                      CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Open screen as popup</p>
      mv_open_as                    TYPE abap_bool  READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current GUI status</p>
      mv_pfstatus                   TYPE syst_pfkey READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Program name to current GUI status</p>
      mv_pfstatus_repid             TYPE syrepid    READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current title bar</p>
      mv_titlebar                   TYPE gui_title  READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Program name to current title bar</p>
      mv_titlebar_repid             TYPE syrepid    READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 1 for current title bar</p>
      mv_titlebar_var1              TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 2 for current title bar</p>
      mv_titlebar_var2              TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 3 for current title bar</p>
      mv_titlebar_var3              TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 4 for current title bar</p>
      mv_titlebar_var4              TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 5 for current title bar</p>
      mv_titlebar_var5              TYPE string     READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter io_screen_ctlr   | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro fw - (Sub-)Screen controller</p>
      "! @parameter iv_repid         | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      "! @parameter iv_dynnr         | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      "! @parameter iv_screen_name   | <p class="shorttext synchronized" lang="en">Name of (sub-)screen</p>
      "! @parameter iv_open_as       | <p class="shorttext synchronized" lang="en">Open as screen or popup -&gt; use ZCL_CA_C_SCR_FW=&gt;OPEN_AS-*</p>
      "! @parameter is_popup_corners | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      constructor
        IMPORTING
          io_screen_ctlr   TYPE REF TO zcl_ca_scr_fw_screen_ctlr
          iv_repid         TYPE syrepid
          iv_dynnr         TYPE syst_dynnr
          iv_screen_name   TYPE zca_d_screen_name OPTIONAL
          iv_open_as       TYPE abap_bool DEFAULT zcl_ca_c_scr_fw=>open_as-screen
          is_popup_corners TYPE zca_s_scr_fw_popup_corners OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Display screen / dynpro</p>
      display,

      "! <p class="shorttext synchronized" lang="en">Set excluded function codes</p>
      "!
      "! @parameter it_fcodes | <p class="shorttext synchronized" lang="en">Excluded function codes</p>
      set_excl_fcode
        IMPORTING
          it_fcodes TYPE zca_tt_excl_fcode,

      "! <p class="shorttext synchronized" lang="en">Set GUI status</p>
      "!
      "! @parameter iv_pfstatus       | <p class="shorttext synchronized" lang="en">Name of GUI status</p>
      "! @parameter iv_pfstatus_repid | <p class="shorttext synchronized" lang="en">Program name to GUI status</p>
      set_pfstatus
        IMPORTING
          iv_pfstatus       TYPE syst_pfkey
          iv_pfstatus_repid TYPE syrepid OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set title bar</p>
      "!
      "! @parameter iv_titlebar       | <p class="shorttext synchronized" lang="en">Name of title bar</p>
      "! @parameter iv_titlebar_repid | <p class="shorttext synchronized" lang="en">Program name to title bar</p>
      "! @parameter iv_titlebar_var1  | <p class="shorttext synchronized" lang="en">Variable 1 for title bar</p>
      "! @parameter iv_titlebar_var2  | <p class="shorttext synchronized" lang="en">Variable 2 for title bar</p>
      "! @parameter iv_titlebar_var3  | <p class="shorttext synchronized" lang="en">Variable 3 for title bar</p>
      "! @parameter iv_titlebar_var4  | <p class="shorttext synchronized" lang="en">Variable 4 for title bar</p>
      "! @parameter iv_titlebar_var5  | <p class="shorttext synchronized" lang="en">Variable 5 for title bar</p>
      set_titlebar
        IMPORTING
          iv_titlebar       TYPE gui_title
          iv_titlebar_repid TYPE syrepid OPTIONAL
          iv_titlebar_var1  TYPE string OPTIONAL
          iv_titlebar_var2  TYPE string OPTIONAL
          iv_titlebar_var3  TYPE string OPTIONAL
          iv_titlebar_var4  TYPE string OPTIONAL
          iv_titlebar_var5  TYPE string OPTIONAL,

      close REDEFINITION,

      on_event REDEFINITION.

*   i n s t a n c e   e v e n t s
    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Trigger call screen</p>
      "!
      "! @parameter iv_dynnr         | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      "! @parameter iv_open_as       | <p class="shorttext synchronized" lang="en">Open as screen or popup -&gt; use ZCL_CA_C_SCR_FW=&gt;OPEN_AS-*</p>
      "! @parameter is_popup_corners | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      call_screen
        EXPORTING
          VALUE(iv_dynnr)         TYPE syst_dynnr
          VALUE(iv_open_as)       TYPE abap_bool
          VALUE(is_popup_corners) TYPE zca_s_scr_fw_popup_corners.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Excluded function codes of current status</p>
      mt_excl_fcode    TYPE zca_tt_excl_fcode,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      ms_popup_corners TYPE zca_s_scr_fw_popup_corners.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Handle event setting GUI status</p>
      on_pfstatus,

      on_fcode REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_scr_fw_window IMPLEMENTATION.

  METHOD close.
    "-----------------------------------------------------------------*
    "   Close (sub-)screen
    "-----------------------------------------------------------------*
    super->close( ).

    FREE: mt_excl_fcode,
          mv_pfstatus,
          mv_pfstatus_repid,
          mv_open_as,
          mv_titlebar,
          mv_titlebar_repid,
          mv_titlebar_var1,
          mv_titlebar_var2,
          mv_titlebar_var3,
          mv_titlebar_var4,
          mv_titlebar_var5,
          ms_popup_corners.

    SET SCREEN 0.
    LEAVE SCREEN.
  ENDMETHOD.                    "close


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( io_screen_ctlr = io_screen_ctlr
                        iv_repid       = iv_repid
                        iv_dynnr       = iv_dynnr
                        iv_screen_name = iv_screen_name ).

    mv_open_as = iv_open_as.

    IF mv_open_as EQ mo_scr_options->open_as-popup.
      ms_popup_corners = is_popup_corners.

      IF ms_popup_corners-starting_at_x IS INITIAL.
        ms_popup_corners-starting_at_x = 25.
      ENDIF.

      IF ms_popup_corners-starting_at_y IS INITIAL.
        ms_popup_corners-starting_at_y = 5.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "constructor


  METHOD display.
    "-----------------------------------------------------------------*
    "   Display screen / dynpro
    "-----------------------------------------------------------------*
    set_active( abap_true ).

    RAISE EVENT call_screen
      EXPORTING
        iv_dynnr         = mv_dynnr
        iv_open_as       = mv_open_as
        is_popup_corners = ms_popup_corners.

    set_active( space ).
  ENDMETHOD.                    "display


  METHOD on_event.
    "-----------------------------------------------------------------*
    "   Handle screen event
    "-----------------------------------------------------------------*
    super->on_event( iv_event ).

    CASE iv_event.
      WHEN mo_scr_options->event-pfstatus.
        on_pfstatus( ).

      WHEN mo_scr_options->event-fcode.
        zcl_ca_scr_fw_ctlr=>get_instance( )->set_fcode( space ).
    ENDCASE.
  ENDMETHOD.                    "on_event


  METHOD on_fcode.
    "-----------------------------------------------------------------*
    "   Handle function code
    "-----------------------------------------------------------------*
    cl_gui_cfw=>dispatch(
                    IMPORTING
                      return_code = DATA(lv_rc) ).
    IF lv_rc EQ cl_gui_cfw=>rc_found   OR
       lv_rc NE cl_gui_cfw=>rc_noevent.
      RETURN.
    ENDIF.

    IF zcl_ca_scr_fw_ctlr=>get_instance( )->mv_fcode EQ mo_scr_options->fcodes-refresh.
      RETURN.
    ENDIF.

    super->on_fcode( ).
  ENDMETHOD.                    "on_fcode


  METHOD on_pfstatus.
    "-----------------------------------------------------------------*
    "   Handle event setting GUI status
    "-----------------------------------------------------------------*
    DATA(lo_fw) = zcl_ca_scr_fw_ctlr=>get_instance( ).

    mv_pfstatus       = lo_fw->mv_pfstatus.
    mv_pfstatus_repid = lo_fw->mv_pfstatus_repid.

    mt_excl_fcode     = lo_fw->mt_excl_fcode.

    mv_titlebar       = lo_fw->mv_titlebar.
    mv_titlebar_repid = lo_fw->mv_titlebar_repid.

    IF mv_pfstatus       IS NOT INITIAL AND
       mv_pfstatus_repid IS NOT INITIAL.
      SET PF-STATUS mv_pfstatus OF PROGRAM mv_pfstatus_repid
                                 EXCLUDING mt_excl_fcode.

      SET TITLEBAR mv_titlebar OF PROGRAM mv_titlebar_repid
                                     WITH mv_titlebar_var1
                                          mv_titlebar_var2
                                          mv_titlebar_var3
                                          mv_titlebar_var4
                                          mv_titlebar_var5.
    ENDIF.
  ENDMETHOD.                    "on_pfstatus


  METHOD set_excl_fcode.
    "-----------------------------------------------------------------*
    "   Set excluded function codes
    "-----------------------------------------------------------------*
    zcl_ca_scr_fw_ctlr=>get_instance( )->set_excl_fcode( it_fcodes ).
    mt_excl_fcode = it_fcodes.
  ENDMETHOD.                    "set_excl_fcode


  METHOD set_pfstatus.
    "-----------------------------------------------------------------*
    "   Set GUI status
    "-----------------------------------------------------------------*
    mv_pfstatus = iv_pfstatus.

    mv_pfstatus_repid = COND #( WHEN iv_pfstatus_repid IS NOT INITIAL
                                  THEN iv_pfstatus_repid
                                  ELSE mv_repid ).

    zcl_ca_scr_fw_ctlr=>get_instance( )->set_pfstatus( iv_pfstatus       = mv_pfstatus
                                                       iv_pfstatus_repid = mv_pfstatus_repid ).
  ENDMETHOD.                    "set_pfstatus


  METHOD set_titlebar.
    "-----------------------------------------------------------------*
    "   Set title bar
    "-----------------------------------------------------------------*
    mv_titlebar = iv_titlebar.

    mv_titlebar_repid = COND #( WHEN iv_titlebar_repid IS NOT INITIAL
                                  THEN iv_titlebar_repid
                                  ELSE mv_repid ).

    mv_titlebar_var1 = iv_titlebar_var1.
    mv_titlebar_var2 = iv_titlebar_var2.
    mv_titlebar_var3 = iv_titlebar_var3.
    mv_titlebar_var4 = iv_titlebar_var4.
    mv_titlebar_var5 = iv_titlebar_var5.

    zcl_ca_scr_fw_ctlr=>get_instance( )->set_titlebar( iv_titlebar       = mv_titlebar
                                                       iv_titlebar_repid = mv_titlebar_repid
                                                       iv_titlebar_var1  = mv_titlebar_var1
                                                       iv_titlebar_var2  = mv_titlebar_var2
                                                       iv_titlebar_var3  = mv_titlebar_var3
                                                       iv_titlebar_var4  = mv_titlebar_var4
                                                       iv_titlebar_var5  = mv_titlebar_var5 ).
  ENDMETHOD.                    "set_titlebar

ENDCLASS.
