"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro framework - (Sub-)Screen</p>
CLASS zcl_ca_scr_fw_screen DEFINITION PUBLIC
                                      CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for screen /dynpro framework</p>
      mo_scr_options                TYPE REF TO zcl_ca_c_scr_fw           READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Parent screen/view of this view</p>
      mo_parent                     TYPE REF TO zcl_ca_scr_fw_screen      READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro fw - (Sub-)Screen controller</p>
      mo_screen_ctlr                TYPE REF TO zcl_ca_scr_fw_screen_ctlr READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Detailed cursor information</p>
      ms_cursor                     TYPE zca_s_scr_fw_cursor_pos_dynpro READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">View type (use const C_VIEW_TYPE_*)</p>
      mv_view_type                  TYPE char1             READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">X = Screen is active</p>
      mv_active                     TYPE abap_bool         READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current mode: Display or modify</p>
      mv_mode                       TYPE syst_ucomm        READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">X = First PBO is already performed</p>
      mv_is_first_pbo               TYPE abap_boolean     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Name of (sub-)screen</p>
      mv_screen_name                TYPE zca_d_screen_name READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Name of dialog (main screen incl. composites and subscreens)</p>
      mv_dialog_name                TYPE zca_d_dialog_name READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Program name to current screen number</p>
      mv_repid                      TYPE syrepid           READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current screen number</p>
      mv_dynnr                      TYPE syst_dynnr        READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Actual function code</p>
      mv_fcode                      TYPE syst_ucomm        READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Close (sub-)screen</p>
      close,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter io_screen_ctlr | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro fw - (Sub-)Screen controller</p>
      "! @parameter iv_repid       | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      "! @parameter iv_dynnr       | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      "! @parameter iv_screen_name | <p class="shorttext synchronized" lang="en">Name of (sub-)screen</p>
      constructor
        IMPORTING
          io_screen_ctlr TYPE REF TO zcl_ca_scr_fw_screen_ctlr
          iv_repid       TYPE syrepid
          iv_dynnr       TYPE syst_dynnr
          iv_screen_name TYPE zca_d_screen_name OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Handle screen event</p>
      "!
      "! @parameter iv_event | <p class="shorttext synchronized" lang="en">Screen event</p>
      on_event
        IMPORTING
          iv_event TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Trigger function code REFRESH</p>
      refresh,

      "! <p class="shorttext synchronized" lang="en">Set screen (in-)active</p>
      "!
      "! @parameter iv_active | <p class="shorttext synchronized" lang="en">X = Activate screen</p>
      set_active
        IMPORTING
          iv_active TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Set cursor information</p>
      "!
      "! @parameter is_cursor | <p class="shorttext synchronized" lang="en">Detailed cursor information</p>
      set_cursor
        IMPORTING
          is_cursor TYPE zca_s_scr_fw_cursor_pos_dynpro,

      "! <p class="shorttext synchronized" lang="en">Set dialog name</p>
      "!
      "! @parameter iv_dialog_name | <p class="shorttext synchronized" lang="en">Dialog name (e. g. helpful for screen control)</p>
      set_dialog_name
        IMPORTING
          iv_dialog_name TYPE zca_d_dialog_name,

      "! <p class="shorttext synchronized" lang="en">Set screen number to replace a screen</p>
      "!
      "! @parameter iv_dynnr | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      set_dynnr
        IMPORTING
          iv_dynnr TYPE syst_dynnr,

      "! <p class="shorttext synchronized" lang="en">Set function code</p>
      "!
      "! @parameter iv_fcode | <p class="shorttext synchronized" lang="en">Function code</p>
      set_fcode
        IMPORTING
          iv_fcode TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Set flag for first PBO</p>
      "!
      "! @parameter iv_first_pbo | <p class="shorttext synchronized" lang="en">Function code</p>
      set_first_pbo
        IMPORTING
          iv_first_pbo TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Set screen mode / change between display and modify</p>
      "!
      "! @parameter iv_mode | <p class="shorttext synchronized" lang="en">Screen mode (use C_MODE_*)</p>
      set_mode
        IMPORTING
          iv_mode TYPE syst_ucomm OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set screen name</p>
      "!
      "! @parameter iv_scr_name | <p class="shorttext synchronized" lang="en">Screen name</p>
      set_screen_name
        IMPORTING
          iv_scr_name TYPE zca_d_screen_name,

      "! <p class="shorttext synchronized" lang="en">Set parent screen instance</p>
      "!
      "! @parameter io_parent | <p class="shorttext synchronized" lang="en">Parent screen instance</p>
      set_parent
        IMPORTING
          io_parent TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Set screen properties</p>
      "!
      "! @parameter iv_repid       | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      "! @parameter iv_dynnr       | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      "! @parameter iv_screen_name | <p class="shorttext synchronized" lang="en">Screen name</p>
      "! @parameter iv_mode        | <p class="shorttext synchronized" lang="en">Screen mode (use C_MODE_*)</p>
      set_properties
        IMPORTING
          iv_repid       TYPE syrepid    OPTIONAL
          iv_dynnr       TYPE syst_dynnr OPTIONAL
          iv_screen_name TYPE zca_d_screen_name OPTIONAL
          iv_mode        TYPE syst_ucomm OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set program name for current screen</p>
      "!
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      set_repid
        IMPORTING
          iv_repid TYPE syrepid,

      "! <p class="shorttext synchronized" lang="en">Set view type</p>
      "!
      "! @parameter iv_view_type | <p class="shorttext synchronized" lang="en">View type</p>
      set_view_type
        IMPORTING
          iv_view_type TYPE char1.

*   i n s t a n c e   e v e n t s
    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Screen is closing</p>
      closed,

      "! <p class="shorttext synchronized" lang="en">Trigger screen event</p>
      "!
      "! @parameter iv_event | <p class="shorttext synchronized" lang="en">Screen event</p>
      process_event
        EXPORTING
          VALUE(iv_event) TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Trigger function code</p>
      "!
      "! @parameter iv_fcode | <p class="shorttext synchronized" lang="en">Function code</p>
      process_fcode
        EXPORTING
          VALUE(iv_fcode) TYPE syst_ucomm.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Register screen in framework</p>
      register.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_scr_fw_screen IMPLEMENTATION.

  METHOD close.
    "-----------------------------------------------------------------*
    "   Close (sub-)screen
    "-----------------------------------------------------------------*
    zcl_ca_scr_fw_ctlr=>get_instance( )->unregister_view( me ).

    RAISE EVENT closed.

    FREE: mo_parent,
          mo_screen_ctlr,
          ms_cursor,
          mv_active,
          mv_repid,
          mv_dynnr,
          mv_mode,
          mv_is_first_pbo,
          mv_screen_name,
          mv_dialog_name,
          mv_fcode,
          mv_view_type.
  ENDMETHOD.                    "close


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    mo_scr_options = zcl_ca_c_scr_fw=>get_instance( ).

    mo_screen_ctlr  = io_screen_ctlr.
    mv_repid        = iv_repid.
    mv_dynnr        = iv_dynnr.
    mv_screen_name  = iv_screen_name.
    mv_is_first_pbo = abap_true.

    set_mode( ).

    register( ).
  ENDMETHOD.                    "constructor


  METHOD on_event.
    "-----------------------------------------------------------------*
    "   Handle screen event
    "-----------------------------------------------------------------*
    DATA(lo_fw) = zcl_ca_scr_fw_ctlr=>get_instance( ).

    DATA(lv_fcode) = lo_fw->mv_fcode.
    IF lv_fcode EQ mo_scr_options->fcodes-refresh AND
       iv_event EQ mo_scr_options->event-pbo.
      lo_fw->set_fcode( space ).
      lv_fcode = space.
    ENDIF.

    CASE iv_event.
      WHEN mo_scr_options->event-fcode.
        RAISE EVENT process_fcode
          EXPORTING iv_fcode = lv_fcode.

      WHEN OTHERS.
        RAISE EVENT process_event
          EXPORTING iv_event = iv_event.
    ENDCASE.

    lv_fcode = lo_fw->mv_fcode.
    IF lv_fcode EQ mo_scr_options->fcodes-refresh AND
       iv_event EQ mo_scr_options->event-pbo.
      lo_fw->set_fcode( space ).
    ENDIF.
  ENDMETHOD.                    "on_event


  METHOD refresh.
    "-----------------------------------------------------------------*
    "   Trigger function code REFRESH
    "-----------------------------------------------------------------*
    IF mv_active EQ abap_false.
      RETURN.
    ENDIF.

    zcl_ca_scr_fw_ctlr=>get_instance( )->set_fcode( mo_scr_options->fcodes-refresh ).
    cl_gui_cfw=>set_new_ok_code( mo_scr_options->fcodes-refresh ).
  ENDMETHOD.                    "refresh


  METHOD register.
    "-----------------------------------------------------------------*
    "   Register screen in framework
    "-----------------------------------------------------------------*
    zcl_ca_scr_fw_ctlr=>get_instance( )->register_view( me ).
  ENDMETHOD.                    "register


  METHOD set_active.
    "-----------------------------------------------------------------*
    "   Set screen (in-)active
    "-----------------------------------------------------------------*
    mv_active = iv_active.
  ENDMETHOD.                    "set_active


  METHOD set_cursor.
    "-----------------------------------------------------------------*
    "   Set cursor information
    "-----------------------------------------------------------------*
    ms_cursor = is_cursor.
  ENDMETHOD.                    "set_cursor


  METHOD set_dialog_name.
    "-----------------------------------------------------------------*
    "   Set dialog name
    "-----------------------------------------------------------------*
    mv_dialog_name = iv_dialog_name.
  ENDMETHOD.                    "set_dialog_name


  METHOD set_dynnr.
    "-----------------------------------------------------------------*
    "   Set screen number to replace a screen
    "-----------------------------------------------------------------*
    DATA(lo_fw) = zcl_ca_scr_fw_ctlr=>get_instance( ).
    mv_dynnr = iv_dynnr.
    lo_fw->unregister_view( me ).
    lo_fw->register_view( me ).
  ENDMETHOD.                    "set_dynnr


  METHOD set_fcode.
    "-----------------------------------------------------------------*
    "   Set function code
    "-----------------------------------------------------------------*
    zcl_ca_scr_fw_ctlr=>get_instance( )->set_fcode( iv_fcode ).
    mv_fcode = iv_fcode.
  ENDMETHOD.                    "set_fcode


  METHOD set_first_pbo.
    "-----------------------------------------------------------------*
    "   Set flag 'FIRST_TIME' for PBO
    "-----------------------------------------------------------------*
    mv_is_first_pbo = iv_first_pbo.
  ENDMETHOD.                    "set_first_pbo


  METHOD set_mode.
    "-----------------------------------------------------------------*
    "   Set screen mode / change between display and modify
    "-----------------------------------------------------------------*
    CASE mv_mode.
      WHEN mo_scr_options->mode-display OR
           mo_scr_options->mode-modify.
        mv_mode = iv_mode.

      WHEN OTHERS.
        mv_mode = mo_scr_options->mode-display.
    ENDCASE.
  ENDMETHOD.                    "set_mode


  METHOD set_screen_name.
    "-----------------------------------------------------------------*
    "   Set screen name
    "-----------------------------------------------------------------*
    mv_screen_name = iv_scr_name.
  ENDMETHOD.                    "set_scr_name


  METHOD set_parent.
    "-----------------------------------------------------------------*
    "   Set parent screen instance
    "-----------------------------------------------------------------*
    IF mo_parent IS NOT BOUND.
      mo_parent      = io_parent.
      mv_dialog_name = mo_parent->mv_dialog_name.
    ENDIF.
  ENDMETHOD.                    "set_parent


  METHOD set_properties.
    "-----------------------------------------------------------------*
    "   Set screen properties
    "-----------------------------------------------------------------*
    IF iv_repid IS SUPPLIED.
      mv_repid = iv_repid.
    ENDIF.

    IF iv_dynnr IS SUPPLIED.
      mv_dynnr = iv_dynnr.
    ENDIF.

    IF iv_screen_name IS SUPPLIED.
      set_screen_name( iv_screen_name ).
    ENDIF.

    IF iv_mode IS SUPPLIED.
      set_mode( iv_mode ).
    ENDIF.

    DATA(lo_fw) = zcl_ca_scr_fw_ctlr=>get_instance( ).
    lo_fw->unregister_view( me ).
    lo_fw->register_view( me ).
  ENDMETHOD.                    "set_properties


  METHOD set_repid.
    "-----------------------------------------------------------------*
    "   Set program name for current screen
    "-----------------------------------------------------------------*
    DATA(lo_fw) = zcl_ca_scr_fw_ctlr=>get_instance( ).
    mv_repid = iv_repid.
    lo_fw->unregister_view( me ).
    lo_fw->register_view( me ).
  ENDMETHOD.                    "set_repid


  METHOD set_view_type.
    "-----------------------------------------------------------------*
    "   Set view type
    "-----------------------------------------------------------------*
    mv_view_type = iv_view_type.
  ENDMETHOD.                    "set_view_type

ENDCLASS.
