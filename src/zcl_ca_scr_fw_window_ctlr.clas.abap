"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro fw - Window controller</p>
CLASS zcl_ca_scr_fw_window_ctlr DEFINITION PUBLIC
                                           INHERITING FROM zcl_ca_scr_fw_composite_ctlr
                                           CREATE PUBLIC
                                           ABSTRACT.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - Window / Dynpro</p>
      mo_window      TYPE REF TO zcl_ca_scr_fw_window READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Publication for Generic Object Services (Manager)</p>
      mo_gos_manager TYPE REF TO cl_gos_manager READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Close screens</p>
      close,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_mode          | <p class="shorttext synchronized" lang="en">Screen mode (use ZCL_CA_C_SCR_FW=&gt;MODE-*)</p>
      "! @parameter iv_dialog_name   | <p class="shorttext synchronized" lang="en">Dialog name (e. g. helpful for screen control)</p>
      "! @parameter iv_open_as       | <p class="shorttext synchronized" lang="en">Open as screen or popup -&gt; use ZCL_CA_C_SCR_FW=&gt;OPEN_AS-*</p>
      "! @parameter is_popup_corners | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      constructor
        IMPORTING
          iv_mode          TYPE syst_ucomm DEFAULT zcl_ca_c_scr_fw=>mode-display
          iv_dialog_name   TYPE zca_d_dialog_name OPTIONAL
          iv_open_as       TYPE abap_bool  DEFAULT zcl_ca_c_scr_fw=>open_as-screen
          is_popup_corners TYPE zca_s_scr_fw_popup_corners OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Display screen</p>
      display,

      "! <p class="shorttext synchronized" lang="en">Get status instance</p>
      "!
      "! @parameter ro_gui_status | <p class="shorttext synchronized" lang="en">Screen / dynpro fw - GUI Status</p>
      get_status
        RETURNING
          VALUE(ro_gui_status) TYPE REF TO zcl_ca_scr_fw_status.

*   i n s t a n c e   e v e n t s
    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Screen were closed</p>
      closed,

      "! <p class="shorttext synchronized" lang="en">Handle function code - use to inform involved objects</p>
      "!
      "! @parameter iv_fcode | <p class="shorttext synchronized" lang="en">Function code</p>
      fcode_triggered
        EXPORTING
          VALUE(iv_fcode) TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Handle status</p>
      "!
      "! @parameter io_gui_status | <p class="shorttext synchronized" lang="en">Screen / dynpro fw - GUI Status</p>
      handle_status
        EXPORTING
          VALUE(io_gui_status) TYPE REF TO zcl_ca_scr_fw_status.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro fw - GUI Status</p>
      mo_gui_status    TYPE REF TO zcl_ca_scr_fw_status,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      ms_popup_corners TYPE zca_s_scr_fw_popup_corners,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Current screen mode</p>
      mv_mode          TYPE syst_ucomm,
      "! <p class="shorttext synchronized" lang="en">Name of dialog (main screen incl. composites and subscreens)</p>
      mv_dialog_name   TYPE zca_d_dialog_name,
      "! <p class="shorttext synchronized" lang="en">X = Open screen as popup</p>
      mv_open_as       TYPE abap_bool.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create GUI status</p>
      create_status,

      "! <p class="shorttext synchronized" lang="en">Call simply the screen - nothing else</p>
      "!
      "! @parameter iv_dynnr         | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      "! @parameter iv_open_as       | <p class="shorttext synchronized" lang="en">Open as screen or popup -&gt; use ZCL_CA_C_SCR_FW=&gt;OPEN_AS-*</p>
      "! @parameter is_popup_corners | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      on_call_screen
        FOR EVENT call_screen OF zcl_ca_scr_fw_window
        IMPORTING
          iv_dynnr
          iv_open_as
          is_popup_corners,

      "! <p class="shorttext synchronized" lang="en">GUI status was set - do depending adjustments here</p>
      "!
      "! @parameter io_gui_status | <p class="shorttext synchronized" lang="en">Description</p>
      on_set_status
        FOR EVENT handle_status OF zcl_ca_scr_fw_window_ctlr
        IMPORTING
          io_gui_status,

      "! <p class="shorttext synchronized" lang="en">Set GUI status</p>
      set_screen_status,

      create_screen REDEFINITION,

      on_closed REDEFINITION,

      on_process_event REDEFINITION,

      on_process_fcode REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Default screen number</p>
      c_def_dynnr       TYPE syst_dynnr   VALUE '0000' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Default program name</p>
      c_def_repid       TYPE syrepid      VALUE 'SAPLZCA_SCR_FW' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Default program name</p>
      c_def_titlebar    TYPE gui_title    VALUE 'TITLE_DEFAULT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Default GUI status for popup</p>
      c_pfstatus_popup  TYPE syst_pfkey   VALUE 'POPUP_DEFAULT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Default GUI status for windows</p>
      c_pfstatus_screen TYPE syst_pfkey   VALUE 'SCREEN_DEFAULT' ##no_text.

ENDCLASS.



CLASS zcl_ca_scr_fw_window_ctlr IMPLEMENTATION.

  METHOD close.
    "-----------------------------------------------------------------*
    "   Close screens
    "-----------------------------------------------------------------*
    IF mo_gos_manager IS BOUND.
      mo_gos_manager->unpublish( ).
      FREE mo_gos_manager.
    ENDIF.

    mo_window->close( ).
  ENDMETHOD.                    "close


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*  USE the constructor of your class to open the controller for
*  specific objects you need there. E. g. the controller instance
*  of another subscreen to interact between them via events.
*
*  The constructor signature/interface can be defined as you like
*  independent what the SUPER->CONSTRUCTOR needs, but it must be
*  called before you can start with your things.
*
*  Furthermore define here function codes that should be ignored for
*  validation checks, e. g. for additional functions. Use therefore
*  method EXCLUDE_FCODE_FOR_VALUE_CHECK to add necessary function
*  codes. To check those functions, typically in method HANDLE_PAI or
*  CHECK_VALUES use method NO_VALUE_CHECK_IS_REQUIRED.
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

    super->constructor( ).

    mv_mode          = iv_mode.
    mv_dialog_name   = iv_dialog_name.

    mv_open_as       = iv_open_as.
    ms_popup_corners = is_popup_corners.

    create_status( ).

    "Set handler for status
    SET HANDLER on_set_status FOR me.
  ENDMETHOD.                    "constructor


  METHOD create_screen.
    "-----------------------------------------------------------------*
    "   Create screen instance
    "-----------------------------------------------------------------*
    IF mo_screen IS NOT BOUND.
      mo_screen = NEW zcl_ca_scr_fw_window( io_screen_ctlr   = me
                                            iv_repid         = mv_repid
                                            iv_dynnr         = mv_dynnr
                                            iv_screen_name   = mv_screen_name
                                            iv_open_as       = mv_open_as
                                            is_popup_corners = ms_popup_corners ).

      mo_screen->set_dialog_name( mv_dialog_name ).
    ENDIF.

    "Cast into WINDOW type to be able to add the following subscreens
    mo_window ?= mo_screen.

    SET HANDLER: on_process_event FOR mo_screen,
                 on_process_fcode FOR mo_screen,
                 on_closed        FOR mo_screen,
                 on_call_screen   FOR mo_window.


    "h a s   t o   b e   r e d i f i n e d   ===>>  BUT ONLY IF YOU USE COMPOSITE- and/or SUB-SCREENS

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

*    "Depending on the interaction between the subscreen you define the instance for subscreen as
*    "an class attribute or local in this redefined method.
*    Subscreen XYZ
*    mo_sscr_xyz = NEW zcl_xx_sscr_xyz( param_a = ?
*                                       param_b = ? ).
*    "Cast to base class to be able to export screen instance
*    DATA(lo_sscr_xyz) = CAST zcl_ca_scr_fw_screen_ctlr( mo_sscr_xyz ).
*    lo_sscr_xyz->create_screen( ).
*    mo_window->add_view( io_view = lo_sscr_xyz->mo_screen ).
*
*    Subscreen ABC
*    mo_sscr_abc = NEW zcl_xx_sscr_abc( param_a = ?
*                                       param_c = mo_sscr_xyz ).
*    "Cast to base class to be able to export screen instance
*    DATA(lo_sscr_abc) = CAST zcl_ca_scr_fw_screen_ctlr( mo_sscr_abc ).
*    lo_sscr_abc->create_screen( ).
*    mo_window->add_view( io_view = lo_sscr_abc->mo_screen ).
*    SET HANDLER mo_sscr_abc->on_.... FOR mo_sscr_xyz.

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*   REPEAT this for each subscreen you want to use in your main
*   screen and/or in your composite screen, like a tab view.
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  ENDMETHOD.                    "create_screen


  METHOD create_status.
    "-----------------------------------------------------------------*
    "   Create GUI status
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_pfstatus TYPE syst_pfkey.

    IF mo_gui_status IS INITIAL.
      mo_gui_status = NEW #( ).
    ENDIF.

    lv_pfstatus = SWITCH #( mv_open_as
                              WHEN mo_scr_options->open_as-screen  THEN c_pfstatus_screen
                              WHEN mo_scr_options->open_as-popup   THEN c_pfstatus_popup ).

    IF mo_gui_status->mv_pfstatus       IS INITIAL OR
       mo_gui_status->mv_pfstatus_repid IS INITIAL.
      mo_gui_status->set_pfstatus( iv_pfstatus       = lv_pfstatus
                                   iv_pfstatus_repid = c_def_repid ).
    ENDIF.

    IF mo_gui_status->mv_titlebar       IS INITIAL OR
       mo_gui_status->mv_titlebar_repid IS INITIAL.
      mo_gui_status->set_titlebar( iv_titlebar       = c_def_titlebar
                                   iv_titlebar_repid = c_def_repid
                                   iv_titlebar_var1  = mo_gui_status->mv_titlebar_var1
                                   iv_titlebar_var2  = mo_gui_status->mv_titlebar_var2
                                   iv_titlebar_var3  = mo_gui_status->mv_titlebar_var3
                                   iv_titlebar_var4  = mo_gui_status->mv_titlebar_var4
                                   iv_titlebar_var5  = mo_gui_status->mv_titlebar_var5 ).
    ENDIF.
  ENDMETHOD.                    "create_status


  METHOD display.
    "-----------------------------------------------------------------*
    "   Display screen
    "-----------------------------------------------------------------*
    "Create screen instance and cast instance into type of a WINDOW
    create_screen( ).

    mo_window->set_mode( mv_mode ).
    mo_window->collect_excl_funcs_for_val_chk( ).

    "Call / display screen
    mo_window->display( ).
  ENDMETHOD.                    "display


  METHOD get_status.
    "-----------------------------------------------------------------*
    "   Get status instance
    "-----------------------------------------------------------------*
    ro_gui_status = mo_gui_status.
  ENDMETHOD.                    "get_status


  METHOD on_call_screen.
    "-----------------------------------------------------------------*
    "   Call simply the screen - nothing else
    "-----------------------------------------------------------------*
    ##needed
    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*    CALL FUNCTION 'Z_xx_yyyyyy_CALL_SCREEN'
*      EXPORTING
*        iv_dynnr         = iv_dynnr
*        iv_open_as       = ...        "use ZCL_CA_C_SCR_FW=>OPEN_AS-*
*        is_popup_corners = ...
  ENDMETHOD.                    "on_call_screen


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Release fields and instances for garbage collection
    "-----------------------------------------------------------------*
    super->on_closed( ).

    FREE: mv_open_as,
          mv_mode,
          ms_popup_corners,

          mo_window,
          mo_gui_status.
  ENDMETHOD.                    "on_closed


  METHOD on_process_event.
    "-----------------------------------------------------------------*
    "   Process screen event
    "-----------------------------------------------------------------*
    TRY.
        CASE iv_event.
          WHEN mo_scr_options->event-pfstatus.
            RAISE EVENT handle_status
              EXPORTING
                io_gui_status = mo_gui_status.
            set_screen_status( ).

          WHEN mo_scr_options->event-pbo.
            handle_pbo( iv_event ).

          WHEN mo_scr_options->event-pai.
            handle_pai( iv_event ).

          WHEN mo_scr_options->event-pov.
            handle_pov( iv_event ).
        ENDCASE.

      CATCH cx_root INTO DATA(lx_catched) ##catch_all.
        CASE iv_event.
          WHEN mo_scr_options->event-pai.
            MESSAGE lx_catched TYPE c_msgty_e.
          WHEN OTHERS.
            MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE c_msgty_e.
        ENDCASE.
    ENDTRY.
  ENDMETHOD.                    "on_process_event


  METHOD on_process_fcode.
    "-----------------------------------------------------------------*
    "   Handle function code
    "-----------------------------------------------------------------*
    IF   mo_gui_status->mv_pfstatus_repid EQ c_def_repid        AND
       ( mo_gui_status->mv_pfstatus       EQ c_pfstatus_screen   OR
         mo_gui_status->mv_pfstatus       EQ c_pfstatus_popup ).
      CASE iv_fcode.
        WHEN mo_fcodes->back   OR mo_fcodes->exit   OR
             mo_fcodes->cancel OR mo_fcodes->enter.
          set_fcode_handled( ).
          close( ).

        WHEN OTHERS.
          RAISE EVENT fcode_triggered
            EXPORTING iv_fcode = iv_fcode.
      ENDCASE.

    ELSE.
      RAISE EVENT fcode_triggered
        EXPORTING iv_fcode = iv_fcode.
    ENDIF.
  ENDMETHOD.                    "on_process_fcode


  METHOD on_set_status.
    "-----------------------------------------------------------------*
    "   GUI status was set - do depending adjustments here
    "-----------------------------------------------------------------*
    ##needed
    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*
*  Steps to do
*  1. COPY function group ZCA_SCR_FW_COPY_PATTERN in package ZCA_TBX_SCR_REUSABLE
*     to your name space. Now you WORK ONLY with this NEW FUNCTION GROUP!!
*
*  2. COPY the needed screens of the new FG to your number and design the
*     layout. Change the screen logic only in a way as described there.
*
*  3. There are the GUI status POPUP_PATTERN and SCREEN_PATTERN included. They
*     provide already the following function codes. Either you copy one of it
*     and change it as requested or redefine this method and set all not needed
*     functions into EXCLUDED functions -> see code pattern below.
*
*     All these functions are provided by inheritance and can be used with mo_fcodes->xxxx.
*
*     Function Code        Function Text             Icon Name
*     BACK                 Back
*     CANCEL               Cancel                    ICON_CANCEL
*     CHANGE               Change                    ICON_CHANGE
*     COPY                 Copy                      ICON_COPY_OBJECT
*     CREATE               Create                    ICON_CREATE
*     DELETE               Delete                    ICON_DELETE
*     DEL_ROW              Delete line               ICON_DELETE_ROW
*     DETAIL               Choose / Detail           ICON_SELECT_DETAIL
*     DISPLAY              Display                   ICON_DISPLAY
*     ENTER                Enter / continue          ICON_OKAY
*     EXIT                 Exit
*     FIRST_PAGE           First Page                ICON_FIRST_PAGE
*     INS_ROW              Insert line               ICON_INSERT_ROW
*     LAST_PAGE            Last Page                 ICON_LAST_PAGE
*     NEXT_PAGE            Next page                 ICON_NEXT_PAGE
*     PREV_PAGE            Previous Page             ICON_PREVIOUS_PAGE
*     PRINT                Print                     ICON_PRINT
*     REFRESH              Refresh                   ICON_REFRESH
*     SAVE                 Save                      ICON_SYSTEM_SAVE
*     SEARCH               Find                      ICON_SEARCH
*     SEARCH_NEXT          Find next                 ICON_SEARCH_NEXT
*
*  4. There is also the title bar TITLE_PATTERN that has simply 5 place holders
*     you can use -> &1 &2 &3 &4 &5. So you can simply use these. Then you need
*     only to provide the variables for the method call below in the pattern code.


*    "Local data definitions
*    DATA:
*      lt_excl_fcode TYPE salv_dynpro_t_fcode.
*
*    CASE mo_screen->mv_mode.
*      WHEN c_mode_display.
*        APPEND: mo_fcodes->... TO lt_excl_fcode,  "?...
*                mo_fcodes->... TO lt_excl_fcode,  "?...
*                mo_fcodes->... TO lt_excl_fcode.  "?...
*
*      WHEN c_mode_modify.
*        APPEND: mo_fcodes->... TO lt_excl_fcode,  "?...
*                mo_fcodes->... TO lt_excl_fcode.  "?...
*    ENDCASE.
*
*    mo_gui_status->set_pfstatus( iv_pfstatus       = 'name_of_pf_status'
*                                 iv_pfstatus_repid = 'name_of_pf_status_program' ) ##no_text.
*
*    "Hide functions for mode x
*    mo_gui_status->set_excl_fcode( lt_excl_fcode ).
*
*    "Prepare object key for titlebar
*    DATA(lv_key) = prepare_key( ).
*
*    mo_gui_status->set_titlebar( iv_titlebar       = 'name_of_titlebar' ##no_text
*                                 iv_titlebar_repid = 'name_of_titlebar_program' ##no_text
*                                 iv_titlebar_var1  = lv_key ).
  ENDMETHOD.                    "on_set_status


  METHOD set_screen_status.
    "-----------------------------------------------------------------*
    "   Set GUI status
    "-----------------------------------------------------------------*
    create_status( ).

    mo_window ?= mo_screen.

    mo_window->set_titlebar( iv_titlebar_repid = mo_gui_status->mv_titlebar_repid
                             iv_titlebar       = mo_gui_status->mv_titlebar
                             iv_titlebar_var1  = mo_gui_status->mv_titlebar_var1
                             iv_titlebar_var2  = mo_gui_status->mv_titlebar_var2
                             iv_titlebar_var3  = mo_gui_status->mv_titlebar_var3
                             iv_titlebar_var4  = mo_gui_status->mv_titlebar_var4
                             iv_titlebar_var5  = mo_gui_status->mv_titlebar_var5 ).

    mo_window->set_pfstatus( iv_pfstatus_repid = mo_gui_status->mv_pfstatus_repid
                             iv_pfstatus       = mo_gui_status->mv_pfstatus ).

    mo_window->set_excl_fcode( mo_gui_status->mt_excl_fcode ).
  ENDMETHOD.                    "set_screen_status

ENDCLASS.
