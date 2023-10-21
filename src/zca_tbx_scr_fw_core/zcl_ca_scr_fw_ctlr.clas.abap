"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro framework - FW controller</p>
CLASS zcl_ca_scr_fw_ctlr DEFINITION PUBLIC
                                    FINAL
                                    CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for screen /dynpro framework</p>
      mo_options   TYPE REF TO zcl_ca_c_scr_fw READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Program name to current screen number</p>
      mv_repid     TYPE syrepid READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current screen number</p>
      mv_dynnr     TYPE syst_dynnr READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Screen field name where evt for value-request was triggered</p>
      mv_pov_field TYPE dynfnam READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Actual function code (used in screens therefore changable)</p>
      mv_okcode    TYPE syst_ucomm.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Excluded function codes of current status</p>
      mt_excl_fcode     TYPE zca_tt_excl_fcode READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Instance of current view and cursor information</p>
      ms_cursor         TYPE zca_s_scr_fw_screen_cursor READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Current (at PAI) / last (at PBO) function code</p>
      mv_fcode          TYPE syst_ucomm READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current GUI status</p>
      mv_pfstatus       TYPE syst_pfkey READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Program name to current GUI status</p>
      mv_pfstatus_repid TYPE syrepid    READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current title bar</p>
      mv_titlebar       TYPE gui_title  READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Program name to current title bar</p>
      mv_titlebar_repid TYPE syrepid    READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 1 for current title bar</p>
      mv_titlebar_var1  TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 2 for current title bar</p>
      mv_titlebar_var2  TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 3 for current title bar</p>
      mv_titlebar_var3  TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 4 for current title bar</p>
      mv_titlebar_var4  TYPE string     READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Variable 5 for current title bar</p>
      mv_titlebar_var5  TYPE string     READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class constructor</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Get current cursor position</p>
      "!
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      dynpro_get_cursor
        IMPORTING
          VALUE(iv_repid) TYPE syrepid,

      "! <p class="shorttext synchronized" lang="en">Get next subscreen</p>
      "!
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      dynpro_get_subscreen
        IMPORTING
          VALUE(iv_repid) TYPE syrepid,

      "! <p class="shorttext synchronized" lang="en">Handle events of current screen</p>
      "!
      "! @parameter iv_repid  | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      "! @parameter iv_event  | <p class="shorttext synchronized" lang="en">Screen event</p>
      dynpro_handle_event
        IMPORTING
          VALUE(iv_repid) TYPE syrepid
          iv_event        TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Get singleton instance of screen framework controller</p>
      "!
      "! @parameter ro_fw | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - FW controller</p>
      get_instance
        RETURNING
          VALUE(ro_fw) TYPE REF TO zcl_ca_scr_fw_ctlr.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Get next active view</p>
      "!
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      "! @parameter iv_dynnr | <p class="shorttext synchronized" lang="en">Number of (sub-)screen</p>
      "! @parameter ro_view  | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      get_view
        IMPORTING
          iv_repid       TYPE syrepid
          iv_dynnr       TYPE syst_dynnr
        RETURNING
          VALUE(ro_view) TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Register view for current screen</p>
      "!
      "! @parameter io_view | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      register_view
        IMPORTING
          io_view TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Set cursor for (sub-)screen</p>
      "!
      "! @parameter io_view   | <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro framework - (Sub-)Screen</p>
      "! @parameter is_cursor | <p class="shorttext synchronized" lang="en">Detailed cursor information, mostly for the value-request</p>
      set_cursor
        IMPORTING
          io_view   TYPE REF TO zcl_ca_scr_fw_screen
          is_cursor TYPE zca_s_scr_fw_cursor_pos_dynpro,

      "! <p class="shorttext synchronized" lang="en">Set excluded function codes</p>
      "!
      "! @parameter it_fcodes | <p class="shorttext synchronized" lang="en">Excluded function codes</p>
      set_excl_fcode
        IMPORTING
          it_fcodes TYPE zca_tt_excl_fcode,

      "! <p class="shorttext synchronized" lang="en">Set function code</p>
      "!
      "! @parameter iv_fcode | <p class="shorttext synchronized" lang="en">Function code</p>
      set_fcode
        IMPORTING
          iv_fcode TYPE syst_ucomm,

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

      "! <p class="shorttext synchronized" lang="en">Unregister view from current screen</p>
      "!
      "! @parameter io_view | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      unregister_view
        IMPORTING
          io_view TYPE REF TO zcl_ca_scr_fw_screen.

* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - FW controller</p>
      mo_fw TYPE REF TO zcl_ca_scr_fw_ctlr.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">All (sub-)screens</p>
      mt_views            TYPE zca_tt_scr_fw_screens,
      "! <p class="shorttext synchronized" lang="en">Currently used (sub-)screens</p>
      mt_views_on_display TYPE zca_tt_scr_fw_screens.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get next screen</p>
      "!
      "! @parameter iv_repid | <p class="shorttext synchronized" lang="en">Program name to current screen</p>
      "! @parameter ro_view  | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      get_screen
        IMPORTING
          VALUE(iv_repid) TYPE syrepid
        RETURNING
          VALUE(ro_view)  TYPE REF TO zcl_ca_scr_fw_screen.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine view type</p>
      "!
      "! @parameter io_view | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      "! @parameter result  | <p class="shorttext synchronized" lang="en">View type</p>
      get_view_type
        IMPORTING
          io_view       TYPE REF TO zcl_ca_scr_fw_screen
        RETURNING
          VALUE(result) TYPE char1.

ENDCLASS.



CLASS zcl_ca_scr_fw_ctlr IMPLEMENTATION.

  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    mo_options = zcl_ca_c_scr_fw=>get_instance( ).
  ENDMETHOD.                    "class_constructor


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "constructor


  METHOD dynpro_get_cursor.
    "-----------------------------------------------------------------*
    "   Get current cursor position
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_cursor TYPE zca_s_scr_fw_cursor_pos_dynpro.

    DATA(lo_view) = zcl_ca_scr_fw_ctlr=>get_screen( iv_repid ).
    IF lo_view IS NOT BOUND.
      RETURN.
    ENDIF.

    GET CURSOR FIELD ls_cursor-field_name VALUE ls_cursor-field_value
                                          LINE  ls_cursor-tab_ctrl_line
                                          AREA  ls_cursor-sscr_area_or_tab_ctrl.
    lo_view->set_cursor( ls_cursor ).
    zcl_ca_scr_fw_ctlr=>mo_fw->set_cursor( io_view   = lo_view
                                           is_cursor = ls_cursor ).
  ENDMETHOD.                    "dynpro_get_cursor


  METHOD dynpro_get_subscreen.
    "-----------------------------------------------------------------*
    "   Get next subscreen
    "-----------------------------------------------------------------*
    DATA(lo_view) = zcl_ca_scr_fw_ctlr=>get_screen( iv_repid ).
    IF lo_view IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lo_composite) = CAST zcl_ca_scr_fw_composite( lo_view ).
    lo_view = lo_composite->get_next_view( ).

    zcl_ca_scr_fw_ctlr=>mv_repid = lo_view->mv_repid.
    zcl_ca_scr_fw_ctlr=>mv_dynnr = lo_view->mv_dynnr.
  ENDMETHOD.                    "dynpro_get_subscreen


  METHOD dynpro_handle_event.
    "-----------------------------------------------------------------*
    "   Handle events of current screen
    "-----------------------------------------------------------------*
    DATA(lo_view) = zcl_ca_scr_fw_ctlr=>get_screen( iv_repid ).
    IF lo_view IS NOT BOUND.
      RETURN.
    ENDIF.

    CASE iv_event.
      WHEN zcl_ca_scr_fw_ctlr=>mo_options->event-pai.
        "The first method has an error, that's why the function MUST
        "be set at first for the view and then for the framework.
        lo_view->set_fcode( zcl_ca_scr_fw_ctlr=>mv_okcode ).
        zcl_ca_scr_fw_ctlr=>mo_fw->set_fcode( zcl_ca_scr_fw_ctlr=>mv_okcode ).

      WHEN zcl_ca_scr_fw_ctlr=>mo_options->event-fcode.
        zcl_ca_scr_fw_ctlr=>mo_fw->set_fcode( zcl_ca_scr_fw_ctlr=>mv_okcode ).
        CLEAR zcl_ca_scr_fw_ctlr=>mv_okcode.

      WHEN zcl_ca_scr_fw_ctlr=>mo_options->event-pov.
        zcl_ca_scr_fw_ctlr=>dynpro_get_cursor( iv_repid ).
    ENDCASE.

    lo_view->on_event( iv_event ).
  ENDMETHOD.                    "dynpro_handle_event


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get singleton instance of screen framework controller
    "-----------------------------------------------------------------*
    IF zcl_ca_scr_fw_ctlr=>mo_fw IS NOT BOUND.
      zcl_ca_scr_fw_ctlr=>mo_fw = NEW #( ).
    ENDIF.

    ro_fw = zcl_ca_scr_fw_ctlr=>mo_fw.
  ENDMETHOD.                    "get_instance


  METHOD get_screen.
    "-----------------------------------------------------------------*
    "   Get next screen
    "-----------------------------------------------------------------*
    ro_view = zcl_ca_scr_fw_ctlr=>mo_fw->get_view( iv_repid = iv_repid
                                                   iv_dynnr = sy-dynnr ).
    IF ro_view IS NOT BOUND.
      RETURN.
    ENDIF.

    zcl_ca_scr_fw_ctlr=>mv_repid = ro_view->mv_repid.
    zcl_ca_scr_fw_ctlr=>mv_dynnr = ro_view->mv_dynnr.
  ENDMETHOD.                    "get_screen


  METHOD get_view.
    "-----------------------------------------------------------------*
    "   Get next active view
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_view              TYPE zca_s_scr_fw_screen.

    LOOP AT mt_views_on_display REFERENCE INTO DATA(lr_view_on_display).
      IF lr_view_on_display->repid      EQ iv_repid AND
         lr_view_on_display->dynnr      EQ iv_dynnr AND
         lr_view_on_display->on_display EQ abap_true.
        TRY.
            ro_view = mt_views[ repid      = iv_repid
                                dynnr      = iv_dynnr
                                on_display = abap_false ]-o_view.

          CATCH cx_sy_itab_line_not_found.
            ro_view = lr_view_on_display->o_view.
        ENDTRY.
        EXIT.

      ELSE.
        IF lr_view_on_display->view_type EQ mo_options->view_type-screen.
          DELETE mt_views_on_display INDEX sy-tabix.
          CONTINUE.

        ELSEIF sy-tabix NE 1.
          EXIT.
        ENDIF.

        IF lr_view_on_display->view_type EQ mo_options->view_type-window     OR
           lr_view_on_display->view_type EQ mo_options->view_type-composite.
          DATA(lo_composite) = CAST zcl_ca_scr_fw_composite( lr_view_on_display->o_view ).
          LOOP AT lo_composite->mt_views ASSIGNING FIELD-SYMBOL(<ls_composite_view>)
                                         WHERE repid      EQ iv_repid
                                           AND dynnr      EQ iv_dynnr
                                           AND on_display EQ abap_false.
            IF NOT line_exists( mt_views_on_display[ o_view = <ls_composite_view>-o_view ] ) AND
                   line_exists( mt_views[ o_view = <ls_composite_view>-o_view ] ).
              ro_view = <ls_composite_view>-o_view.
              <ls_composite_view>-on_display = abap_true.
              INSERT <ls_composite_view> INTO mt_views_on_display
                                         INDEX 1.
              EXIT.
            ENDIF.
          ENDLOOP.

          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ro_view IS NOT BOUND.
      TRY.
          DATA(ls_view_for_display) = VALUE #( mt_views[ repid      = iv_repid
                                                         dynnr      = iv_dynnr
                                                         on_display = abap_false ] ).
          ro_view = ls_view_for_display-o_view.
          ls_view_for_display-on_display = abap_true.
          INSERT ls_view_for_display INTO  mt_views_on_display
                                     INDEX 1.

        CATCH cx_sy_itab_line_not_found.
          RETURN.
      ENDTRY.
    ENDIF.

    IF ro_view->mv_view_type EQ mo_options->view_type-window.
      DATA(lo_window)   = CAST zcl_ca_scr_fw_window( ro_view ).
      mv_pfstatus       = lo_window->mv_pfstatus.
      mv_pfstatus_repid = lo_window->mv_pfstatus_repid.
      mv_titlebar       = lo_window->mv_titlebar.
      mv_titlebar_repid = lo_window->mv_titlebar_repid.
      mv_titlebar_var1  = lo_window->mv_titlebar_var1.
      mv_titlebar_var2  = lo_window->mv_titlebar_var2.
      mv_titlebar_var3  = lo_window->mv_titlebar_var3.
      mv_titlebar_var4  = lo_window->mv_titlebar_var4.
      mv_titlebar_var5  = lo_window->mv_titlebar_var5.
    ENDIF.
  ENDMETHOD.                    "get_view


  METHOD get_view_type.
    "-----------------------------------------------------------------*
    "   Determine view type
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_obj_desc  TYPE REF TO cl_abap_objectdescr,
      lv_view_type TYPE i.

    lo_obj_desc ?= cl_abap_objectdescr=>describe_by_object_ref( io_view ).

*... l_type = 1 --> WINDOW VIEW
*    l_type = 2 --> COMPOSITE VIEW
*    l_type = 3 --> SCREEN VIEW

    IF lo_obj_desc->applies_to_class( 'ZCL_CA_SCR_FW_WINDOW' ) EQ abap_true ##no_text.
      lv_view_type += 1.
    ENDIF.

    IF lo_obj_desc->applies_to_class( 'ZCL_CA_SCR_FW_COMPOSITE' ) EQ abap_true ##no_text.
      lv_view_type += 1.
    ENDIF.

    IF lo_obj_desc->applies_to_class( 'ZCL_CA_SCR_FW_SCREEN' ) EQ abap_true ##no_text.
      lv_view_type += 1.
    ENDIF.

    CASE lv_view_type.
      WHEN 1.
        result = mo_options->view_type-window.
      WHEN 2.
        result = mo_options->view_type-composite.
      WHEN 3.
        result = mo_options->view_type-screen.
    ENDCASE.
  ENDMETHOD.


  METHOD register_view.
    "-----------------------------------------------------------------*
    "   Register view for current screen
    "-----------------------------------------------------------------*
    DATA(lv_view_index) = line_index( mt_views[ o_view = io_view ] ).
    IF lv_view_index NE 0.
      DELETE mt_views INDEX lv_view_index.
    ENDIF.

*... check dynnr and repid don't already exist.
*    window view can exist more than once -> top node is valid
    io_view->set_view_type( get_view_type( io_view ) ).

    READ TABLE mt_views INTO DATA(ls_view)
                        WITH KEY view_type = io_view->mv_view_type
                        TRANSPORTING index.
    ls_view-repid       = io_view->mv_repid.
    ls_view-dynnr       = io_view->mv_dynnr.
    ls_view-screen_name = io_view->mv_screen_name.
    ls_view-index       = ls_view-index + 1.
    ls_view-o_view      = io_view.
    ls_view-view_type   = io_view->mv_view_type.
    ls_view-on_display  = abap_false.
    INSERT ls_view INTO TABLE mt_views.

    SORT mt_views BY view_type ASCENDING
                     index     DESCENDING.
  ENDMETHOD.                    "register_view


  METHOD set_cursor.
    "-----------------------------------------------------------------*
    "   Set cursor for (sub-)screen
    "-----------------------------------------------------------------*
    IF io_view IS NOT BOUND.
      RETURN.
    ENDIF.

    ms_cursor-s_cursor_pos = is_cursor.
    ms_cursor-o_view       = io_view.
  ENDMETHOD.                    "set_cursor


  METHOD set_excl_fcode.
    "-----------------------------------------------------------------*
    "   Set excluded function codes
    "-----------------------------------------------------------------*
    mt_excl_fcode = it_fcodes.
  ENDMETHOD.                    "set_excl_fcode


  METHOD set_fcode.
    "-----------------------------------------------------------------*
    "   Set function code
    "-----------------------------------------------------------------*
    mv_fcode = iv_fcode.
  ENDMETHOD.                    "set_fcode


  METHOD set_pfstatus.
    "-----------------------------------------------------------------*
    "   Set GUI status
    "-----------------------------------------------------------------*
    mv_pfstatus       = iv_pfstatus.
    mv_pfstatus_repid = iv_pfstatus_repid.
  ENDMETHOD.                    "set_pfstatus


  METHOD set_titlebar.
    "-----------------------------------------------------------------*
    "   Set title bar
    "-----------------------------------------------------------------*
    mv_titlebar       = iv_titlebar.
    mv_titlebar_repid = iv_titlebar_repid.

    mv_titlebar_var1 = iv_titlebar_var1.
    mv_titlebar_var2 = iv_titlebar_var2.
    mv_titlebar_var3 = iv_titlebar_var3.
    mv_titlebar_var4 = iv_titlebar_var4.
    mv_titlebar_var5 = iv_titlebar_var5.
  ENDMETHOD.                    "set_titlebar


  METHOD unregister_view.
    "-----------------------------------------------------------------*
    "   Unregister view from current screen
    "-----------------------------------------------------------------*
    DELETE: mt_views            WHERE o_view = io_view,
            mt_views_on_display WHERE o_view = io_view.
  ENDMETHOD.                    "unregister_view

ENDCLASS.
