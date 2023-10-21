"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro framework - Composite screen</p>
CLASS zcl_ca_scr_fw_composite DEFINITION PUBLIC
                                         INHERITING FROM zcl_ca_scr_fw_screen
                                         CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">(Sub-) Screen informations (MUST be changeable!!)</p>
      mt_views             TYPE zca_tt_scr_fw_screens.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add view to screen</p>
      "!
      "! @parameter iv_index | <p class="shorttext synchronized" lang="en">Index of screen</p>
      "! @parameter io_view  | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      add_view
        IMPORTING
          iv_index TYPE syst_index OPTIONAL
          io_view  TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Collect func codes of involved views to suppress value check</p>
      collect_excl_funcs_for_val_chk,

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

      "! <p class="shorttext synchronized" lang="en">Get current view</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      get_current_view
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Get next view</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      get_next_view
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Get view by index</p>
      "!
      "! @parameter iv_index | <p class="shorttext synchronized" lang="en">Index of screen</p>
      "! @parameter result   | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      get_view_by_index
        IMPORTING
          iv_index      TYPE syst_index
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Avoid value check for specific function codes</p>
      "!
      "! @parameter rv_no_value_check | <p class="shorttext synchronized" lang="en">X = No value check required</p>
      no_value_check_is_required
        RETURNING
          VALUE(rv_no_value_check) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Remove view from screen</p>
      "!
      "! @parameter io_view | <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      remove_view
        IMPORTING
          io_view TYPE REF TO zcl_ca_scr_fw_screen,

      close REDEFINITION,

      on_event REDEFINITION,

      set_active REDEFINITION,

      set_mode REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Function code list where no value check should be executed</p>
      mra_excl_func_for_value_check TYPE zcl_ca_c_scr_fw=>ty_t_func_codes,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Number of subscreens</p>
      mv_subviews      TYPE syst_index,
      "! <p class="shorttext synchronized" lang="en">Index of last subscreen</p>
      mv_subview_index TYPE syst_index.


*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Handle function code</p>
      on_fcode,

      register REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_scr_fw_composite IMPLEMENTATION.

  METHOD add_view.
    "-----------------------------------------------------------------*
    "   Add view to screen
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_index             TYPE syst_index.

    IF iv_index IS NOT SUPPLIED.
      lv_index = mv_subviews + 1.

    ELSE.
      IF line_exists( mt_views[ index = iv_index ] ).
        lv_index = mv_subviews + 1.
      ELSE.
        lv_index = iv_index.
      ENDIF.
    ENDIF.

    TRY.
        DATA(lr_view) = REF #( mt_views[ index = lv_index ] ).
        lr_view->screen_name = io_view->mv_screen_name.
        lr_view->repid       = io_view->mv_repid.
        lr_view->dynnr       = io_view->mv_dynnr.
        lr_view->o_view      = io_view.
        lr_view->view_type   = io_view->mv_view_type.
        lr_view->on_display  = abap_false.

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( index       = lv_index
                        screen_name = io_view->mv_screen_name
                        repid       = io_view->mv_repid
                        dynnr       = io_view->mv_dynnr
                        o_view      = io_view
                        view_type   = io_view->mv_view_type
                        on_display  = abap_false ) INTO TABLE mt_views.

        mv_subviews += 1.
    ENDTRY.

    SORT mt_views BY index.
    io_view->set_parent( me ).
    io_view->set_active( mv_active ).
  ENDMETHOD.                    "add_view


  METHOD close.
    "-----------------------------------------------------------------*
    "   Close all screen
    "-----------------------------------------------------------------*
    LOOP AT mt_views REFERENCE INTO DATA(lr_view).
      lr_view->o_view->close( ).
    ENDLOOP.

    super->close( ).

    FREE: mt_views,
          mv_subviews,
          mv_subview_index.
  ENDMETHOD.                    "close


  METHOD collect_excl_funcs_for_val_chk.
    "-----------------------------------------------------------------*
    "   Collect all excluded function codes avoiding a value check
    "-----------------------------------------------------------------*
    LOOP AT mt_views REFERENCE INTO DATA(lr_view).
      APPEND LINES OF lr_view->o_view->mo_screen_ctlr->get_excl_funcs_for_val_check( )
                                                                          TO mra_excl_func_for_value_check.
    ENDLOOP.

    APPEND LINES OF mo_screen_ctlr->get_excl_funcs_for_val_check( ) TO mra_excl_func_for_value_check.
  ENDMETHOD.                    "collect_excl_funcs_for_val_chk


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( io_screen_ctlr = io_screen_ctlr
                        iv_repid       = iv_repid
                        iv_dynnr       = iv_dynnr
                        iv_screen_name = iv_screen_name ).
  ENDMETHOD.                    "constructor


  METHOD get_current_view.
    "-----------------------------------------------------------------*
    "   Get current view
    "-----------------------------------------------------------------*
    result = get_view_by_index( mv_subview_index ).
  ENDMETHOD.                    "get_current_view


  METHOD get_next_view.
    "-----------------------------------------------------------------*
    "   Get next view
    "-----------------------------------------------------------------*
    mv_subview_index += 1.
    result = get_view_by_index( mv_subview_index ).

    IF mv_subview_index GE mv_subviews.
      CLEAR mv_subview_index.
    ENDIF.
  ENDMETHOD.                    "get_next_view


  METHOD get_view_by_index.
    "-----------------------------------------------------------------*
    "   Get view by index
    "-----------------------------------------------------------------*
    IF iv_index IS INITIAL.
      RETURN.
    ENDIF.

    result = VALUE #( mt_views[ iv_index ]-o_view OPTIONAL ).
  ENDMETHOD.                    "get_view_by_index


  METHOD no_value_check_is_required.
    "-----------------------------------------------------------------*
    "   Is a value check required for specific function codes
    "-----------------------------------------------------------------*
    rv_no_value_check = abap_false.
    IF zcl_ca_scr_fw_ctlr=>get_instance( )->mv_fcode IN mra_excl_func_for_value_check.
      rv_no_value_check = abap_true.
    ENDIF.
  ENDMETHOD.                    "no_value_check_is_required


  METHOD on_event.
    "-----------------------------------------------------------------*
    "   Handle screen event
    "-----------------------------------------------------------------*
    CASE iv_event.
      WHEN mo_scr_options->event-fcode.
        on_fcode( ).

      WHEN mo_scr_options->event-pbo OR
           mo_scr_options->event-pai.

      WHEN OTHERS.
    ENDCASE.

    super->on_event( iv_event ).
  ENDMETHOD.                    "on_event


  METHOD on_fcode.
    "-----------------------------------------------------------------*
    "   Handle function code
    "-----------------------------------------------------------------*
    LOOP AT mt_views REFERENCE INTO DATA(lr_view).
      lr_view->on_display = abap_false.
      lr_view->o_view->on_event( mo_scr_options->event-fcode ).
      lr_view->o_view->set_first_pbo( abap_false ).
    ENDLOOP.
  ENDMETHOD.                    "on_fcode


  METHOD register.
    "-----------------------------------------------------------------*
    "   Register screens
    "-----------------------------------------------------------------*
    super->register( ).

    LOOP AT mt_views REFERENCE INTO DATA(lr_view).
      lr_view->o_view->register( ).
    ENDLOOP.
  ENDMETHOD.                    "register


  METHOD remove_view.
    "-----------------------------------------------------------------*
    "   Remove view from screen
    "-----------------------------------------------------------------*
    DELETE mt_views WHERE o_view EQ io_view.
    IF sy-subrc EQ 0.
      mv_subviews -= 1.
    ENDIF.
  ENDMETHOD.                    "remove_view


  METHOD set_active.
    "-----------------------------------------------------------------*
    "   Set screen (in-)active
    "-----------------------------------------------------------------*
    super->set_active( iv_active ).

    LOOP AT mt_views REFERENCE INTO DATA(lr_view).
      lr_view->o_view->set_active( iv_active ).
    ENDLOOP.
  ENDMETHOD.                    "set_active


  METHOD set_mode.
    "-----------------------------------------------------------------*
    "   Set screen mode / change between display and modify
    "-----------------------------------------------------------------*
    LOOP AT mt_views REFERENCE INTO DATA(lr_view).
      lr_view->o_view->set_mode( iv_mode ).
    ENDLOOP.

    super->set_mode( iv_mode ).
  ENDMETHOD.                    "set_mode

ENDCLASS.
