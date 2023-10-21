"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro fw - (Sub-)Screen controller</p>
CLASS zcl_ca_scr_fw_screen_ctlr DEFINITION PUBLIC
                                           CREATE PUBLIC
                                           ABSTRACT.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for screen/dynpro framework</p>
      mo_scr_options       TYPE REF TO zcl_ca_c_scr_fw READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_frame_label | <p class="shorttext synchronized" lang="en">Frame label</p>
      constructor
        IMPORTING
          iv_frame_label TYPE text70 OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Get excluded function codes of the view</p>
      "!
      "! @parameter rra_excl_functions | <p class="shorttext synchronized" lang="en">Exclude functions of the view</p>
      get_excl_funcs_for_val_check
        RETURNING
          VALUE(rra_excl_functions) TYPE zcl_ca_c_scr_fw=>ty_t_func_codes,

      "! <p class="shorttext synchronized" lang="en">Avoid value check for specific function codes</p>
      "!
      "! @parameter rv_no_value_check | <p class="shorttext synchronized" lang="en">X = No value check required</p>
      no_value_check_is_required
        RETURNING
          VALUE(rv_no_value_check) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Trigger function code REFRESH</p>
      refresh.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_a FOR  if_xo_const_message~abort,
      c_msgty_e FOR  if_xo_const_message~error,
      c_msgty_i FOR  if_xo_const_message~info,
      c_msgty_s FOR  if_xo_const_message~success,
      c_msgty_w FOR  if_xo_const_message~warning.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Frame label in subscrs. with dynpro no -> see FG fields</p>
      c_frame_label   TYPE dynfnam VALUE 'GS_FRAME_LABEL-D' ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Common object: Screen / dynpro framework - (Sub-)Screen</p>
      mo_screen                     TYPE REF TO zcl_ca_scr_fw_screen,
      "! <p class="shorttext synchronized" lang="en">Screen field attributes (usage with table SCREEN)</p>
      mo_scr_fld_attr               TYPE REF TO zcl_ca_c_screen_field_attr,
      "! <p class="shorttext synchronized" lang="en">Function code constants (with suggested icons)</p>
      mo_fcodes                     TYPE REF TO zcl_ca_c_fcodes,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Function code list where no value check should be executed</p>
      mra_excl_func_for_value_check TYPE zcl_ca_c_scr_fw=>ty_t_func_codes,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Frame label</p>
      mv_frame_label                TYPE text70,
      "! <p class="shorttext synchronized" lang="en">(Sub-)Screen name</p>
      mv_screen_name                TYPE zca_d_screen_name,
      "! <p class="shorttext synchronized" lang="en">Is screen controller to this screen number ...</p>
      mv_dynnr                      TYPE syst_dynnr,
      "! <p class="shorttext synchronized" lang="en">... and this program id</p>
      mv_repid                      TYPE syrepid.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Adjust screen fields for display or modification</p>
      adjust_screen,

      "! <p class="shorttext synchronized" lang="en">Validate captured data</p>
      check_values,

      "! <p class="shorttext synchronized" lang="en">Create screen instance</p>
      create_screen,

      "! <p class="shorttext synchronized" lang="en">Exchange data between screen program and controller</p>
      "!
      "! <p>Use this method at PBO to set initial and default values and/or at PAI to get the values captured
      "! by the user.</p>
      "!
      "! <p><strong>Precondition</strong> is that you defined the necessary DDIC structures with statement
      "! TABLES or as global fields in the TOP include of your function group containing your screens</p>.
      "!
      "! @parameter iv_event    | <p class="shorttext synchronized" lang="en">Screen event - only PBO or PAI</p>
      "! @parameter iv_fname    | <p class="shorttext synchronized" lang="en">Field / structure name defined in TOP include</p>
      "! <p>Contains the field name used within the TOP include of your function group.</p>
      "! @parameter ir_data_ref | <p class="shorttext synchronized" lang="en">Outgoing data reference depending on the event</p>
      "! @parameter io_object   | <p class="shorttext synchronized" lang="en">Outgoing object reference depending on the event</p>
      "!
      "! @parameter er_data_ref | <p class="shorttext synchronized" lang="en">Incoming data reference depending on the event</p>
      "! @parameter eo_object   | <p class="shorttext synchronized" lang="en">Incoming object reference depending on the event</p>
      "!
      "! @parameter cv_data     | <p class="shorttext synchronized" lang="en">Incoming or outgoing data depending on the event</p>
      "! <p>Hand over the corresponding structure or field you use in your controller class. So typically a
      "! structure or field used in a subclass of this class.</p>
      exchange_data
        IMPORTING
          iv_event    TYPE syst_ucomm
          iv_fname    TYPE dynfnam
          ir_data_ref TYPE REF TO data OPTIONAL
          io_object   TYPE REF TO object OPTIONAL
        EXPORTING
          er_data_ref TYPE REF TO data
          eo_object   TYPE REF TO object
        CHANGING
          cv_data     TYPE any OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Register function code to be excluded from value check</p>
      "!
      "! @parameter iv_fcode | <p class="shorttext synchronized" lang="en">Function code to be excluded (generic vals possible with *+)</p>
      exclude_fcode_for_value_check
        IMPORTING
          iv_fcode TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Handle Process After Input - but no FCODE handling!</p>
      "!
      "! @parameter iv_event     | <p class="shorttext synchronized" lang="en">Screen event PAI</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      handle_pai
        IMPORTING
          iv_event TYPE syst_ucomm
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Handle Process Before Output - but set no GUI status!</p>
      "!
      "! @parameter iv_event     | <p class="shorttext synchronized" lang="en">Screen event PBO</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      handle_pbo
        IMPORTING
          iv_event TYPE syst_ucomm
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Handle value requests</p>
      "!
      "! @parameter iv_event     | <p class="shorttext synchronized" lang="en">Screen event PBO</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      handle_pov
        IMPORTING
          iv_event TYPE syst_ucomm
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Release fields and instances for garbage collection</p>
      on_closed
        FOR EVENT closed OF zcl_ca_scr_fw_screen,

      "! <p class="shorttext synchronized" lang="en">Process screen event</p>
      "!
      "! @parameter iv_event     | <p class="shorttext synchronized" lang="en">Screen event</p>
      on_process_event
        FOR EVENT process_event OF zcl_ca_scr_fw_screen
        IMPORTING
          iv_event,

      "! <p class="shorttext synchronized" lang="en">Handle function code</p>
      "!
      "! @parameter iv_fcode | <p class="shorttext synchronized" lang="en">Function code</p>
      on_process_fcode
        FOR EVENT process_fcode OF zcl_ca_scr_fw_screen
        IMPORTING
          iv_fcode,

      "! <p class="shorttext synchronized" lang="en">Initialize function code to space - execution is complete</p>
      "!
      "! <p>Use this method to confirm that the function is completed, either successfully or in case an
      "! error occurred and further actions have to be avoided for this function code.</p>
      set_fcode_handled,

      "! <p class="shorttext synchronized" lang="en">Use at PBO: Set frame label if passed via constructor</p>
      "!
      "! <p><strong>Precondition</strong> is that a corresponding field is defined within the TOP include of
      "! your function group containing your screens. The field must be duplicated in structure GS_FRAME_LABEL
      "! using the screen number you want to label.</p>
      set_frame_label.

ENDCLASS.



CLASS zcl_ca_scr_fw_screen_ctlr IMPLEMENTATION.

  METHOD adjust_screen.
    "-----------------------------------------------------------------*
    "   Use at PBO: Adjust screen fields for display or modification
    "-----------------------------------------------------------------*

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*  REDEFINE this method to adjust screen fields and call it in HANDLE_PBO
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

    "Modify screen if only display
    IF mo_screen->mv_mode EQ mo_scr_options->mode-display.
      mo_scr_fld_attr->set_to_display_only( screen_field_name = '*' ).

* ! ! ! ! ! !   Old solution ==> USE THE NEW ONE ABOVE  ! ! ! ! ! !
*      LOOP AT SCREEN INTO DATA(ls_screen).
*        IF ls_screen-input EQ mo_scr_fld_attr->switch-on.
*          ls_screen-input = mo_scr_fld_attr->switch-off.
*          MODIFY SCREEN FROM ls_screen.
*        ENDIF.
*      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "adjust_screen


  METHOD check_values ##needed.
    "-----------------------------------------------------------------*
    "   Validate captured data
    "-----------------------------------------------------------------*

    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*  REDEFINE this method to check the values and call it in HANDLE_PAI
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  ENDMETHOD.                    "check_values


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    mo_scr_options  = zcl_ca_c_scr_fw=>get_instance( ).
    mo_fcodes       = zcl_ca_c_fcodes=>get_instance( ).
    mo_scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).


    mv_frame_label  = iv_frame_label.

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*  USE the constructor to open the controller for specific objects
*  you need in here. E. g. the controller instance of another subscreen
*  to interact between them via events.
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

  ENDMETHOD.                    "constructor


  METHOD create_screen.
    "-----------------------------------------------------------------*
    "   Create screen instance
    "-----------------------------------------------------------------*
    IF mo_screen IS NOT BOUND.
      mo_screen = NEW #( io_screen_ctlr = me
                         iv_repid       = mv_repid
                         iv_dynnr       = mv_dynnr
                         iv_screen_name = mv_screen_name ).

      SET HANDLER: on_process_event FOR mo_screen,
                   on_process_fcode FOR mo_screen,
                   on_closed        FOR mo_screen.
    ENDIF.
  ENDMETHOD.                    "create_screen


  METHOD exchange_data.
    "-----------------------------------------------------------------*
    "   Exchange data between screen program and controller
    "-----------------------------------------------------------------*
    "Local data definitions
    FIELD-SYMBOLS:
      <lv_data>   TYPE data.

    "Assemble field name
    DATA(lv_fname) = CONV char80( |({ mv_repid }){ iv_fname }| ).
    ASSIGN (lv_fname) TO <lv_data>.
    IF sy-subrc NE 0.
      "Allocation ASSIGN &1 ->* to <&2> failed
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>assign_val_failed
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( lv_fname )
          mv_msgv2 = '<LV_DATA>' ##no_text.
    ENDIF.

    TRY.
        CASE iv_event.
          WHEN mo_scr_options->event-pbo.
            IF cv_data IS SUPPLIED.
              <lv_data> = cv_data.
            ELSEIF ir_data_ref IS SUPPLIED.
              <lv_data> = ir_data_ref.
            ELSEIF io_object IS SUPPLIED.
              <lv_data> ?= io_object.
            ENDIF.

          WHEN mo_scr_options->event-pai.
            IF cv_data IS SUPPLIED.
              cv_data = <lv_data>.
            ELSEIF er_data_ref IS SUPPLIED.
              er_data_ref = <lv_data>.
            ELSEIF eo_object IS SUPPLIED.
              eo_object ?= <lv_data>.
            ENDIF.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error) ##catch_all.
        "Create exception
        DATA(lx_intern) =
              CAST zcx_ca_intern(
                     zcx_ca_intern=>create_exception(
                             iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                             iv_class    = CONV #( cl_abap_classdescr=>get_class_name( me ) )
                             iv_method   = 'EXCHANGE_DATA'
                             ix_error    = lx_error ) )  ##no_text.
        IF lx_intern IS BOUND.
          RAISE EXCEPTION lx_intern.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "exchange_data


  METHOD exclude_fcode_for_value_check.
    "-----------------------------------------------------------------*
    "   Register function code to be excluded from value check
    "-----------------------------------------------------------------*
    APPEND VALUE #( sign   = zcl_ca_c_sel_options=>sign-incl
                    option = COND #( WHEN iv_fcode CA '*+'
                                       THEN zcl_ca_c_sel_options=>option-cp
                                       ELSE zcl_ca_c_sel_options=>option-eq )
                    low    = iv_fcode ) TO mra_excl_func_for_value_check.
  ENDMETHOD.                    "exclude_fcode_for_value_check


  METHOD get_excl_funcs_for_val_check.
    "-----------------------------------------------------------------*
    "   Get excluded function codes defined by the view
    "-----------------------------------------------------------------*
    rra_excl_functions = mra_excl_func_for_value_check.
  ENDMETHOD.                    "get_excl_funcs_for_val_check


  METHOD handle_pai ##needed.
    "-----------------------------------------------------------------*
    "   Handle Process After Input - but no FCODE handling!
    "-----------------------------------------------------------------*

    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*    "E. g. no data transport in display mode
*    IF mo_screen->mv_mode EQ mo_options->mode-display.
*      RETURN.
*    ENDIF.
*
*    "E. g. no data transfer or value checks for specific function codes
*    "Define the required function in your CONSTRUCTOR using method
*    "EXCLUDE_FCODE_FOR_VALUE_CHECK for each function code.
*    IF no_value_check_is_required( ).
*      RETURN.
*    ENDIF.
*
*    TRY.
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*   REPEAT the call for each used structure and/or you need to
*   to transfer it into the corresponding objects of the controller
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*        "Get data from screen structure
*        exchange_data(
*                 EXPORTING
*                   iv_event = iv_event
*                   iv_fname = 'name_of_structure/field' ##no_text
*                 CHANGING
*                   cv_data  = m...structure/field ).
*
*      CATCH zcx_ca_param INTO DATA(lx_error).
*        set_fcode_handled( ).
*        MESSAGE lx_error TYPE c_msgty_e.
*    ENDTRY.
  ENDMETHOD.                    "handle_pai


  METHOD handle_pbo ##needed.
    "-----------------------------------------------------------------*
    "   Handle Process Before Output - but set no GUI status!
    "-----------------------------------------------------------------*
    set_frame_label( ).

    "Adjust screen objects
    adjust_screen( ).

    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*    TRY.
*        "Do preparations for display
*        get_company_code_data( ).
*
*        "...?
*        get_plant_data( ).
*
*        "Handle specific actions only with the first execution of this method
*        IF mo_screen->mv_is_first_pbo EQ abap_true   AND
*           m....                      IS NOT INITIAL.
*          "Use events to interact between subscreens
*          RAISE EVENT cc_data_changed
*            EXPORTING
*              is_t001 = ms_t001.
*        ENDIF.
*
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*   REPEAT the call for each used structure and/or field you need to
*   to transfer it into the corresponding objects of the controller
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*        "Set prepared values to screen structure
*        exchange_data(
*                  EXPORTING
*                    iv_event = iv_event
*                    iv_fname = 'name_of_structure/field' ##no_text
*                  CHANGING
*                    cv_data  = m...structure/field ).
*
*        "Set company code data to screen structure
*        exchange_data(
*                  EXPORTING
*                    iv_event = iv_event
*                    iv_fname = 'T001' ##no_text
*                  CHANGING
*                    cv_data  = ms_t001 ).
*
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*  REDEFINE this method to adjust fields and call it here in HANDLE_PBO
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*
*        IF m... IS NOT INITIAL.
*          SET CURSOR FIELD c_fname_....
*        ENDIF.
*
*      CATCH zcx_ca_param INTO DATA(lx_error).
*        MESSAGE lx_error TYPE c_msgty_s DISPLAY LIKE lx_error->mv_msgty.
*    ENDTRY.
  ENDMETHOD.                    "handle_pbo


  METHOD handle_pov ##needed.
    "-----------------------------------------------------------------*
    "   Handle value requests
    "-----------------------------------------------------------------*

    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*    "Handle search help for ...
*    CASE mr_screen->ms_cursor-field.
*      WHEN cs_scr_fname-....
*        "... Field description
*        f4_...( ).
*
*      WHEN cs_scr_fname-plant.
*        "... Plant
*        f4_plant( ).
*    ENDCASE.
  ENDMETHOD.                    "handle_pov


  METHOD no_value_check_is_required.
    "-----------------------------------------------------------------*
    "   Is a value check required for specific function codes
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_window            TYPE REF TO zcl_ca_scr_fw_window.

    CASE mo_screen->mv_view_type.
      WHEN mo_scr_options->view_type-screen.
        lo_window = CAST #( mo_screen->mo_parent ).

      WHEN OTHERS.    "mo_scr_options->view_type-window.
        lo_window = CAST #( mo_screen ).
    ENDCASE.

    rv_no_value_check = lo_window->no_value_check_is_required( ).
  ENDMETHOD.                    "no_value_check_is_required


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Release fields and instances for garbage collection
    "-----------------------------------------------------------------*
    FREE: mo_screen,
          mo_fcodes,
          mo_scr_fld_attr,
          mo_scr_options,
          mra_excl_func_for_value_check,
          mv_frame_label,
          mv_repid,
          mv_dynnr,
          mv_screen_name.
  ENDMETHOD.                    "on_closed


  METHOD on_process_event.
    "-----------------------------------------------------------------*
    "   Process screen events
    "-----------------------------------------------------------------*
    TRY.
        CASE iv_event.
          WHEN mo_scr_options->event-pbo.
            handle_pbo( iv_event ).

          WHEN mo_scr_options->event-pai.
            handle_pai( iv_event ).

          WHEN mo_scr_options->event-pov.
            handle_pov( iv_event ).
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error) ##catch_all.
        IF iv_event EQ mo_scr_options->event-pai.
          MESSAGE lx_error TYPE c_msgty_e.
        ELSE.
          MESSAGE lx_error TYPE c_msgty_s DISPLAY LIKE c_msgty_e.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "on_process_event


  METHOD on_process_fcode ##needed.
    "-----------------------------------------------------------------*
    "   Handle function code
    "-----------------------------------------------------------------*

    "h a s   t o   b e   r e d i f i n e d

*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*     E X A M P L E   C O D E
*  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
*    "E. g. no transfer into DB in display mode
*    IF mo_screen->mv_mode NE mo_options->mode-modify.
*      RETURN.
*    ENDIF.
*
*    TRY.
*        CASE iv_fcode.
*          WHEN mo_fcodes->save OR
*               mo_fcodes->...
*            "Check data again before saving in DB
*            check_values( ).
*
*            "...
*            update_...( ).
*            close( ).
*
*          WHEN mo_fcodes->enter OR
*               space.
*            "Send e. g. data to subscreen x to refresh data there
*            RAISE EVENT data_changed
*              EXPORTING
*                is_... = m...
*
*          WHEN mo_fcodes->back  OR mo_fcodes->exit.
*            ask_for_confirmation( ).
*
*          WHEN mo_fcodes->cancel.
*            close( ).
*        ENDCASE.
*
*        set_fcode_handled( ).
*
*      CATCH cx_root INTO DATA(lx_error) ##catch_all.
*        set_fcode_handled( ).
*        MESSAGE lx_error TYPE c_msgty_e.
*    ENDTRY.
  ENDMETHOD.                    "on_process_fcode


  METHOD refresh.
    "-----------------------------------------------------------------*
    "   Trigger function code REFRESH
    "-----------------------------------------------------------------*
    IF mo_screen IS NOT INITIAL.
      mo_screen->refresh( ).
    ENDIF.
  ENDMETHOD.                    "refresh


  METHOD set_fcode_handled.
    "-----------------------------------------------------------------*
    "   Initialize function code to space - execution is complete
    "-----------------------------------------------------------------*
    IF mo_screen IS BOUND.
      mo_screen->set_fcode( space ).
    ENDIF.
  ENDMETHOD.                    "set_fcode_handled


  METHOD set_frame_label.
    "-----------------------------------------------------------------*
    "   Use at PBO: Set frame description if passed via constructor
    "-----------------------------------------------------------------*
    IF mv_frame_label IS NOT INITIAL.
      exchange_data(
              EXPORTING
                iv_event = mo_scr_options->event-pbo
                iv_fname = |{ c_frame_label }{ mv_dynnr }|
              CHANGING
                cv_data  = mv_frame_label ).
    ENDIF.
  ENDMETHOD.                    "set_frame_label

ENDCLASS.
