"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for screen framework</p>
CLASS zcl_ca_c_scr_fw DEFINITION PUBLIC
                                 FINAL
                                 CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Range table for function codes</p>
      ty_t_func_codes      TYPE RANGE OF syst_ucomm.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Function codes</p>
      BEGIN OF fcodes,
        "! <p class="shorttext synchronized" lang="en">Function code: Refresh</p>
        refresh TYPE syucomm VALUE '&REFRESH'  ##no_text,
      END OF fcodes,

      "! <p class="shorttext synchronized" lang="en">Events</p>
      BEGIN OF event,
        "! <p class="shorttext synchronized" lang="en">Screen event: Execute function code</p>
        fcode    TYPE syst_ucomm VALUE '&FCODE'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen event: Process After Input</p>
        pai      TYPE syst_ucomm VALUE '&PAI'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen event: Process Before Output</p>
        pbo      TYPE syst_ucomm VALUE '&PBO'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen event: Set screen / PF status</p>
        pfstatus TYPE syst_ucomm VALUE 'PFSTATUS'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen event: Process On Value-Request</p>
        pov      TYPE syst_ucomm VALUE '&POV'  ##no_text,
      END OF event,

      "! <p class="shorttext synchronized" lang="en">Screen / dynpro modes</p>
      BEGIN OF mode,
        "! <p class="shorttext synchronized" lang="en">Screen mode: Display</p>
        display TYPE syst_ucomm VALUE '&DISPLAY'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen mode: Modify</p>
        modify  TYPE syst_ucomm VALUE '&MODIFY'  ##no_text,
      END OF mode,

      "! <p class="shorttext synchronized" lang="en">Open as ...</p>
      BEGIN OF open_as,
        "! <p class="shorttext synchronized" lang="en">Open dialog as: Popup</p>
        popup  TYPE abap_boolean VALUE abap_true,
        "! <p class="shorttext synchronized" lang="en">Open dialog as: Normal / Full screen</p>
        screen TYPE abap_boolean VALUE abap_false,
      END   OF open_as,

      "! <p class="shorttext synchronized" lang="en">View types</p>
      BEGIN OF view_type,
        "! <p class="shorttext synchronized" lang="en">View type: Window / Dynpro</p>
        window    TYPE char1   VALUE '1'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">View type: Composite</p>
        composite TYPE char1   VALUE '2'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">View type: Subscreen / View</p>
        screen    TYPE char1   VALUE '3'  ##no_text,
      END OF view_type.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_scr_fw.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid screen framework event passed?</p>
      "!
      "! @parameter event | <p class="shorttext synchronized" lang="en">Event</p>
      is_event_valid
        IMPORTING
          event TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Valid screen / dynpro mode passed?</p>
      "!
      "! @parameter mode | <p class="shorttext synchronized" lang="en">Screen / dynpro mode</p>
      is_mode_valid
        IMPORTING
          mode TYPE syst_ucomm,

      "! <p class="shorttext synchronized" lang="en">Valid screen framework view type passed?</p>
      "!
      "! @parameter view_type | <p class="shorttext synchronized" lang="en">View type</p>
      is_view_type_valid
        IMPORTING
          view_type TYPE char1.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_scr_fw.

ENDCLASS.



CLASS zcl_ca_c_scr_fw IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_scr_fw=>singleton_instance IS NOT BOUND.
      zcl_ca_c_scr_fw=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_scr_fw=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_event_valid.
    "-----------------------------------------------------------------*
    "   Valid screen framework event passed?
    "-----------------------------------------------------------------*
    IF event NE me->event-fcode AND
       event NE me->event-pai   AND
       event NE me->event-pbo   AND
       event NE me->event-pov   AND
       event NE me->event-pfstatus.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'EVENT'
          mv_msgv2 = CONV #( event ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_event_valid


  METHOD is_mode_valid.
    "-----------------------------------------------------------------*
    "   Valid screen / dynpro mode passed?
    "-----------------------------------------------------------------*
    IF mode NE me->mode-display AND
       mode NE me->mode-modify.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'MODE'
          mv_msgv2 = CONV #( mode ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_mode_valid


  METHOD is_view_type_valid.
    "-----------------------------------------------------------------*
    "  Valid screen framework view type passed?
    "-----------------------------------------------------------------*
    IF view_type NE me->view_type-window    AND
       view_type NE me->view_type-composite AND
       view_type NE me->view_type-screen.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'VIEW_TYPE'
          mv_msgv2 = CONV #( view_type ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_view_type_valid

ENDCLASS.
