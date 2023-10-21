"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen / dynpro fw - GUI Status</p>
CLASS zcl_ca_scr_fw_status DEFINITION PUBLIC
                                      CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Description</p>
      mt_excl_fcode     TYPE zca_tt_excl_fcode READ-ONLY,

*     s i n g l e   v a l u e s
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

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Reset / initialize excluded function codes</p>
      reset_excl_fcodes,

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
          iv_titlebar       TYPE gui_title OPTIONAL
          iv_titlebar_repid TYPE syrepid OPTIONAL
          iv_titlebar_var1  TYPE string  OPTIONAL
          iv_titlebar_var2  TYPE string  OPTIONAL
          iv_titlebar_var3  TYPE string  OPTIONAL
          iv_titlebar_var4  TYPE string  OPTIONAL
          iv_titlebar_var5  TYPE string  OPTIONAL.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_scr_fw_status IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "constructor


  METHOD reset_excl_fcodes.
    "-----------------------------------------------------------------*
    "   Reset / initialize excluded function codes
    "-----------------------------------------------------------------*
    CLEAR mt_excl_fcode.
  ENDMETHOD.                    "reset_excl_fcodes


  METHOD set_excl_fcode.
    "-----------------------------------------------------------------*
    "   Set excluded function codes
    "-----------------------------------------------------------------*
    APPEND LINES OF it_fcodes TO mt_excl_fcode.
  ENDMETHOD.                    "set_excl_fcode


  METHOD set_pfstatus.
    "-----------------------------------------------------------------*
    "   Set GUI status
    "-----------------------------------------------------------------*
    mv_pfstatus = iv_pfstatus.

    IF iv_pfstatus_repid IS SUPPLIED.
      mv_pfstatus_repid = iv_pfstatus_repid.
    ENDIF.
  ENDMETHOD.                    "set_pfstatus


  METHOD set_titlebar.
    "-----------------------------------------------------------------*
    "   Set title bar
    "-----------------------------------------------------------------*
    IF iv_titlebar IS SUPPLIED.
      mv_titlebar = iv_titlebar.
    ENDIF.

    IF iv_titlebar_repid IS SUPPLIED.
      mv_titlebar_repid = iv_titlebar_repid.
    ENDIF.

    mv_titlebar_var1 = iv_titlebar_var1.
    mv_titlebar_var2 = iv_titlebar_var2.
    mv_titlebar_var3 = iv_titlebar_var3.
    mv_titlebar_var4 = iv_titlebar_var4.
    mv_titlebar_var5 = iv_titlebar_var5.
  ENDMETHOD.                    "set_titlebar

ENDCLASS.
