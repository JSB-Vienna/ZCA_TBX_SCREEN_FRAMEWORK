"! <p class="shorttext synchronized" lang="en">CA-TBX: COPY this FG for your implementation</p>
FUNCTION-POOL zca_scr_fw_copy_pattern.      "MESSAGE-ID ..

* g l o b a l   d e f i n i t i o n s
DATA:
* s t r u c t u r e s
  "! <p class="shorttext synchronized" lang="en">Labels/descriptions for subscreen frames</p>
  BEGIN OF gs_frame_label,
    "! <p class="shorttext synchronized" lang="en">Subscreen 0003: Subscreen pattern</p>
    d0003 TYPE text70,
  END   OF gs_frame_label.


* INCLUDE LZCA_SCR_FW_COPY_PATTERND...       " Local class definition
