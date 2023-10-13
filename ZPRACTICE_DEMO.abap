*&---------------------------------------------------------------------*
*& Report ZPRACTICE_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpractice_demo.
TABLES:mara.
DATA:lv_matnr TYPE mara-matnr,
     wa_lay   TYPE slis_layout_alv,
     lt_fcat  TYPE slis_t_fieldcat_alv.

TYPES:BEGIN OF ty_final,
        light,
        matnr TYPE matnr,
        maktx TYPE maktx,
        aufnr TYPE aufnr,
        sel,
      END OF ty_final.

DATA:lt_final TYPE TABLE OF ty_final,
     wa_final TYPE ty_final.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS :so_mat FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK b1.


SELECT matnr
  FROM mara
  INTO TABLE @DATA(lt_mara)
  WHERE matnr IN @so_mat.
IF sy-subrc = 0.
  SELECT matnr,maktx
    FROM makt
    INTO TABLE @DATA(lt_makt)
    FOR ALL ENTRIES IN @lt_mara
   WHERE matnr = @lt_mara-matnr
    AND spras = 'E'.
  IF sy-subrc = 0.
    SELECT aufnr,plnbez
      FROM afko
      INTO TABLE @DATA(lt_afko)
      FOR ALL ENTRIES IN @lt_makt
      WHERE plnbez = @lt_makt-matnr.
    IF sy-subrc = 0.
      SORT lt_afko BY plnbez.
    ENDIF.
  ENDIF.
ENDIF.

LOOP AT lt_makt INTO DATA(wa_makt)."2
  wa_final-matnr = wa_makt-matnr.
  wa_final-maktx = wa_makt-maktx.

  LOOP AT lt_afko INTO DATA(wa_afko) WHERE plnbez = wa_makt-matnr."2
    DATA(flag) = abap_true.
    wa_final-light = '3'.
    wa_final-aufnr = wa_afko-aufnr.
    APPEND wa_final TO lt_final.
    CLEAR:wa_afko.
  ENDLOOP.

  IF flag =  abap_false.
    wa_final-light = '1'.
    APPEND wa_final TO lt_final.
  ENDIF.
  CLEAR:wa_makt,wa_afko,flag.
ENDLOOP.

*Layout
wa_lay-lights_fieldname = 'LIGHT'.
WA_lay-zebra = 'X'.
wa_lay-colwidth_optimize = 'X'.

*FILEDCAT
lt_fcat = VALUE #( ( col_pos = 1 fieldname = 'MATNR' outputlen = 10 seltext_m = 'Material' Key = 'X' hotspot = 'X' )
                  ( col_pos = 2 fieldname = 'MAKTX' outputlen = 15 seltext_m = 'Description'  )
                  ( col_pos = 3 fieldname = 'AUFNR' outputlen = 10 seltext_m = 'Process Order' )
                  ( col_pos = 4 fieldname = 'SEL' outputlen = 1 seltext_m = 'Check Box' checkbox = 'X' edit = 'X' ) ).

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program      = sy-repid
*   I_CALLBACK_PF_STATUS_SET          = ' '
    i_callback_user_command = 'USR_CMD'
    is_layout               = wa_lay
    it_fieldcat             = lt_fcat
    i_save                  = 'A'
  TABLES
    t_outtab                = lt_final
  EXCEPTIONS
    program_error           = 1
    OTHERS                  = 2.

FORM usr_cmd USING action TYPE sy-ucomm
                   w_selfield TYPE slis_selfield.
  IF action = '&IC1'.
    IF w_selfield-fieldname NE 'MATNR'.
      MESSAGE :'Please click on material number' TYPE 'E'.
    ELSE.
      lv_matnr = w_selfield-value.
      SET PARAMETER ID 'MAT' FIELD lv_matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.
