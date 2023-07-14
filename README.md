# sales_classes_report
sales report with classes

*&---------------------------------------------------------------------*
*& Report ZT_SALES_CLASSES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zt_sales_classes.
TYPES:BEGIN OF ty_vbak,
        vbeln TYPE vbeln_va,
        erdat TYPE erdat,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
        spart TYPE spart,
        vkgrp TYPE vkgrp,
        kunnr TYPE kunag,
      END OF ty_vbak,
      BEGIN OF ty_vbap,
        vbeln TYPE vbeln_va,
        posnr TYPE posnr,
        matnr TYPE matnr,
        netwr TYPE netwr_ap,
        waerk TYPE waerk,
      END OF ty_vbap,
      BEGIN OF ty_kna1,
        kunnr TYPE kunag,
        land1 TYPE land1_gp,
        name1 TYPE name1_gp,
        pstlz TYPE pstlz,
      END OF ty_kna1,
      BEGIN OF ty_mat,
        matnr TYPE matnr,
        mtart TYPE mtart,
        mbrsh TYPE mbrsh,
        meins TYPE meins,
      END OF ty_mat,
      BEGIN OF ty_makt,
        matnr TYPE matnr,
        maktx TYPE maktx,
      END OF ty_makt,
      BEGIN OF ty_fin,
        vbeln TYPE vbeln_va,
        erdat TYPE erdat,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
        spart TYPE spart,
        vkgrp TYPE vkgrp,
        kunnr TYPE kunag,
        posnr TYPE posnr,
        matnr TYPE matnr,
        netwr TYPE netwr_ap,
        waerk TYPE waerk,
        land1 TYPE land1_gp,
        name1 TYPE name1_gp,
        pstlz TYPE pstlz,
        mtart TYPE mtart,
        mbrsh TYPE mbrsh,
        meins TYPE meins,
        maktx TYPE maktx,

      END OF ty_fin.
      TYPES GTT_FIN TYPE TABLE OF TY_FIN.
DATA : gt_fin TYPE STANDARD TABLE OF ty_fin INITIAL SIZE 1,
gs_fin TYPE  ty_fin.

CLASS lcl_sales DEFINITION
 FINAL.
  PUBLIC SECTION.
    METHODS : get_data
      IMPORTING iv_vbeln TYPE vbeln_va
                iv_matnr TYPE matnr OPTIONAL
      EXPORTING et_mat   TYPE mara_tt
                et_vbap  TYPE vbap_t
                ET_FIN   TYPE GTT_FIN
     EXCEPTIONS wrong_input,
       display_output.
  PRIVATE SECTION .

ENDCLASS.
CLASS lcl_sales IMPLEMENTATION .
  METHOD get_data.
    SELECT vbeln,erdat,vkorg,vtweg,spart,vkgrp,kunnr
       FROM vbak INTO TABLE @DATA(lt_vbak)
           WHERE vbeln = @iv_vbeln.
    IF sy-subrc = 0.
      SORT lt_vbak BY vbeln.
      else.
        RAISE wrong_input.
    ENDIF.

    IF lt_vbak IS NOT INITIAL.
      SELECT kunnr,land1,name1,pstlz
        FROM kna1 INTO TABLE @DATA(lt_kna1)
        FOR ALL ENTRIES IN @lt_vbak
        WHERE kunnr = @lT_vbak-kunnr.
      IF sy-subrc = 0.
        SORT lt_kna1 BY kunnr.
      ENDIF.

      SELECT vbeln,posnr,matnr,netwr,waerk
        FROM vbap INTO TABLE @DATA(lt_vbap)
        FOR ALL ENTRIES IN @lt_vbak
        WHERE vbeln = @lt_vbak-vbeln.
      IF sy-subrc = 0.
        SORT lt_vbak BY vbeln.
      ENDIF.

      SELECT matnr,mtart,mbrsh,meins
        FROM mara INTO TABLE @DATA(lt_mat)
        FOR ALL ENTRIES IN @lt_vbap
        WHERE matnr = @lt_vbap-matnr.
      IF sy-subrc = 0.
        SORT lt_mat BY matnr.
      ENDIF.

      SELECT matnr,maktx FROM makt
        INTO TABLE @DATA(lt_makt)
        FOR ALL ENTRIES IN @lt_mat
        WHERE matnr = @lt_mat-matnr.
      IF sy-subrc = 0.
        SORT lt_makt BY matnr.
      ENDIF.

    ENDIF.



    LOOP AT lt_vbap INTO DATA(ls_vbap).
      READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = ls_vbap-vbeln BINARY SEARCH.
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_vbak-kunnr BINARY SEARCH.
      READ TABLE lt_mat INTO DATA(ls_mat)   WITH KEY matnr = ls_vbap-matnr BINARY SEARCH.
      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_mat-matnr BINARY SEARCH.
      gs_fin-vbeln = ls_vbak-vbeln.
      gs_fin-erdat = ls_vbak-erdat.
      gs_fin-vkorg = ls_vbak-vkorg.
      gs_fin-vtweg = ls_vbak-vtweg.
      gs_fin-spart = ls_vbak-spart.
      gs_fin-vkgrp = ls_vbak-vkgrp.
      gs_fin-kunnr = ls_kna1-kunnr.
      gs_fin-posnr = ls_vbap-posnr.
      gs_fin-matnr = ls_vbap-matnr.
      gs_fin-netwr = ls_vbap-netwr.
      gs_fin-waerk = ls_vbap-waerk.
      gs_fin-land1 = ls_kna1-land1.
      gs_fin-name1 = ls_kna1-name1.
      gs_fin-pstlz = ls_kna1-pstlz.
      gs_fin-mtart = ls_mat-mtart.
      gs_fin-mbrsh = ls_mat-mbrsh.
      gs_fin-meins = ls_mat-meins.
      gs_fin-maktx = ls_makt-maktx.
      APPEND gs_fin TO gt_fin.
      CLEAR : ls_vbak,ls_vbap,ls_kna1,ls_mat,ls_makt.
    ENDLOOP.
      ENDMETHOD.
      METHOD display_output.
        cl_demo_output=>display( gt_fin ).
        ENDMETHOD.

ENDCLASS.

PARAMETERS : p_vbeln TYPE vbeln_va,
             p_matnr TYPE matnr.

START-OF-SELECTION.
  DATA lt_sales TYPE REF TO lcl_sales.
  CREATE OBJECT lt_sales.
lt_sales->get_data(
   EXPORTING iv_vbeln = p_vbeln
             iv_matnr = p_matnr
  EXCEPTIONS wrong_input = 1 )
   .
case sy-subrc.
  when '0'.
    lt_sales->display_output( ).

*  cl_demo_output=>display( gt_fin ).
  when '1'.
  MESSAGE 'please Enter the correct Sales Order Number' TYPE 'E'.
  ENDCASE.
