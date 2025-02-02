*&---------------------------------------------------------------------*
*& Report ZR_TESTES_TABELAS_API
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_testes_tabelas_api.

DATA lt_id TYPE TABLE OF zovitem_ord-itemid.
lt_id = VALUE #( ( 19 ) ( 20 ) ( 21 ) ( 22 ) ( 23 ) ( 24 ) ( 25 ) ( 26 ) ).

"DELETE FROM zovitem_ord WHERE itemid BETWEEN 19 AND 26.

DELETE FROM zovitem_ord WHERE itemid = 19 AND itemid = 26.
