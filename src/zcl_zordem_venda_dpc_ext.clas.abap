class ZCL_ZORDEM_VENDA_DPC_EXT definition
  public
  inheriting from ZCL_ZORDEM_VENDA_DPC
  create public .

public section.
protected section.

  methods CABECALHOSET_CREATE_ENTITY
    redefinition .
  methods CABECALHOSET_DELETE_ENTITY
    redefinition .
  methods CABECALHOSET_GET_ENTITY
    redefinition .
  methods CABECALHOSET_GET_ENTITYSET
    redefinition .
  methods CABECALHOSET_UPDATE_ENTITY
    redefinition .
  methods CHECK_SUBSCRIPTION_AUTHORITY
    redefinition .
  methods ITEMSET_CREATE_ENTITY
    redefinition .
  methods ITEMSET_DELETE_ENTITY
    redefinition .
  methods ITEMSET_GET_ENTITY
    redefinition .
  methods ITEMSET_GET_ENTITYSET
    redefinition .
  methods ITEMSET_UPDATE_ENTITY
    redefinition .
  methods MENSAGEMSET_CREATE_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITYSET
    redefinition .
  methods MENSAGEMSET_UPDATE_ENTITY
    redefinition .
  methods MENSAGEMSET_DELETE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZORDEM_VENDA_DPC_EXT IMPLEMENTATION.


  METHOD cabecalhoset_create_entity.
    DATA: ld_lastid TYPE int4.
    DATA: ls_cab    TYPE zovcabecalho. " table

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_cab.

    ls_cab-criacao_data    = sy-datum.
    ls_cab-criacao_hora    = sy-uzeit.
    ls_cab-criacao_usuario = sy-uname.

    SELECT SINGLE MAX( ordemid )
      INTO ld_lastid
      FROM zovcab.

    ls_cab-ordemid = ld_lastid + 1.
    INSERT zovcabecalho FROM ls_cab.

    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir ordem'
      ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " atualizando
    MOVE-CORRESPONDING ls_cab TO er_entity.
    CONVERT
      DATE ls_cab-criacao_data
      TIME ls_cab-criacao_hora
      INTO TIME STAMP er_entity-datacriacao
      TIME ZONE sy-zonlo.

  ENDMETHOD.


  method CABECALHOSET_DELETE_ENTITY.

  endmethod.


  method CABECALHOSET_GET_ENTITY.

  endmethod.


  method CABECALHOSET_GET_ENTITYSET.

  endmethod.


  method CABECALHOSET_UPDATE_ENTITY.

  endmethod.


  method CHECK_SUBSCRIPTION_AUTHORITY.
  endmethod.


  method ITEMSET_CREATE_ENTITY.

  endmethod.


  method ITEMSET_DELETE_ENTITY.

  endmethod.


  method ITEMSET_GET_ENTITY.

  endmethod.


  method ITEMSET_GET_ENTITYSET.

  endmethod.


  method ITEMSET_UPDATE_ENTITY.

  endmethod.


  method MENSAGEMSET_CREATE_ENTITY.

  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.

  endmethod.


  method MENSAGEMSET_GET_ENTITY.

  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.

  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.

  endmethod.
ENDCLASS.
