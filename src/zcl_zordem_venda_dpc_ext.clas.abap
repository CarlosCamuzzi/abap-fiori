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

    " Objeto de mensagens para quem estiver consumindo o serviço
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Pegar os dados da request e copiar para entidade er_entity
    " Todos os cmapos definidos na SEGW, no cabeçalho, estão nessa estrutura
    " A er_entity será retornada ao chamador
    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

   " Copiando os campos da entidade para a estrura tabela
   " A tabela pode ter algum campo diferente do que tem na entidade, mandt por ex
   " Então, movemos os campos correspondentes para ls_cab, que é um type tabela zovcabecalho
   " Depois podemos preencher o que faltou, como mostrado abaixo
    MOVE-CORRESPONDING er_entity TO ls_cab.

    " Preenchendo outros dados que não vem na request
    ls_cab-criacao_data    = sy-datum.
    ls_cab-criacao_hora    = sy-uzeit.
    ls_cab-criacao_usuario = sy-uname.

   " Último ID, incrementando e salva no banco
    SELECT SINGLE MAX( ordemid )
      INTO ld_lastid
      FROM zovcab.

    ls_cab-ordemid = ld_lastid + 1.
    INSERT zovcabecalho FROM ls_cab.

  " Verifica se o insert deu certo
  " Se estiver errado, o objeto de msg retorna a msg
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir ordem'
      ).

      " Lança a exceção
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " Copiando os dados do cabeçalho para e entidade para quem chamar request
    MOVE-CORRESPONDING ls_cab TO er_entity.

    " Convertendo para o formato da entidade er_entity
    " É preciso fazer essa conversão pois, na tabela os campos de data e hora são individuais,
    "   e na requisição é um só.
    " Dessa forma, é preciso faezr a conversão na er_entity que irá retornar ao chamador
    CONVERT
      DATE ls_cab-criacao_data
      TIME ls_cab-criacao_hora
      INTO TIME STAMP er_entity-datacriacao
      TIME ZONE sy-zonlo. " Fuso horário

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


  METHOD itemset_create_entity.

    DATA: ls_item TYPE zovitem_ord. " Structure para tabela item

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
         es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_item.

  " Caso quem estiver cosumindo o serviço não passar o ID do item,
  "   vamos pegar o último ID this ordem e incrementar o ID do item
    IF er_entity-itemid EQ 0.
      SELECT SINGLE MAX( itemid )
        INTO er_entity-itemid
        FROM zovitem_ord
        WHERE ordemid = er_entity-ordemid.

      ls_item-itemid = er_entity-itemid = er_entity-itemid + 1.
      ENDIF.


    INSERT zovitem_ord FROM ls_item.

    IF sy-subrc NE 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


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
