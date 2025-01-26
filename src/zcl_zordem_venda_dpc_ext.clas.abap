CLASS zcl_zordem_venda_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zordem_venda_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS cabecalhoset_create_entity
        REDEFINITION .
    METHODS cabecalhoset_delete_entity
        REDEFINITION .
    METHODS cabecalhoset_get_entity
        REDEFINITION .
    METHODS cabecalhoset_get_entityset
        REDEFINITION .
    METHODS cabecalhoset_update_entity
        REDEFINITION .
    METHODS check_subscription_authority
        REDEFINITION .
    METHODS itemset_create_entity
        REDEFINITION .
    METHODS itemset_delete_entity
        REDEFINITION .
    METHODS itemset_get_entity
        REDEFINITION .
    METHODS itemset_get_entityset
        REDEFINITION .
    METHODS itemset_update_entity
        REDEFINITION .
    METHODS mensagemset_create_entity
        REDEFINITION .
    METHODS mensagemset_get_entity
        REDEFINITION .
    METHODS mensagemset_get_entityset
        REDEFINITION .
    METHODS mensagemset_update_entity
        REDEFINITION .
    METHODS mensagemset_delete_entity
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zordem_venda_dpc_ext IMPLEMENTATION.


  METHOD cabecalhoset_create_entity.    " Criar cabeçalho
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
      FROM zovcabecalho.

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


  METHOD cabecalhoset_delete_entity.

  ENDMETHOD.


  METHOD cabecalhoset_get_entity.   " Cabeçalho específico

    DATA: ld_ordemid TYPE zovcabecalho-ordemid,
          ls_key_tab LIKE LINE OF it_key_tab,   " Pegando a chave
          ls_cab     TYPE zovcabecalho.         " Dados do banco

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
    IF sy-subrc NE 0.
      lo_msg->add_message_text_only(
        EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'ID da ordem não informado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " Setando a ordem encontrada no read table par ao campo que vai no where
    ld_ordemid = ls_key_tab-value.

    SELECT SINGLE *
       INTO ls_cab
       FROM zovcabecalho
       WHERE ordemid = ld_ordemid.

    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_cab TO er_entity.

      er_entity-criadopor = ls_cab-criacao_usuario.

      CONVERT
         DATE ls_cab-criacao_data
         TIME ls_cab-criacao_hora
         INTO TIME STAMP er_entity-datacriacao
         TIME ZONE sy-zonlo.    " UTC

    ELSE.
      lo_msg->add_message_text_only(
      EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'ID da ordem não encontrado'
    ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD cabecalhoset_get_entityset.    " Todos os cabeçalhos

    DATA: lt_cab       TYPE STANDARD TABLE OF zovcabecalho,
          ls_cab       TYPE zovcabecalho,
          " variável tem a mesma estrutura de uma linha individual da tabela interna.
          " usada para armazenar uma linha da tabela interna referenciada.
          ls_entityset LIKE LINE OF et_entityset.

    " Ordernação
    DATA: lt_orderby TYPE STANDARD TABLE OF string,
          ld_orderby TYPE string.

    " Essa rotina serve para converter para setar DESCENDING ou
    "   ASCENDING para criar o orderby dinâmico

    " IT_ORDER type /IWBEP/S_MGW_SORTING_ORDER
    " Estrutura com 2 campos: ORDER e PROPERTY
    LOOP AT it_order INTO DATA(ls_order).
      " OBS: Translate funciona da mesma forma que a func
      "      Modifica para TO UPPER e salva na mesma variável
      " Necessário para padronizar o case das palavras que vem na URI
      ls_order-property = to_upper( ls_order-property ).
      TRANSLATE ls_order-order TO UPPER CASE.

      IF ls_order-order = 'DESC'.
        ls_order-order = 'DESCENDING'.
      ELSE.
        ls_order-order = 'ASCENDING'.
      ENDIF.

      " Append com toda a string de property + order
      APPEND |{ ls_order-property } { ls_order-order }|
          TO lt_orderby.

    ENDLOOP.

    " Concatenação necessária, pois o que é passado para o select é o tipo de dados
    "       e não a tabela
    CONCATENATE LINES OF lt_orderby INTO ld_orderby SEPARATED BY ', '.

    " ordenação obrigatória caso nenhuma seja definida
    " foi feito assim pois dará erro se não passar nada junto com offset da paginação
    IF ld_orderby = '' .
      ld_orderby = 'OrdemId ASCENDING'.
    ENDIF.

    " Select para paginação é declarado de forma diferente, conforme abaixo
*    SELECT *
*       INTO TABLE lt_cab
*       FROM zovcabecalho
*       WHERE (iv_filter_string)
*       ORDER BY (ld_orderby).

    " Select com a paginação
    SELECT *
        FROM zovcabecalho
        WHERE (iv_filter_string)
        ORDER BY (ld_orderby)
        INTO TABLE @lt_cab
        UP TO @is_paging-top ROWS
        OFFSET @is_paging-skip.

    LOOP AT lt_cab INTO ls_cab.
      CLEAR ls_entityset.
      MOVE-CORRESPONDING ls_cab TO ls_entityset.

      " Atribuindo direto, pois o nome do campo de et_entityset é diferente de ls_cab
      ls_entityset-criadopor = ls_cab-criacao_usuario.

      " Conversão pelo mesmo motivo do post do cabeçalho.
      " Na tabela os campos de data e hora são individ  uais e na requisição é um só
      CONVERT
          DATE ls_cab-criacao_data
          TIME ls_cab-criacao_hora
          INTO TIME STAMP ls_entityset-datacriacao
          TIME ZONE sy-zonlo.

      " ls_entityset é uma linha (like line of)
      " Então precisa de fazer o append para a tabela de entidade
      APPEND ls_entityset TO et_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD cabecalhoset_update_entity.

  ENDMETHOD.


  METHOD check_subscription_authority.
  ENDMETHOD.


  METHOD itemset_create_entity.     " Criar item

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


  METHOD itemset_delete_entity.

  ENDMETHOD.


  METHOD itemset_get_entity.

    DATA: ls_key_tab LIKE LINE OF it_key_tab,
          ls_item    TYPE zovitem_ord,
          ld_error   TYPE flag.    " Booleano

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    "input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
    " PAREI AQUI



  ENDMETHOD.


  METHOD itemset_get_entityset.     " Retorna itens baseado na ordemId (cabeçalho)

    DATA: ld_ordemid       TYPE int4,                       " Local data
          lt_ordemid_range TYPE RANGE OF int4,              " Range de ID de ordem
          ls_ordemid_range LIKE LINE OF lt_ordemid_range,   " Linha da tabela de range de ID Ordem
          ls_key_tab       LIKE LINE OF it_key_tab.         " Linha da tabela de chaves (ordemId)

    " Input
    " Aqui vamos passar o ID do cabeçalho da ordem de venda na request
    " Passando a chave, vamos retornar apenas os Itens dessa Ordem específica
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
    IF sy-subrc EQ 0.
      ld_ordemid = ls_key_tab-value.

      CLEAR ls_ordemid_range.

      ls_ordemid_range-sign = 'I'.          " Include
      ls_ordemid_range-option = 'EQ'.       " Equal - Somente itens da Ordem especificada
      ls_ordemid_range-low = ld_ordemid.    " Limite inferior do intervalo
      " Esse último campo poderia ser passado direto, conforme abaixo
      " ls_ordemid_range-low = ls_key_tab-value.

      " Setando na tabela de range de ordemId
      APPEND ls_ordemid_range TO lt_ordemid_range.
    ENDIF.

    " Setando na et_entityset de acordo com os ids do range
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_entityset
    FROM zovitem_ord
   WHERE ordemid IN lt_ordemid_range.

  ENDMETHOD.


  METHOD itemset_update_entity.

  ENDMETHOD.


  METHOD mensagemset_create_entity.

  ENDMETHOD.


  METHOD mensagemset_delete_entity.

  ENDMETHOD.


  METHOD mensagemset_get_entity.

  ENDMETHOD.


  METHOD mensagemset_get_entityset.

  ENDMETHOD.


  METHOD mensagemset_update_entity.

  ENDMETHOD.
ENDCLASS.

