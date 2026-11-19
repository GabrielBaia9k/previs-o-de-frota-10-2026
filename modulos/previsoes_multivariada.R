# ==============================================================================
# previsoes_multivariada.R | MULTIVARIADA-FRONTEND-1
# Template: secoes de projecoes/validacao de modulos/previsoes_todos.R.
# Carregar depois desse modulo: leitores, metricas, graficos e exportadores pt_*
# sao compartilhados. Este arquivo define somente UI/servidor Shiny.
#
# Entrada dos produtores: data/dados_mensais_multivariados.csv.
# Scripts locais: Previsoes_validacao_temporal_multivariada/ (P maiusculo).
# Resultados: Previsoes_output/multivariada/previsoes_<modelo>_pre_comp.csv;
# GRU conserva previsoes_gru_global_pre_comp.csv. Validacoes ficam em
# multivariada/validacao_temporal/MODELO/previsoes_<modelo>_teste_<inicio>_2025.csv;
# na janela anual: previsoes_<modelo>_teste_2025.csv.
# CSV: Localidade,Data,Real,Previsao_MODELO; incluir Real do historico de treino.
# Os produtores sao executados somente na maquina local, fora do Shiny.
# ==============================================================================

previsoes_multivariada_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      bs4Dash::box(
        title = "Análise Multivariada",
        width = 12, status = "primary", solidHeader = FALSE,
        shiny::tags$p(
          "Resultados das previsões com variáveis explicativas, gerados pelos scripts locais. ",
          "Selecione a localidade e recarregue os arquivos após uma nova execução."
        ),
        shiny::fluidRow(
          shiny::column(8, shiny::selectInput(ns("select_local"),
            "MUNICÍPIO OU ESTADO", choices = character())),
          shiny::column(4, shiny::tags$br(),
            shiny::actionButton(ns("recarregar"), "Recarregar arquivos"))
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Comparação das previsões multivariadas por algoritmo",
        width = 12, status = "success", solidHeader = FALSE, maximizable = TRUE,
        shiny::uiOutput(ns("status_projecoes")),
        echarts4r::echarts4rOutput(ns("grafico_projecoes"),
          width = "100%", height = "520px"),
        shiny::tags$p(
          "Clique nos nomes da legenda para ocultar ou mostrar as curvas. ",
          "A seleção da legenda afeta somente este gráfico."
        ),
        shiny::tags$p(
          "As métricas usam pares de Real e Previsão disponíveis no CSV de cada modelo. ",
          "Projeções futuras sem observação não entram nos erros. MAPE exclui Real igual a zero; ",
          "as demais métricas mantêm esses meses. R² pode ser negativo ou indefinido."
        ),
        shiny::tags$p(
          "ME = Real − Previsão: positivo indica subestimação. MASE e RMSSE usam ",
          "escala sazonal de 12 meses do histórico de 2016–2024. RelMAE usa o SARIMAX ",
          "multivariado nos mesmos meses e valores reais. Escalas ou referências ",
          "indisponíveis aparecem como N/A."
        ),
        shiny::selectInput(ns("periodo_metricas"), "PERÍODO PARA AS MÉTRICAS",
          choices = c("Meses em comum entre modelos com observações" = "comum",
            "Meses disponíveis de cada modelo" = "individual"),
          selected = "comum", width = "100%"),
        shiny::uiOutput(ns("status_metricas")),
        DT::dataTableOutput(ns("tabela_erros_projecoes")),
        shiny::tags$details(
          shiny::tags$summary("Ver valores mensais das projeções"),
          DT::dataTableOutput(ns("tabela_resultados"))
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Validação temporal — modelos multivariados",
        width = 12, status = "info", solidHeader = FALSE, maximizable = TRUE,
        shiny::tags$p(
          "Cada rodada corresponde a um corte de treinamento e prevê até dezembro de 2025. ",
          "Escolha a janela e a localidade para comparar os arquivos disponíveis. ",
          "Os erros são calculados ao abrir esta seção."
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("ano_validacao"), "JANELA DE TESTE",
            choices = c("2021–2025 (60 meses)" = "2021", "2022–2025 (48 meses)" = "2022",
              "2023–2025 (36 meses)" = "2023", "2024–2025 (24 meses)" = "2024",
              "Últimos 12 meses (2025)" = "2025"), selected = "2024")),
          shiny::column(3, shiny::selectInput(ns("tipo_local_validacao"), "ABRANGÊNCIA",
            choices = c("Pará (Geral)" = "geral", "Município" = "municipio"), selected = "geral")),
          shiny::column(5, shiny::selectInput(ns("local_validacao"),
            "LOCALIDADE DA VALIDAÇÃO", choices = character()))
        ),
        shiny::selectInput(ns("periodo_metricas_validacao"),
          "PERÍODO DAS MÉTRICAS DA VALIDAÇÃO",
          choices = c("Meses comuns aos modelos com dados observados" = "comum",
            "Período disponível de cada modelo" = "individual"), selected = "comum"),
        shiny::tags$div(
          style = "display:flex;align-items:center;flex-wrap:wrap;gap:6px;margin:4px 0 8px;",
          shiny::downloadButton(ns("baixar_previsoes_validacao_csv"), "Previsões CSV",
            class = "btn-sm", style = "padding:3px 8px;font-size:12px;",
            title = "Baixar previsões de todas as janelas e modelos multivariados da localidade selecionada"),
          shiny::downloadButton(ns("baixar_metricas_validacao_csv"), "Métricas CSV",
            class = "btn-sm", style = "padding:3px 8px;font-size:12px;",
            title = "Baixar métricas totais e anuais de todas as janelas da localidade selecionada"),
          shiny::tags$small(class = "text-muted", "Todas as janelas da localidade selecionada.")
        ),
        shiny::uiOutput(ns("status_exportacao_validacao")),
        shiny::uiOutput(ns("status_validacao")),
        echarts4r::echarts4rOutput(ns("grafico_validacao"), width = "100%", height = "460px"),
        shiny::tags$p(
          "A linha tracejada marca o fim do treino. Somente os meses da janela de teste ",
          "entram nos erros. MASE e RMSSE usam a escala sazonal do treino desta rodada. ",
          "RelMAE usa o SARIMAX multivariado da mesma janela; sem referência válida, N/A. ",
          "A legenda filtra somente as curvas; as tabelas seguem o período escolhido acima."
        ),
        shiny::tags$h4("Erros do período completo"),
        DT::dataTableOutput(ns("metricas_validacao")),
        shiny::tags$h4("Erros por ano previsto"),
        shiny::tags$p(
          "As janelas têm durações diferentes. Compare um mesmo ano observado entre ",
          "as origens, considerando a distância de cada corte."
        ),
        DT::dataTableOutput(ns("anuais_validacao")),
        shiny::tags$details(
          shiny::tags$summary("Ver valores mensais e erros do teste"),
          DT::dataTableOutput(ns("mensal_validacao"))
        )
      )
    )
  )
}

previsoes_multivariada_server <- function(
    id, previsoes_dir = here::here("Previsoes_output", "multivariada")
) {
  shiny::moduleServer(id, function(input, output, session) {

    # 1. Leitura compartilhada em cache proprio. Recarregar invalida os CSVs.
    # Nenhum objeto global ou diretorio de resultados antigos e usado como fallback.
    arquivos <- shiny::reactive({
      input$recarregar
      pt_carregar_previsoes(previsoes_dir)
    })
    listar_avisos <- function(avisos, titulo = "Avisos dos arquivos") {
      avisos <- unique(avisos)
      if (!length(avisos)) return(NULL)
      shiny::tags$details(shiny::tags$summary(titulo),
        shiny::tags$ul(lapply(avisos, shiny::tags$li)))
    }

    # 2. Localidade resolvida tambem no servidor, antes da atualizacao do navegador.
    localidades_projecoes <- shiny::reactive({
      locais <- unique(arquivos()$dados$Localidade)
      locais <- sort(locais[!is.na(locais) & nzchar(locais)])
      c(intersect("Pará", locais), setdiff(locais, "Pará"))
    })
    localidade_projecoes <- shiny::reactive({
      locais <- localidades_projecoes()
      atual <- input$select_local
      indice <- if (length(atual) == 1L) match(pt_chave_localidade(atual),
        pt_chave_localidade(locais)) else NA_integer_
      if (length(indice) == 1L && !is.na(indice)) locais[indice] else head(locais, 1L)
    })
    shiny::observe({
      locais <- localidades_projecoes()
      shiny::updateSelectInput(session, "select_local", choices = locais,
        selected = shiny::isolate(localidade_projecoes()))
    })
    projecoes_comparacao <- shiny::reactive({
      carregado <- arquivos()
      shiny::validate(shiny::need(nrow(carregado$dados) > 0L,
        "Ainda não há previsões multivariadas disponíveis. Gere os resultados localmente e recarregue os arquivos."))
      localidade <- localidade_projecoes()
      shiny::req(length(localidade) == 1L)
      projecoes <- pt_dados_projecoes_comparacao(carregado$dados, localidade)
      shiny::validate(shiny::need(nrow(projecoes) > 0L,
        "Não há previsões disponíveis para esta localidade."))
      list(projecoes = projecoes, completos = carregado$dados)
    })
    output$status_projecoes <- shiny::renderUI({
      carregado <- arquivos()
      if (!nrow(carregado$dados)) return(shiny::tagList(
        shiny::tags$p("Ainda não há previsões multivariadas disponíveis. ",
          "Após gerar os resultados na máquina local, clique em Recarregar arquivos."),
        listar_avisos(carregado$avisos)))
      localidade <- localidade_projecoes()
      shiny::req(length(localidade) == 1L)
      dados <- pt_dados_projecoes_comparacao(carregado$dados, localidade)
      modelos <- pt_modelos[pt_modelos %in% unique(dados$Modelo)]
      reais <- pt_reais_projecao(carregado$dados, localidade)
      divergencias <- sum(reais$Valores_distintos > 1L)
      shiny::tagList(
        shiny::tags$p("Localidade: ", localidade, ". Modelos multivariados: ",
          if (length(modelos)) paste(modelos, collapse = ", ") else "nenhum", "."),
        if (divergencias > 0L) shiny::tags$p(style = "color:#9a3412;",
          "Os modelos apresentam valores reais diferentes em ", divergencias,
          " meses. A linha real segue a prioridade do leitor compartilhado; ",
          "cada métrica usa o Real do próprio CSV."),
        listar_avisos(carregado$avisos)
      )
    })
    output$grafico_projecoes <- echarts4r::renderEcharts4r({
      dados <- projecoes_comparacao()
      pt_desenhar_projecoes(dados$projecoes, dados$completos, localidade_projecoes())
    })

    # 3. Metricas em runtime: mesmas formulas, escalas, cores e tabelas do painel.
    metricas_projecoes <- shiny::reactive({
      dados <- projecoes_comparacao()
      periodo <- if (isTRUE(input$periodo_metricas == "individual")) "individual" else "comum"
      pt_tabela_erros_projecoes(dados$projecoes, dados$completos,
        localidade_projecoes(), periodo = periodo)
    })
    output$status_metricas <- shiny::renderUI({
      tabela <- metricas_projecoes()
      shiny::req(nrow(tabela) > 0L)
      escala <- attr(tabela, "escala_sazonal")
      sem_referencia <- tabela$Periodos_validos > 0L & !is.finite(tabela$RelMAE)
      shiny::tagList(
        shiny::tags$p(shiny::tags$small("Escala sazonal 2016–2024: Real de ",
          if (is.na(escala$fonte)) "indisponível" else escala$fonte,
          "; ", escala$pares, " pares anuais conhecidos.")),
        if (!is.finite(escala$mae) || escala$mae <= 0 ||
            !is.finite(escala$mse) || escala$mse <= 0) shiny::tags$p(
          shiny::tags$small("MASE/RMSSE indisponíveis por escala histórica sem pares válidos ou nula.")),
        if (any(sem_referencia)) listar_avisos(paste0(tabela$Modelo[sem_referencia], ": ",
          tabela$Motivo_RelMAE[sem_referencia]), "Referência RelMAE")
      )
    })
    output$tabela_erros_projecoes <- DT::renderDataTable({
      tabela <- metricas_projecoes()
      shiny::validate(shiny::need(nrow(tabela) > 0L && any(tabela$Periodos_validos > 0L),
        "Não há meses observados com previsão para calcular as métricas neste período."))
      pt_widget_erros(tabela)
    })
    output$tabela_resultados <- DT::renderDataTable({
      dados <- projecoes_comparacao()
      tabela <- pt_tabela_previsoes(pt_filtrar_previsoes_localidade(
        dados$completos, localidade_projecoes()))
      tabela$Data <- format(tabela$Data, "%m/%Y")
      DT::datatable(tabela, rownames = FALSE,
        options = list(pageLength = 12, scrollX = TRUE, ordering = FALSE,
          columnDefs = list(list(targets = "_all", defaultContent = "N/A")))) |>
        DT::formatRound(columns = setdiff(names(tabela), "Data"), digits = 2)
    })

    # 4. Validacao: janela, abrangencia e localidade independentes das projecoes.
    arquivos_validacao <- shiny::reactive({
      input$recarregar
      ano <- if (length(input$ano_validacao)) input$ano_validacao else 2024L
      pt_carregar_validacao(previsoes_dir, ano, ano_fim_teste = 2025L)
    })
    tipo_validacao <- shiny::reactive({
      if (isTRUE(input$tipo_local_validacao == "municipio")) "municipio" else "geral"
    })
    localidades_validacao <- shiny::reactive({
      pt_localidades_validacao(arquivos_validacao()$dados, tipo_validacao())
    })
    localidade_validacao <- shiny::reactive({
      locais <- localidades_validacao()
      atual <- input$local_validacao
      if (length(atual) == 1L && !is.na(atual) && atual %in% locais) atual else head(locais, 1L)
    })
    shiny::observe({
      locais <- localidades_validacao()
      rotulos <- ifelse(pt_chave_localidade(locais) == "PARA", "Pará (Geral)", locais)
      shiny::updateSelectInput(session, "local_validacao",
        choices = stats::setNames(locais, rotulos),
        selected = shiny::isolate(localidade_validacao()))
    })
    dados_validacao <- shiny::reactive({
      carregado <- arquivos_validacao()
      shiny::validate(shiny::need(nrow(carregado$dados) > 0L,
        "Ainda não há validações multivariadas desta janela. Gere os resultados localmente e recarregue os arquivos."))
      shiny::validate(shiny::need(length(localidades_validacao()) > 0L,
        if (tipo_validacao() == "municipio") "Não há resultados municipais nesta janela." else
          "Não há resultado de Pará (Geral) nesta janela."))
      localidade <- localidade_validacao()
      shiny::req(length(localidade) == 1L)
      periodo <- if (isTRUE(input$periodo_metricas_validacao == "individual")) "individual" else "comum"
      pt_avaliar_validacao(carregado, localidade, periodo = periodo)
    })
    output$status_validacao <- shiny::renderUI({
      carregado <- arquivos_validacao()
      if (!nrow(carregado$dados)) return(shiny::tagList(
        shiny::tags$p("Ainda não há validações multivariadas para ", carregado$ano_teste,
          "–", carregado$ano_fim_teste, ". Gere os resultados na máquina local e recarregue os arquivos."),
        listar_avisos(carregado$avisos, "Avisos da rodada")))
      if (!length(localidades_validacao())) return(shiny::tagList(
        shiny::tags$p(if (tipo_validacao() == "municipio")
          "Não há resultados municipais nesta janela." else "Não há resultado de Pará (Geral) nesta janela."),
        listar_avisos(carregado$avisos, "Avisos da rodada")))
      dados <- dados_validacao()
      escala <- attr(dados$metricas, "escala_sazonal")
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong("Localidade: "), localidade_validacao(), ". ",
          shiny::tags$strong("Teste: "), format(carregado$inicio_teste, "%m/%Y"), " a ",
          format(carregado$fim_teste, "%m/%Y"), ". Fim do treino: ",
          format(dados$fim_treino, "%m/%Y"), "."),
        shiny::tags$ul(lapply(seq_len(nrow(dados$cobertura)), function(i) {
          linha <- dados$cobertura[i, ]
          shiny::tags$li(shiny::tags$strong(paste0(linha$Modelo, ": ")),
            linha$Meses_pareados, " de ", carregado$meses_teste,
            " meses com Real e Previsão; ", linha$Meses_avaliados, " avaliados. ",
            "Histórico de treino no CSV: ", if (is.na(linha$Inicio_treino)) "não informado" else
              paste0(format(linha$Inicio_treino, "%m/%Y"), " a ", format(linha$Fim_treino, "%m/%Y")), ".")
        })),
        shiny::tags$p(shiny::tags$small(if (dados$periodo == "comum") paste0(
          "Métricas nos meses comuns: ", dados$meses_validos,
          ". Modelos sem pares observados não restringem a amostra dos demais.") else
            "Métricas no período disponível de cada modelo; as amostras podem ser diferentes.")),
        if (!is.null(escala)) shiny::tags$p(shiny::tags$small(
          "Escala sazonal do treino: ", format(escala$inicio, "%m/%Y"), " a ",
          format(escala$fim, "%m/%Y"), "; fonte: ",
          if (is.na(escala$fonte)) "indisponível" else escala$fonte,
          "; ", escala$pares, " pares anuais conhecidos.")),
        listar_avisos(c(carregado$avisos, dados$avisos), "Avisos da rodada")
      )
    })
    output$grafico_validacao <- echarts4r::renderEcharts4r({
      dados <- dados_validacao()
      shiny::validate(shiny::need(nrow(dados$projecoes) > 0L,
        "Não há previsões válidas para desenhar nesta rodada."))
      pt_desenhar_projecoes(dados$projecoes, dados$completos,
        localidade_validacao(), data_corte = dados$fim_treino)
    })
    output$metricas_validacao <- DT::renderDataTable({
      tabela <- dados_validacao()$metricas
      shiny::validate(shiny::need(nrow(tabela) > 0L && any(tabela$Periodos_validos > 0L),
        "Não há meses de teste com Real e Previsão para calcular os erros."))
      pt_widget_erros(tabela)
    })
    output$anuais_validacao <- DT::renderDataTable({
      tabela <- dados_validacao()$metricas_anuais
      shiny::validate(shiny::need(nrow(tabela) > 0L,
        "Não há meses observados para calcular os erros anuais."))
      pt_widget_erros(tabela, ano = tabela$Ano)
    })
    output$mensal_validacao <- DT::renderDataTable({
      tabela <- dados_validacao()$mensal
      tabela <- tabela[c("Modelo", "Data", "Horizonte", "Real", "Previsao",
        "Erro", "Erro_absoluto", "Usado_metricas")]
      tabela$Data <- format(tabela$Data, "%m/%Y")
      tabela$Usado_metricas <- ifelse(tabela$Usado_metricas, "Sim", "Não")
      names(tabela) <- c("Modelo", "Mês", "Horizonte (meses)", "Real", "Previsão",
        "Erro (Real − Previsão)", "Erro absoluto", "Usado nas métricas")
      DT::datatable(tabela, rownames = FALSE,
        options = list(pageLength = 12, lengthChange = FALSE, scrollX = TRUE,
          dom = "tip", ordering = FALSE,
          columnDefs = list(list(targets = "_all", defaultContent = "N/A")))) |>
        DT::formatRound(columns = c("Real", "Previsão", "Erro (Real − Previsão)", "Erro absoluto"), digits = 2)
    })

    # 5. Downloads: caches de todas as origens avaliados somente sob demanda.
    # As duas funcoes de exportacao recebem a mesma preparacao e a mesma amostra.
    arquivos_exportacao_validacao <- shiny::reactive({
      input$recarregar
      stats::setNames(lapply(2021:2025, function(ano) {
        pt_carregar_validacao(previsoes_dir, ano, ano_fim_teste = 2025L)
      }), as.character(2021:2025))
    })
    rodadas_exportacao_validacao <- shiny::reactive({
      localidade <- localidade_validacao()
      shiny::req(length(localidade) == 1L)
      periodo <- if (isTRUE(input$periodo_metricas_validacao == "individual")) "individual" else "comum"
      pt_preparar_exportacao_validacao(arquivos_exportacao_validacao(), localidade, periodo)
    })
    ultimo_download_validacao <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$recarregar, localidade_validacao(), input$periodo_metricas_validacao), {
      ultimo_download_validacao(NULL)
    }, ignoreInit = TRUE)
    nome_download_validacao <- function(tipo) {
      localidade <- localidade_validacao()
      shiny::req(length(localidade) == 1L)
      nome <- tolower(gsub(" ", "_", pt_chave_localidade(localidade), fixed = TRUE))
      periodo <- if (isTRUE(input$periodo_metricas_validacao == "individual")) "individual" else "comum"
      paste0("validacao_multivariada_", tipo, "_", nome, "_2021_2025_", periodo, ".csv")
    }
    concluir_download_validacao <- function(tabela, arquivo, preparado) {
      if (!nrow(tabela)) {
        ultimo_download_validacao(list(anos = character(), avisos = preparado$avisos))
        shiny::showNotification("Sem resultados disponíveis para exportar nesta localidade.", type = "warning")
        shiny::validate(shiny::need(FALSE, "Sem resultados disponíveis para exportar."))
      }
      ultimo_download_validacao(NULL)
      pt_gravar_csv_validacao(tabela, arquivo)
      ultimo_download_validacao(list(anos = names(preparado$rodadas), avisos = preparado$avisos))
    }
    output$baixar_previsoes_validacao_csv <- shiny::downloadHandler(
      filename = function() nome_download_validacao("previsoes"),
      contentType = "text/csv; charset=utf-8",
      content = function(arquivo) {
        preparado <- rodadas_exportacao_validacao()
        concluir_download_validacao(pt_exportar_previsoes_validacao(preparado$rodadas), arquivo, preparado)
      }
    )
    output$baixar_metricas_validacao_csv <- shiny::downloadHandler(
      filename = function() nome_download_validacao("metricas"),
      contentType = "text/csv; charset=utf-8",
      content = function(arquivo) {
        preparado <- rodadas_exportacao_validacao()
        concluir_download_validacao(pt_exportar_metricas_validacao(preparado$rodadas), arquivo, preparado)
      }
    )
    output$status_exportacao_validacao <- shiny::renderUI({
      estado <- ultimo_download_validacao()
      if (is.null(estado)) return(NULL)
      texto <- if (length(estado$anos)) paste0("Janelas incluídas no CSV: ",
        paste(estado$anos, collapse = ", "), ".") else "Nenhuma janela disponível para esta localidade."
      shiny::tagList(shiny::tags$small(class = "text-muted", texto),
        listar_avisos(estado$avisos, "Avisos de cobertura da exportação"))
    })

    invisible(list(arquivos = arquivos, projecoes = projecoes_comparacao,
      validacao = dados_validacao))
  })
}
