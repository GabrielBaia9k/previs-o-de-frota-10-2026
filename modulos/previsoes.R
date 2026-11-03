# ==============================================================================
# previsoes.R - Tabela de Visão Geral (Projeções acumuladas por modelo)
# Padronizado segundo as convenções visuais dos demais módulos do dashboard
# ==============================================================================

previsoes_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    #------------------------------------------------------------------------------#
    ## 1. VALUE BOXES (Padrão do projeto) ----
    fluidRow(
      bs4InfoBoxOutput(ns("vb_frota_2031"), width = 4),
      bs4InfoBoxOutput(ns("vb_crescimento"), width = 4),
      bs4InfoBoxOutput(ns("vb_melhor"), width = 4)
    ),

    tags$br(),

    #------------------------------------------------------------------------------#
    ## 2. Painel de Filtros ----
    fluidRow(
      bs4Dash::box(
        title = tagList(icon("filter"), "Filtros de Projeção da Frota"),
        status = "primary",
        solidHeader = FALSE,
        elevation = 2,
        collapsible = TRUE,
        width = 12,

        fluidRow(
          column(
            3,
            selectInput(
              inputId = ns("filtro_ri"),
              label = "REGIAO DE INTEGRACAO",
              choices = NULL,
              selected = "Todas as Regiões",
              width = "100%"
            )
          ),

          column(
            4,
            selectizeInput(
              inputId = ns("filtro_muni"),
              label = "MUNICIPIOS",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "Todos (clique ou digite para escolher especificamente)",
                plugins = list("remove_button")
              )
            )
          ),

          column(
            3,
            selectInput(
              inputId = ns("filtro_modelo"),
              label = "MODELO DE PREVISÃO",
              choices = c(
                "SARIMAX", "SARIMA", "ARIMA",
                "ETS", "LSTM", "GRU"
              ),
              selected = "SARIMAX",
              width = "100%"
            )
          ),

          column(
            2,
            tags$label(HTML("&nbsp;")),
            actionBttn(
              inputId = ns("reset_prev"),
              label = "🔄 LIMPAR",
              style = "material-flat",
              color = "primary",
              size = "md",
              block = TRUE
            )
          )
        )
      )
    ),

    #------------------------------------------------------------------------------#
    ## 3. Tabela Principal ----
    fluidRow(
      bs4Dash::box(
        title = htmlOutput(ns("txtprev")),
        status = "primary",
        solidHeader = FALSE,
        elevation = 2,
        maximizable = TRUE,
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 12,

        uiOutput(ns("descricao_prev")),

        tags$hr(),

        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tabela_geral_previsoes")),
          type = 8,
          color = "#1D44B8",
          size = 0.8
        ),

        footer = list(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",

            div(
              tags$h6(
                tags$b("Fonte:", style = "font-family: sans-serif;"),
                "DTI/DETRAN-PA",
                style = "margin-bottom: 0;"
              ),

              tags$h6(
                tags$b("Elaboração:"),
                "CNP/DETRAN-PA",
                style = "margin-bottom: 0;"
              )
            ),

            div(
              style = "text-align: center;",
              downset_ui(ns("prevdown"))
            )
          )
        )
      )
    )
  )
}

previsoes_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Período projetado: colunas da tabela.
    anos_prev <- as.character(2026:2031)

    #------------------------------------------------------------------------------#
    # 1. Mapeamento de regiões e municípios
    #------------------------------------------------------------------------------#
    mapa_regioes <- reactive({
      frota_acumulado |>
        dplyr::select(ri, local) |>
        dplyr::distinct() |>
        dplyr::collect() |>
        dplyr::mutate(
          ri = toupper(trimws(ri)),
          local = pt_normalizar_localidade(local),
          Chave_Localidade = pt_chave_localidade(local)
        ) |>
        dplyr::filter(!is.na(local), nzchar(local))
    })

    #------------------------------------------------------------------------------#
    # 2. Inicializar o filtro de regiões
    #------------------------------------------------------------------------------#
    observe({
      df_mapa <- mapa_regioes()

      vetor_ri <- sort(unique(
        df_mapa$ri[
          df_mapa$ri != "" &
            df_mapa$ri != "PARÁ" &
            !is.na(df_mapa$ri)
        ]
      ))

      updateSelectInput(
        session,
        "filtro_ri",
        choices = c("Todas as Regiões", vetor_ri),
        selected = "Todas as Regiões"
      )
    })

    #------------------------------------------------------------------------------#
    # 3. Atualizar municípios conforme a região
    #------------------------------------------------------------------------------#
    observe({
      req(input$filtro_ri)

      df_mapa <- mapa_regioes()

      if (input$filtro_ri == "Todas as Regiões") {
        munis_diponiveis <- sort(unique(
          df_mapa$local[df_mapa$local != "Pará"]
        ))

        munis_diponiveis <- c("Pará", munis_diponiveis)

      } else {
        munis_diponiveis <- df_mapa |>
          dplyr::filter(ri == input$filtro_ri) |>
          dplyr::pull(local) |>
          sort()
      }

      # Preservar somente seleções que pertencem à região atual.
      selecionados <- intersect(
        isolate(input$filtro_muni),
        munis_diponiveis
      )

      updateSelectizeInput(
        session,
        "filtro_muni",
        choices = munis_diponiveis,
        selected = selecionados,
        server = TRUE
      )
    })

    #------------------------------------------------------------------------------#
    # 3.1 Botão Limpar
    #------------------------------------------------------------------------------#
    observeEvent(input$reset_prev, {
      freezeReactiveValue(input, "filtro_ri")
      freezeReactiveValue(input, "filtro_muni")
      freezeReactiveValue(input, "filtro_modelo")

      updateSelectInput(
        session,
        "filtro_ri",
        selected = "Todas as Regiões"
      )

      updateSelectizeInput(
        session,
        "filtro_muni",
        selected = character(0)
      )

      updateSelectInput(
        session,
        "filtro_modelo",
        selected = "SARIMAX"
      )
    })

    #------------------------------------------------------------------------------#
    # 4. Título e descrição conforme o modelo selecionado
    #------------------------------------------------------------------------------#
    output$txtprev <- renderUI({
      req(input$filtro_modelo)

      tags$h4(
        style = "text-align:center; font-weight:bold; margin: 0;",
        paste0(
          "Projeção da Frota por Localidade (2026-2031) — ",
          input$filtro_modelo
        )
      )
    })

    output$descricao_prev <- renderUI({
      req(input$filtro_modelo)

      tags$p(
        style = "text-align: justify; font-size: 11pt;",
        "A tabela abaixo detalha a ",
        tags$strong("estimação total acumulada"),
        " da frota veicular até 2031. Os valores apresentados consideram a base real ",
        "consolidada de dezembro de 2025, acrescida progressivamente das previsões ",
        "mensais do modelo ",
        tags$strong(input$filtro_modelo),
        " para cada localidade."
      )
    })

    #------------------------------------------------------------------------------#
    # 5.1 Base real de 2025
    #------------------------------------------------------------------------------#
    # A chave ignora diferenças de caixa e acentuação.
    # O nome apresentado continua sendo o nome da base real.
    base_2025 <- reactive({
      db01_acumulado |>
        dplyr::filter(ano == "2025") |>
        dplyr::select(local, valor) |>
        dplyr::collect() |>
        dplyr::mutate(
          Localidade = pt_normalizar_localidade(local),
          Chave_Localidade = pt_chave_localidade(Localidade)
        ) |>
        dplyr::group_by(Chave_Localidade) |>
        dplyr::summarise(
          Localidade = dplyr::first(Localidade),
          frota_inicial_2025 = sum(valor, na.rm = TRUE),
          .groups = "drop"
        )
    })

    #------------------------------------------------------------------------------#
    # 5.2 Previsões anuais acumuladas por modelo e localidade
    #------------------------------------------------------------------------------#
    dados_anuais <- reactive({
      req(input$filtro_modelo, input$filtro_ri)

      # Dados padronizados pelo leitor existente, carregados no global.R.
      previsoes <- previsoes_compartilhadas$dados |>
        dplyr::filter(
          Modelo == input$filtro_modelo,
          Data >= as.Date("2026-01-01"),
          Data <= as.Date("2031-12-31")
        ) |>
        dplyr::mutate(
          Chave_Localidade = pt_chave_localidade(
            pt_normalizar_localidade(Localidade)
          )
        )

      # Municípios selecionados têm prioridade sobre o filtro regional.
      if (!is.null(input$filtro_muni) && length(input$filtro_muni) > 0) {

        chaves_muni <- pt_chave_localidade(
          pt_normalizar_localidade(input$filtro_muni)
        )

        previsoes <- previsoes |>
          dplyr::filter(Chave_Localidade %in% chaves_muni)

      } else if (input$filtro_ri != "Todas as Regiões") {

        chaves_ri <- mapa_regioes() |>
          dplyr::filter(ri == input$filtro_ri) |>
          dplyr::pull(Chave_Localidade)

        previsoes <- previsoes |>
          dplyr::filter(Chave_Localidade %in% chaves_ri)
      }

      validate(need(
        nrow(previsoes) > 0,
        paste(
          "Não há previsões de",
          input$filtro_modelo,
          "para os filtros selecionados."
        )
      ))

      # Somar os meses de cada ano e acumular os anos em ordem.
      # Anos ausentes permanecem NA, sem serem convertidos em crescimento zero.
      df_resultado <- previsoes |>
        dplyr::mutate(Ano = format(Data, "%Y")) |>
        dplyr::group_by(Chave_Localidade, Ano) |>
        dplyr::summarise(
          crescimento_ano = sum(Previsao),
          .groups = "drop"
        ) |>
        tidyr::complete(
          Chave_Localidade,
          Ano = anos_prev
        ) |>
        dplyr::arrange(Chave_Localidade, Ano) |>
        dplyr::group_by(Chave_Localidade) |>
        dplyr::mutate(
          crescimento_acumulado = cumsum(crescimento_ano)
        ) |>
        dplyr::ungroup() |>
        dplyr::left_join(
          base_2025(),
          by = "Chave_Localidade"
        )

      validate(need(
        all(!is.na(df_resultado$frota_inicial_2025)),
        "Há localidades sem correspondência na base real de 2025."
      ))

      # Frota projetada = frota real de 2025 + previsões acumuladas.
      df_resultado |>
        dplyr::mutate(
          frota_final_estimada =
            frota_inicial_2025 + crescimento_acumulado
        ) |>
        dplyr::select(
          Localidade,
          Ano,
          frota_final_estimada
        ) |>
        tidyr::pivot_wider(
          names_from = Ano,
          values_from = frota_final_estimada
        ) |>
        dplyr::arrange(Localidade) |>
        dplyr::rename(`Município / Região` = Localidade)
    })

    #------------------------------------------------------------------------------#
    # 6. Rótulo do escopo selecionado
    #------------------------------------------------------------------------------#
    rotulo_regiao <- reactive({
      if (!is.null(input$filtro_muni) && length(input$filtro_muni) > 0) {

        chaves_selecionadas <- pt_chave_localidade(
          pt_normalizar_localidade(input$filtro_muni)
        )

        if ("PARA" %in% chaves_selecionadas) {
          "Pará"
        } else {
          "Seleção"
        }

      } else if (input$filtro_ri != "Todas as Regiões") {
        input$filtro_ri

      } else {
        "Pará"
      }
    })

    #------------------------------------------------------------------------------#
    # 6.1 Tabela com total na primeira linha
    #------------------------------------------------------------------------------#
    tabela_previsoes <- reactive({
      df <- dados_anuais()
      req(nrow(df) > 0)

      anos <- anos_prev

      # A previsão estadual já representa o estado inteiro.
      # Não somar a linha Pará novamente aos municípios.
      if ("Pará" %in% df[["Município / Região"]]) {

        total <- df[
          df[["Município / Região"]] == "Pará",
          ,
          drop = FALSE
        ]

        mun <- df[
          df[["Município / Região"]] != "Pará",
          ,
          drop = FALSE
        ]

      } else {

        # Total da região ou seleção: soma de todos os municípios exibidos.
        total <- as.data.frame(
          t(colSums(df[anos], na.rm = FALSE))
        )

        total[["Município / Região"]] <- rotulo_regiao()
        total <- total[, c("Município / Região", anos)]

        mun <- df
      }

      dplyr::bind_rows(total, mun)
    })

    #------------------------------------------------------------------------------#
    # 6.2 Indicadores
    #------------------------------------------------------------------------------#
    resumo_prev <- reactive({
      df <- dados_anuais()
      req(nrow(df) > 0)

      # O cartão usa o mesmo total apresentado na primeira linha da tabela.
      v_2031 <- as.numeric(
        tabela_previsoes()[["2031"]][1]
      )

      # Manter o cálculo de crescimento ponderado pela frota de 2026.
      municipios <- df[
        df[["Município / Região"]] != "Pará",
        ,
        drop = FALSE
      ]

      if (nrow(municipios) == 0) {
        municipios <- df
      }

      peso <- as.numeric(municipios[["2026"]])

      cresc_i <- (
        as.numeric(municipios[["2031"]]) / peso - 1
      ) * 100

      valido <- !is.na(peso) &
        peso > 0 &
        !is.na(cresc_i)

      cresc <- if (any(valido)) {
        sum(
          peso[valido] * cresc_i[valido],
          na.rm = TRUE
        ) / sum(
          peso[valido],
          na.rm = TRUE
        )
      } else {
        NA_real_
      }

      if (any(valido)) {
        melhor_idx <- which.max(cresc_i[valido])

        melhor_nome <- municipios[["Município / Região"]][valido][
          melhor_idx
        ]

        melhor_pct <- cresc_i[valido][melhor_idx]

      } else {
        melhor_nome <- NA_character_
        melhor_pct <- NA_real_
      }

      list(
        rotulo = rotulo_regiao(),
        v_2031 = v_2031,
        cresc = cresc,
        melhor_nome = melhor_nome,
        melhor_pct = melhor_pct
      )
    })

    output$vb_frota_2031 <- renderInfoBox({
      r <- resumo_prev()

      bs4InfoBox(
        title = tags$strong("FROTA PROJETADA 2031"),

        value = tags$h2(
          tags$strong(
            prettyNum(
              r$v_2031,
              big.mark = ".",
              decimal.mark = ","
            )
          )
        ),

        subtitle = paste0(
          "Projeção para ",
          r$rotulo,
          " — ",
          input$filtro_modelo
        ),

        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        icon = icon("flag"),
        color = "success"
      )
    })

    output$vb_crescimento <- renderInfoBox({
      r <- resumo_prev()

      cresc_fmt <- if (is.na(r$cresc)) {
        "N/A"
      } else {
        paste0(
          format(
            round(r$cresc, 1),
            decimal.mark = ","
          ),
          "%"
        )
      }

      bs4InfoBox(
        title = tags$strong("CRESCIMENTO 2026-2031"),
        value = tags$h2(tags$strong(cresc_fmt)),

        subtitle = paste0(
          "Média ponderada (frota 2026) - ",
          r$rotulo
        ),

        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        icon = icon("chart-line"),
        color = "info"
      )
    })

    output$vb_melhor <- renderInfoBox({
      r <- resumo_prev()

      melhor_fmt <- if (is.na(r$melhor_pct)) {
        "N/A"
      } else {
        paste0(
          format(
            round(r$melhor_pct, 1),
            decimal.mark = ","
          ),
          "%"
        )
      }

      bs4InfoBox(
        title = tags$strong("MAIOR CRESCIMENTO"),
        value = tags$h2(tags$strong(melhor_fmt)),

        subtitle = if (is.na(r$melhor_nome)) {
          "Sem dados"
        } else {
          r$melhor_nome
        },

        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        icon = icon("trophy"),
        color = "danger"
      )
    })

    #------------------------------------------------------------------------------#
    # 7. Renderização da tabela
    #------------------------------------------------------------------------------#
    output$tabela_geral_previsoes <- DT::renderDataTable({
      df_tabela <- tabela_previsoes()

      colunas_existentes <- intersect(
        anos_prev,
        colnames(df_tabela)
      )

      DT::datatable(
        df_tabela,
        rownames = FALSE,
        class = "cell-border stripe hover",

        options = list(
          pageLength = 15,
          lengthMenu = c(15, 30, 50, 100),
          scrollX = TRUE,

          language = list(
            sEmptyTable = "Nenhum registro encontrado",
            sInfo = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
            sInfoEmpty = "Mostrando 0 até 0 de 0 registros",
            sInfoFiltered = "(Filtrados de _MAX_ registros)",
            sInfoPostFix = "",
            sInfoThousands = ".",
            sLengthMenu = "_MENU_ resultados por página",
            sLoadingRecords = "Carregando...",
            sProcessing = "Processando...",
            sZeroRecords = "Nenhum registro encontrado",
            sSearch = "Pesquisar dentro da Tabela:",

            oPaginate = list(
              sNext = "Próximo",
              sPrevious = "Anterior",
              sFirst = "Primeiro",
              sLast = "Último"
            ),

            oAria = list(
              sSortAscending = ": Ordenar colunas de forma ascendente",
              sSortDescending = ": Ordenar colunas de forma descendente"
            )
          )
        )
      ) |>
        DT::formatRound(
          columns = colunas_existentes,
          digits = 0,
          mark = ".",
          dec.mark = ","
        ) |>
        DT::formatStyle(
          "Município / Região",
          target = "row",

          backgroundColor = DT::styleEqual(
            rotulo_regiao(),
            "#f0f0f0"
          ),

          fontWeight = DT::styleEqual(
            rotulo_regiao(),
            "bold"
          )
        ) |>
        DT::formatStyle(
          columns = colunas_existentes,
          textAlign = "right"
        )
    })

    #------------------------------------------------------------------------------#
    # 8. Download conforme o modelo selecionado
    #------------------------------------------------------------------------------#
    nome_prev <- reactive({
      req(input$filtro_modelo)

      paste0(
        "Projecao_Frota_",
        input$filtro_modelo,
        "_2026_2031"
      )
    })

    downset_Server(
      "prevdown",
      tabela_previsoes,
      nome_prev
    )
  })
}