# ==============================================================================
# previsoes.R - Tabela de Visão Geral (Projeções SARIMAX ACUMULADAS)
# Padronizado segundo as convenções visuais dos demais módulos do dashboard
# ==============================================================================

previsoes_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    #------------------------------------------------------------------------------#
    ## 1. VALUE BOXES (Padrão do projeto: bs4InfoBox) ----
    fluidRow(
      bs4InfoBoxOutput(ns("vb_frota_2031"), width = 4),
      bs4InfoBoxOutput(ns("vb_crescimento"), width = 4),
      bs4InfoBoxOutput(ns("vb_melhor"), width = 4)
    ),
    
    tags$br(),
    
    #------------------------------------------------------------------------------#
    ## 2. Painel de Filtros (Padrão do projeto) ----
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
              choices = NULL, # Carregado no Server
              selected = "Todas as Regiões",
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              inputId = ns("filtro_muni"),
              label = "MUNICIPIOS",
              choices = NULL, # Carregado no Server baseado na RI
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
              choices = c("SARIMAX", "SARIMA", "ARIMA", "LSTM", "GRU"),
              selected = "SARIMAX",
              width = "100%"
            )
          ),
          column(
            2,
            tags$label(HTML("&nbsp;")), # Alinhamento perfeito do botão
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
        
        tags$p(
          style = "text-align: justify; font-size: 11pt;",
          "A tabela abaixo detalha a ", tags$strong("estimação total acumulada"), " da frota veicular até 2031. ",
          "Os valores apresentados consideram a base real consolidada de dezembro de 2025, acrescida progressivamente da projeção de crescimento calculada pelo modelo SARIMAX de acordo com sua respectiva localidade."
        ),
        
        tags$hr(),
        
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tabela_geral_previsoes")),
          type = 8, color = "#1D44B8", size = 0.8
        ),
        
        footer = list(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "DTI/DETRAN-PA", style = "margin-bottom: 0;"),
              tags$h6(tags$b("Elaboração:"), "CNP/DETRAN-PA", style = "margin-bottom: 0;")
            ),
            div(style = "text-align: center;", downset_ui(ns("prevdown")))
          )
        )
      )
    )
  )
}

previsoes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Período projetado (colunas da tabela)
    anos_prev <- as.character(2026:2031)
    
    # 1. Obter o mapeador Região -> Municípios a partir da base principal arrow
    mapa_regioes <- reactive({
      frota_acumulado |> 
        dplyr::select(ri, local) |> 
        dplyr::distinct() |> 
        dplyr::collect() |>
        dplyr::mutate(ri = toupper(ri))
    })
    
    # 2. Inicializar o dropdown de Regiões de Integração
    observe({
      df_mapa <- mapa_regioes()
      vetor_ri <- sort(unique(df_mapa$ri[df_mapa$ri != "" & df_mapa$ri != "PARÁ" & !is.na(df_mapa$ri)]))
      
      updateSelectInput(
        session,
        "filtro_ri",
        choices = c("Todas as Regiões", vetor_ri),
        selected = "Todas as Regiões"
      )
    })
    
    # 3. Lógica Encadeada: O filtro de R.I alimenta o dropdown de Município
    observe({
      req(input$filtro_ri)
      df_mapa <- mapa_regioes()
      
      if (input$filtro_ri == "Todas as Regiões") {
        munis_diponiveis <- sort(unique(df_mapa$local[df_mapa$local != "Pará"]))
        munis_diponiveis <- c("Pará", munis_diponiveis)
      } else {
        munis_diponiveis <- df_mapa |> 
          dplyr::filter(ri == input$filtro_ri) |> 
          dplyr::pull(local) |> 
          sort()
      }
      
      updateSelectizeInput(
        session,
        "filtro_muni",
        choices = munis_diponiveis,
        server = TRUE
      )
    })
    
    # 3.1 Botão LIMPAR (Padrão do projeto)
    observeEvent(input$reset_prev, {
      freezeReactiveValue(input, "filtro_ri")
      freezeReactiveValue(input, "filtro_muni")
      
      updateSelectInput(session, "filtro_ri", selected = "Todas as Regiões")
      updateSelectizeInput(session, "filtro_muni", selected = character(0))
    })
    
    # 4. Título da caixa principal (Padrão do projeto: h4 centralizado)
    output$txtprev <- renderText({
      HTML(paste0(
        "<h4 style='text-align:center; font-weight:bold; margin: 0;'>",
        "Projeção da Frota por Localidade (2026-2031)",
        "</h4>"
      ))
    })
    
    # 5. Cálculo Preditivo e Filtros
    dados_anuais <- reactive({
      # 5.1 Carregar o tamanho inicial da Frota consolidado de 2025 para cada localização.
      base_2025 <- db01_acumulado |>
        dplyr::filter(ano == "2025") |>
        dplyr::select(local, valor) |>
        dplyr::collect() |>
        dplyr::group_by(local) |>
        dplyr::summarise(frota_inicial_2025 = sum(valor, na.rm = TRUE), .groups = "drop")
      
      # 5.2 Carregar a taxa de crescimento projetada pela modelagem (Parquet lazy)
      df_sarimax_lazy <- arrow::open_dataset(here::here("data", "previsoes_sarimax_pre_comp.parquet"))
      
      # Filtragem do Dashboard (Regiões/Municípios) - pushdown antes do collect
      if (!is.null(input$filtro_muni) && length(input$filtro_muni) > 0) {
        df_sarimax <- df_sarimax_lazy |> dplyr::filter(Localidade_Nome %in% input$filtro_muni)
      } else {
        req(input$filtro_ri)
        if (input$filtro_ri != "Todas as Regiões") {
          df_mapa <- mapa_regioes()
          munis_da_ri <- df_mapa |> dplyr::filter(ri == input$filtro_ri) |> dplyr::pull(local)
          df_sarimax <- df_sarimax_lazy |> dplyr::filter(Localidade_Nome %in% munis_da_ri)
        } else {
          df_sarimax <- df_sarimax_lazy
        }
      }
      
      # Intervalo preditivo (2026-2031) aplicado via pushdown, depois materializa o filtrado
      df_sarimax <- df_sarimax |>
        dplyr::filter(Data >= as.Date("2026-01-01"), Data <= as.Date("2031-12-31")) |>
        dplyr::collect()
      
      # 5.3 Calcular o valor ACUMULADO (junção direta pelo nome, alinhado ao db01)
      df_resultado <- df_sarimax |>
        dplyr::mutate(
          Ano = format(Data, "%Y")
        ) |>
        # Agrupamos por Ano para somar TODOS OS MESES e achar o crescimento total daquele respectivo ano
        dplyr::group_by(Localidade_Nome, Ano) |>
        dplyr::summarise(crescimento_ano = sum(Previsao_SARIMAX, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(Localidade_Nome, Ano) |>
        # Agrupamos de novo apenas por cidade, criando agora uma soma degrau sobre degrau de cada ano cumsum()
        dplyr::group_by(Localidade_Nome) |>
        dplyr::mutate(crescimento_acumulado = cumsum(crescimento_ano)) |>
        dplyr::ungroup() |>
        # Fazemos a junção do valor projetado com a Base Verídica do db01 (pelo nome padronizado)
        dplyr::left_join(base_2025, by = c("Localidade_Nome" = "local")) |>
        # Para evitar NAs da base, substituímos por 0 se esvaziar
        dplyr::mutate(frota_inicial_2025 = ifelse(is.na(frota_inicial_2025), 0, frota_inicial_2025)) |>
        # Cálculo da estimativa final: soma do valor acumulado projetado à base real de 2025
        dplyr::mutate(frota_final_estimada = frota_inicial_2025 + crescimento_acumulado) |>
        # Mantém colunas chaves
        dplyr::select(Localidade = Localidade_Nome, Ano, frota_final_estimada) |>
        tidyr::pivot_wider(
          names_from = Ano, 
          values_from = frota_final_estimada
        ) |>
        dplyr::arrange(Localidade) |> 
        dplyr::rename(`Município / Região` = Localidade)
      
      return(df_resultado)
    })
    
    # 6. VALUE BOXES (Padrão do projeto)
    
    # Rótulo da região selecionada (para títulos dos KPIs)
    rotulo_regiao <- reactive({
      if (!is.null(input$filtro_muni) && length(input$filtro_muni) > 0) {
        "Seleção"
      } else if (input$filtro_ri != "Todas as Regiões") {
        input$filtro_ri
      } else {
        "Pará"
      }
    })
    
    # 6.5 Tabela com a linha de Total (RI) no TOPO
    tabela_previsoes <- reactive({
      df <- dados_anuais()
      req(nrow(df) > 0)
      anos <- anos_prev
      
      # Agregado já existe no parquet (visão geral) -> usa sem recalcular
      if ("Pará" %in% df[["Município / Região"]]) {
        total <- df[df[["Município / Região"]] == "Pará", , drop = FALSE]
        mun   <- df[df[["Município / Região"]] != "Pará", , drop = FALSE]
      } else {
        # Soma dos municípios do escopo filtrado (RI/seleção)
        total <- as.data.frame(t(colSums(df[anos], na.rm = TRUE)))
        total[["Município / Região"]] <- rotulo_regiao()
        total <- total[, c("Município / Região", anos)]
        mun <- df
      }
      
      dplyr::bind_rows(total, mun)   # RI no topo + municípios abaixo
    })
    
    resumo_prev <- reactive({
      df <- dados_anuais()
      req(nrow(df) > 0)
      
      # Linha do total (estado) para o KPI de frota; senão usa a primeira localidade do filtro
      if ("Pará" %in% df[["Município / Região"]]) {
        linha <- df[df[["Município / Região"]] == "Pará", , drop = FALSE]
      } else {
        linha <- df[1, , drop = FALSE]
      }
      
      v_2031 <- as.numeric(linha[["2031"]])
      
      # Crescimento = média PONDERADA dos crescimentos dos municípios da região,
      # com peso = quantidade de veículos em 2026 (exclui a linha de total "Pará")
      municipios <- df[df[["Município / Região"]] != "Pará", , drop = FALSE]
      if (nrow(municipios) == 0) municipios <- df   # segurança (ex.: só "Pará" selecionado)
      
      peso    <- as.numeric(municipios[["2026"]])
      cresc_i <- (as.numeric(municipios[["2031"]]) / peso - 1) * 100
      valido  <- !is.na(peso) & peso > 0 & !is.na(cresc_i)
      
      cresc <- if (any(valido)) {
        sum(peso[valido] * cresc_i[valido], na.rm = TRUE) / sum(peso[valido], na.rm = TRUE)
      } else {
        NA_real_
      }
      
      # Município com maior crescimento na região
      if (any(valido)) {
        melhor_idx <- which.max(cresc_i[valido])
        melhor_nome <- municipios[["Município / Região"]][valido][melhor_idx]
        melhor_pct  <- cresc_i[valido][melhor_idx]
      } else {
        melhor_nome <- NA_character_
        melhor_pct  <- NA_real_
      }
      
      list(
        rotulo     = rotulo_regiao(),
        v_2031     = v_2031,
        cresc      = cresc,
        melhor_nome = melhor_nome,
        melhor_pct  = melhor_pct
      )
    })
    
    output$vb_frota_2031 <- renderInfoBox({
      r <- resumo_prev()
      bs4InfoBox(
        title = tags$strong("FROTA PROJETADA 2031"),
        value = tags$h2(tags$strong(prettyNum(r$v_2031, big.mark = ".", decimal.mark = ","))),
        subtitle = paste0("Projeção para ", r$rotulo),
        fill = TRUE, gradient = TRUE, iconElevation = 2, icon = icon("flag"), color = "success"
      )
    })
    
    output$vb_crescimento <- renderInfoBox({
      r <- resumo_prev()
      cresc_fmt <- if (is.na(r$cresc)) "N/A" else paste0(format(round(r$cresc, 1), decimal.mark = ","), "%")
      bs4InfoBox(
        title = tags$strong("CRESCIMENTO 2026-2031"),
        value = tags$h2(tags$strong(cresc_fmt)),
        subtitle = paste0("Média ponderada (frota 2026) - ", r$rotulo),
        fill = TRUE, gradient = TRUE, iconElevation = 2, icon = icon("chart-line"), color = "info"
      )
    })
    
    output$vb_melhor <- renderInfoBox({
      r <- resumo_prev()
      melhor_fmt <- if (is.na(r$melhor_pct)) "N/A" else paste0(format(round(r$melhor_pct, 1), decimal.mark = ","), "%")
      bs4InfoBox(
        title = tags$strong("MAIOR CRESCIMENTO"),
        value = tags$h2(tags$strong(melhor_fmt)),
        subtitle = if (is.na(r$melhor_nome)) "Sem dados" else r$melhor_nome,
        fill = TRUE, gradient = TRUE, iconElevation = 2, icon = icon("trophy"), color = "danger"
      )
    })
    
    # 7. Renderização configurada puramente para o Português (Offline Safe)
    output$tabela_geral_previsoes <- DT::renderDataTable({
      df_tabela <- tabela_previsoes()
      
      colunas_anos <- anos_prev
      colunas_existentes <- intersect(colunas_anos, colnames(df_tabela))
      
      DT::datatable(
        df_tabela, 
        rownames = FALSE, 
        class = 'cell-border stripe hover',
        options = list(
          pageLength = 15,
          lengthMenu = c(15, 30, 50, 100), 
          scrollX = TRUE,
          # Tradução Local Explicita (Garante português sem depender de CDN/Internet)
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
        # Destaque da linha de Total (RI) - rótulo dinâmico (Padrão do projeto: styleEqual)
        DT::formatStyle(
          "Município / Região",
          target = "row",
          backgroundColor = DT::styleEqual(rotulo_regiao(), "#f0f0f0"),
          fontWeight = DT::styleEqual(rotulo_regiao(), "bold")
        ) |>
        # Alinhamento numérico à direita
        DT::formatStyle(
          columns = colunas_existentes,
          textAlign = "right"
        )
    })
    
    # 8. Download (Padrão do projeto: downset)
    nome_prev <- reactive({"Projecao_Frota_SARIMAX_2026_2031"})
    downset_Server("prevdown", tabela_previsoes, nome_prev)
    
  })
}