# ==============================================================================
# modelagem.R - Resultados pré-compilados por modelo e localidade
# Usa as previsões padronizadas disponibilizadas pelo global.R.
# ==============================================================================

modelagem_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    # 1. Indicadores
    fluidRow(
      bs4InfoBoxOutput(ns("vb_anos"), width = 4),
      bs4InfoBoxOutput(ns("vb_rmse"), width = 4),
      bs4InfoBoxOutput(ns("vb_cresc"), width = 4)
    ),

    tags$br(),

    # 2. Filtros
    fluidRow(
      bs4Dash::box(
        title = tagList(icon("filter"), "Filtros de Modelagem"),
        status = "primary",
        solidHeader = FALSE,
        elevation = 2,
        collapsible = TRUE,
        width = 12,
        fluidRow(
          column(
            6,
            selectInput(
              inputId = ns("select_local"),
              label = "MUNICIPIO OU ESTADO",
              choices = NULL,
              selected = NULL,
              width = "100%"
            )
          ),
          column(
            4,
            selectInput(
              inputId = ns("filtro_modelo"),
              label = "MODELO DE PREVISÃO",
              choices = c("SARIMAX", "SARIMA", "ARIMA", "ETS", "LSTM", "GRU"),
              selected = "SARIMAX",
              width = "100%"
            )
          ),
          column(
            2,
            tags$label(HTML("&nbsp;")),
            actionBttn(
              inputId = ns("reset_modelagem"),
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

    # 3. Métricas, tabela e gráfico
    fluidRow(
      bs4Dash::box(
        title = htmlOutput(ns("txtmodelagem")),
        status = "primary",
        solidHeader = FALSE,
        elevation = 2,
        maximizable = TRUE,
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 12,

        uiOutput(ns("descricao_modelagem")),
        tags$hr(),
        uiOutput(ns("metricas_modelo")),

        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tabela_real_previsto")),
          type = 8, color = "#1D44B8", size = 0.8
        ),

        tags$hr(),

        shinycssloaders::withSpinner(
          echarts4rOutput(ns("grafico_modelo"), width = "100%", height = "60vh"),
          type = 8, color = "#1D44B8", size = 0.8
        ),

        footer = list(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              tags$h6(
                tags$b("Fonte:", style = "font-family: sans-serif;"),
                "DTI/DETRAN-PA", style = "margin-bottom: 0;"
              ),
              tags$h6(
                tags$b("Elaboração:"), "CNP/DETRAN-PA",
                style = "margin-bottom: 0;"
              )
            ),
            div(style = "text-align: center;", downset_ui(ns("modeldown")))
          )
        )
      )
    )
  )
}

modelagem_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # 1. Previsões já lidas dos CSVs pelo leitor compartilhado no global.R.
    dados_modelo <- reactive({
      req(input$filtro_modelo)

      dados <- previsoes_compartilhadas$dados |>
        dplyr::filter(Modelo == input$filtro_modelo, !is.na(Data)) |>
        dplyr::mutate(
          Localidade = pt_normalizar_localidade(Localidade),
          Chave_Localidade = pt_chave_localidade(Localidade)
        ) |>
        dplyr::filter(!is.na(Localidade), nzchar(Localidade)) |>
        dplyr::arrange(Data)

      validate(need(
        nrow(dados) > 0,
        paste("Não há dados disponíveis para o modelo", input$filtro_modelo, ".")
      ))

      dados
    })

    # 2. Localidades disponíveis no modelo selecionado.
    observe({
      locais <- sort(unique(dados_modelo()$Localidade))
      locais <- c(intersect("Pará", locais), setdiff(locais, "Pará"))

      atual <- pt_chave_localidade(
        pt_normalizar_localidade(isolate(input$select_local))
      )
      indice <- match(atual, pt_chave_localidade(locais))

      selecionado <- if (length(indice) == 1L && !is.na(indice)) {
        locais[indice]
      } else {
        locais[1]
      }

      updateSelectInput(
        session, "select_local",
        choices = locais, selected = selecionado
      )
    })

    observeEvent(input$reset_modelagem, {
      freezeReactiveValue(input, "select_local")
      freezeReactiveValue(input, "filtro_modelo")

      updateSelectInput(session, "filtro_modelo", selected = "SARIMAX")
      updateSelectInput(session, "select_local", selected = "Pará")
    })

    # 3. Dados comuns a tabela, gráfico, métricas e download.
    dados_filtrados <- reactive({
      req(input$select_local)

      chave <- pt_chave_localidade(pt_normalizar_localidade(input$select_local))
      dados <- dados_modelo() |>
        dplyr::filter(Chave_Localidade %in% chave)

      validate(need(
        nrow(dados) > 0,
        "Não há dados deste modelo para a localidade selecionada."
      ))

      dados
    })

    # Avaliar somente pares de valores reais e previsões finitos.
    dados_avaliacao <- reactive({
      dados_filtrados() |>
        dplyr::filter(is.finite(Real), is.finite(Previsao))
    })

    # 4. Funções auxiliares de cálculo e apresentação.
    calcular_erros_nativos <- function(y_real, y_pred) {
      validos <- is.finite(y_real) & is.finite(y_pred)
      y_real <- y_real[validos]
      y_pred <- y_pred[validos]
      n <- length(y_real)

      if (n == 0L) {
        return(list(
          mae = NA_real_, rmse = NA_real_, mape = NA_real_,
          r2 = NA_real_, n = 0L, n_mape = 0L
        ))
      }

      erro <- y_pred - y_real
      sem_zero <- y_real != 0
      sst <- sum((y_real - mean(y_real))^2)

      list(
        mae = mean(abs(erro)),
        rmse = sqrt(mean(erro^2)),
        mape = if (any(sem_zero)) {
          mean(abs(erro[sem_zero] / y_real[sem_zero])) * 100
        } else {
          NA_real_
        },
        r2 = if (n >= 2L && sst > 0) {
          1 - sum(erro^2) / sst
        } else {
          NA_real_
        },
        n = n,
        n_mape = sum(sem_zero)
      )
    }

    primeiro_finito <- function(x) {
      valores <- x[is.finite(x)]
      if (length(valores)) valores[1] else NA_real_
    }

    formatar_numero <- function(x, casas = 0, indisponivel = "N/A") {
      if (length(x) != 1L || !is.finite(x)) return(indisponivel)
      format(round(x, casas), nsmall = casas, big.mark = ".", decimal.mark = ",")
    }

    # 5. Resumo único para os indicadores e a tabela de métricas.
    resumo_modelo <- reactive({
      df <- dados_filtrados()
      avaliacao <- dados_avaliacao()
      erros <- calcular_erros_nativos(avaliacao$Real, avaliacao$Previsao)

      # Horizonte futuro: previsões de 2026 a 2031 sem valor real observado.
      futuro <- df |>
        dplyr::filter(
          is.na(Real), is.finite(Previsao),
          Data >= as.Date("2026-01-01"),
          Data <= as.Date("2031-12-31")
        )

      anos <- sort(unique(format(futuro$Data, "%Y")))
      cresc_anual <- NA_real_

      if (length(anos) >= 2L) {
        soma_ano <- futuro |>
          dplyr::mutate(ano = as.integer(format(Data, "%Y"))) |>
          dplyr::group_by(ano) |>
          dplyr::summarise(total = sum(Previsao), .groups = "drop") |>
          dplyr::arrange(ano)

        anterior <- dplyr::lag(soma_ano$total)
        consecutivo <- soma_ano$ano - dplyr::lag(soma_ano$ano) == 1L
        validos <- !is.na(consecutivo) & consecutivo &
          is.finite(anterior) & anterior != 0

        if (any(validos)) {
          variacao <- (soma_ano$total[validos] / anterior[validos] - 1) * 100
          variacao <- variacao[is.finite(variacao)]
          if (length(variacao)) cresc_anual <- mean(variacao)
        }
      }

      list(
        modelo = input$filtro_modelo,
        local = df$Localidade[1],
        anos_min = if (length(anos)) anos[1] else NA_character_,
        anos_max = if (length(anos)) anos[length(anos)] else NA_character_,
        erros = erros,
        rmse = erros$rmse,
        periodo_avaliacao = if (nrow(avaliacao)) {
          paste(
            format(min(avaliacao$Data), "%m/%Y"),
            "a", format(max(avaliacao$Data), "%m/%Y")
          )
        } else {
          "Nenhum período observado com previsão disponível"
        },
        aic = primeiro_finito(df[["AIC"]]),
        bic = primeiro_finito(df[["BIC"]]),
        cresc_anual = cresc_anual
      )
    })

    # Tabela observada, também utilizada no download.
    tabela_previsto <- reactive({
      df <- dados_avaliacao() |>
        dplyr::mutate(
          `Diferenca Absoluta` = abs(Previsao - Real),
          `Erro (%)` = 100 * `Diferenca Absoluta` /
            ifelse(Real == 0, NA_real_, abs(Real)),
          Data = format(Data, "%b/%Y")
        ) |>
        dplyr::select(
          `Mês` = Data, `Frota Real` = Real, Previsto = Previsao,
          `Diferenca Absoluta`, `Erro (%)`
        )

      names(df)[names(df) == "Previsto"] <- paste("Previsto", input$filtro_modelo)
      df
    })

    # 6. Indicadores.
    output$vb_anos <- renderInfoBox({
      r <- resumo_modelo()
      mostra <- if (!is.na(r$anos_min)) {
        paste0(r$anos_min, " - ", r$anos_max)
      } else {
        "N/A"
      }

      bs4InfoBox(
        title = tags$strong("ANOS COBERTOS"),
        value = tags$h2(tags$strong(mostra)),
        subtitle = paste0("Horizonte projetado - ", r$local, " — ", r$modelo),
        fill = TRUE, gradient = TRUE, iconElevation = 2,
        icon = icon("calendar"), color = "primary"
      )
    })

    output$vb_rmse <- renderInfoBox({
      r <- resumo_modelo()
      bs4InfoBox(
        title = tags$strong("ERRO (RMSE)"),
        value = tags$h2(tags$strong(formatar_numero(r$rmse))),
        subtitle = paste0("Erro em ", r$erros$n, " meses observados — ", r$modelo),
        fill = TRUE, gradient = TRUE, iconElevation = 2,
        icon = icon("ruler-horizontal"), color = "success"
      )
    })

    output$vb_cresc <- renderInfoBox({
      r <- resumo_modelo()
      cresc_fmt <- if (is.finite(r$cresc_anual)) {
        paste0(formatar_numero(r$cresc_anual, 1), "%")
      } else {
        "N/A"
      }

      bs4InfoBox(
        title = tags$strong("CRESCIMENTO MÉDIO ANUAL"),
        value = tags$h2(tags$strong(cresc_fmt)),
        subtitle = paste0("Variação anual projetada - ", r$local, " — ", r$modelo),
        fill = TRUE, gradient = TRUE, iconElevation = 2,
        icon = icon("chart-line"), color = "info"
      )
    })

    # 7. Textos e métricas conforme o modelo e a localidade.
    output$txtmodelagem <- renderUI({
      r <- resumo_modelo()
      tags$h4(
        style = "text-align:center; font-weight:bold; margin: 0;",
        paste0("Projeção - Modelo ", r$modelo, " (", r$local, ")")
      )
    })

    output$descricao_modelagem <- renderUI({
      r <- resumo_modelo()
      tagList(
        tags$p(
          style = "text-align: justify; font-size: 11pt;",
          "Resultados pré-compilados do modelo ", tags$strong(r$modelo),
          " para ", r$local, ". A linha azul representa os valores reais ",
          "e a vermelha as previsões."
        ),
        tags$p(
          style = "text-align: justify; font-size: 11pt;",
          icon("table"), " A tabela compara valores reais e previsões de ",
          r$modelo, " nos meses em que ambos estão disponíveis."
        )
      )
    })

    output$metricas_modelo <- renderUI({
      r <- resumo_modelo()
      erros <- r$erros
      cor_mape <- if (!is.finite(erros$mape)) {
        "#666666"
      } else if (erros$mape <= 5) {
        "green"
      } else if (erros$mape <= 15) {
        "#d39e00"
      } else {
        "red"
      }
      mape_fmt <- if (is.finite(erros$mape)) {
        paste0(formatar_numero(erros$mape, 2), "%")
      } else {
        "N/A"
      }

      tags$div(
        style = "margin-bottom: 20px;",
        tags$b(icon("chart-bar"), " Erros no período observado e critérios do modelo:"),
        tags$p(paste0(
          "Período avaliado: ", r$periodo_avaliacao,
          " (", erros$n, " meses)."
        )),
        tags$table(
          class = "table table-bordered table-sm table-striped",
          style = "width: 100%; text-align: center; margin-top: 10px;",
          tags$thead(tags$tr(
            class = "bg-light",
            tags$th("MAPE (%)"), tags$th("MAE"), tags$th("RMSE"),
            tags$th("AIC"), tags$th("BIC"), tags$th("R²")
          )),
          tags$tbody(tags$tr(
            tags$td(
              style = paste0("color: ", cor_mape, "; font-weight: bold;"),
              mape_fmt
            ),
            tags$td(formatar_numero(erros$mae)),
            tags$td(formatar_numero(erros$rmse)),
            tags$td(formatar_numero(r$aic, 2, "Indisponível")),
            tags$td(formatar_numero(r$bic, 2, "Indisponível")),
            tags$td(formatar_numero(erros$r2, 4))
          ))
        ),
        tags$p(
          style = "font-size: 10pt;",
          paste0(
            "MAPE calculado em ", erros$n_mape,
            " meses com valor real diferente de zero. MAE, RMSE e R² mantêm os zeros. ",
            "AIC e BIC são critérios de ajuste e são exibidos quando disponíveis."
          )
        )
      )
    })

    output$tabela_real_previsto <- DT::renderDataTable({
      df <- tabela_previsto()
      coluna_previsao <- paste("Previsto", input$filtro_modelo)
      colunas_valores <- c("Frota Real", coluna_previsao, "Diferenca Absoluta")

      DT::datatable(
        df, rownames = FALSE, class = "cell-border stripe hover",
        options = list(
          dom = "t", paging = FALSE,
          columnDefs = list(list(targets = "_all", defaultContent = "N/A")),
          language = list(
            sEmptyTable = "Nenhum período observado com previsão disponível",
            sZeroRecords = "Nenhum registro encontrado"
          )
        )
      ) |>
        DT::formatRound(columns = colunas_valores, digits = 0) |>
        DT::formatRound(columns = "Erro (%)", digits = 2) |>
        DT::formatStyle(
          "Erro (%)",
          color = DT::styleInterval(c(5, 15), c("green", "orange", "red")),
          fontWeight = "bold"
        ) |>
        DT::formatStyle(
          columns = c(colunas_valores, "Erro (%)"), textAlign = "right"
        )
    })

    # 8. Histórico e previsões do modelo escolhido.
    output$grafico_modelo <- renderEcharts4r({
      old_outdec <- getOption("OutDec")
      options(OutDec = ".")
      on.exit(options(OutDec = old_outdec), add = TRUE)

      df <- dados_filtrados() |>
        dplyr::mutate(
          Real = ifelse(is.finite(Real), Real, NA_real_),
          Previsao = ifelse(is.finite(Previsao), Previsao, NA_real_)
        )

      df |>
        e_charts(x = Data) |>
        e_line(
          serie = Real, name = "Valores Reais", color = "#007bff",
          smooth = FALSE, connectNulls = FALSE, symbol = "roundRect",
          symbolSize = 3, legendHoverLink = TRUE, lineStyle = list(width = 2)
        ) |>
        e_line(
          serie = Previsao, name = paste("Previsões", input$filtro_modelo),
          color = "#dc3545", smooth = FALSE, connectNulls = FALSE,
          symbol = "roundRect", symbolSize = 3,
          legendHoverLink = TRUE, lineStyle = list(width = 2)
        ) |>
        e_tooltip(
          trigger = "axis", axisPointer = list(type = "shadow"),
          formatter = htmlwidgets::JS(
            "function(params) {
              if (!params || params.length === 0) return '';
              var result = '<b>' + (params[0].axisValueLabel || params[0].name) + '</b><br/>';
              params.forEach(function(item) {
                var val = Array.isArray(item.value) ? item.value[1] : item.value;
                if (val !== null && val !== undefined && val !== '' && isFinite(Number(val))) {
                  result += item.marker + item.seriesName + ': <b>' +
                    Math.round(Number(val)).toLocaleString('pt-BR') + ' veículos</b><br/>';
                }
              });
              return result;
            }"
          )
        ) |>
        e_x_axis(
          type = "time",
          axisLabel = list(show = TRUE, fontSize = 14, fontWeight = "bold"),
          name = "", splitLine = list(show = TRUE),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14, padding = c(0, 0, 0, 20))
        ) |>
        e_y_axis(
          name = "Veículos", scale = TRUE,
          nameTextStyle = list(fontWeight = "bold", fontSize = 16),
          axisLabel = list(formatter = htmlwidgets::JS(
            "function(value) { return value.toLocaleString('pt-BR'); }"
          ))
        ) |>
        e_locale("pt-Br") |>
        e_grid(show = TRUE, bottom = "20%") |>
        e_animation(duration = 600) |>
        e_legend(bottom = 0, textStyle = list(fontSize = 14, fontWeight = "bold")) |>
        e_toolbox(feature = list(saveAsImage = list(title = "Salvar Gráfico")))
    })

    # 9. Download da tabela observada para o modelo e a localidade.
    nome_modelagem <- reactive({
      r <- resumo_modelo()
      paste("Previsao_x_Real", r$modelo, r$local, sep = "_")
    })

    downset_Server("modeldown", tabela_previsto, nome_modelagem)
  })
}