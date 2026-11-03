# Execute na raiz do projeto: Rscript --vanilla tests/test_frontend.R
# Valida as tres abas e os arquivos pre-compilados; nao treina modelos.
if (!all(file.exists(c("app.R", "global.R")))) {
  stop("Execute este teste a partir da raiz do projeto.", call. = FALSE)
}
arquivos_r <- c(
  list.files(pattern = "\\.[Rr]$", full.names = TRUE),
  list.files("modulos", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
  list.files("tests", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
)
invisible(lapply(arquivos_r, function(arquivo) parse(arquivo, keep.source = FALSE)))
cat("SINTAXE_R_OK:", length(arquivos_r), "arquivos\n")

source("global.R", encoding = "UTF-8")
stopifnot(identical(normalizePath(here::here()), normalizePath(getwd())))
stopifnot(basename(data_dir) == "data", basename(previsoes_dir) == "Previsoes_output")
stopifnot(length(previsoes_compartilhadas$avisos) == 0L)
stopifnot(setequal(unique(previsoes_compartilhadas$dados$Modelo),
                  c("ARIMA", "ETS", "SARIMA", "SARIMAX", "LSTM", "GRU")))
dados_reais <- previsoes_compartilhadas$dados
chave <- function(x) pt_chave_localidade(pt_normalizar_localidade(x))
base_real <- db01_acumulado |>
  dplyr::filter(ano == "2025") |>
  dplyr::select(local, valor) |>
  dplyr::collect()
cat("LEITOR_SEIS_CSVS_OK\n")
print(as.data.frame(table(dados_reais$Modelo)))

shiny::testServer(previsoes_server, {
  for (modelo in c("SARIMAX", "SARIMA", "ARIMA", "ETS", "LSTM", "GRU")) {
    session$setInputs(filtro_modelo = modelo, filtro_ri = "Todas as Regiões",
                      filtro_muni = character())
    for (ano in c("2026", "2031")) {
      idx <- dados_reais$Modelo == modelo & chave(dados_reais$Localidade) == "PARA" &
        dados_reais$Data >= as.Date("2026-01-01") &
        dados_reais$Data <= as.Date(paste0(ano, "-12-31"))
      esperado <- sum(base_real$valor[chave(base_real$local) == "PARA"]) +
        sum(dados_reais$Previsao[idx])
      estado <- tabela_previsoes()[1, ]
      stopifnot(estado[["Município / Região"]] == "Pará")
      stopifnot(isTRUE(all.equal(as.numeric(estado[[ano]]), esperado)))
    }
    stopifnot(grepl(modelo, nome_prev(), fixed = TRUE))
    stopifnot(grepl(modelo, output$txtprev$html, fixed = TRUE))
    invisible(output$tabela_geral_previsoes)
  }
  session$setInputs(filtro_modelo = "GRU", filtro_ri = "GUAJARÁ", filtro_muni = character())
  stopifnot(nrow(dados_anuais()) == 5L)
  stopifnot(isTRUE(all.equal(resumo_prev()$v_2031, sum(dados_anuais()[["2031"]]))))
  session$setInputs(filtro_ri = "Todas as Regiões", filtro_muni = c("Belém", "Ananindeua"))
  stopifnot(nrow(dados_anuais()) == 2L)
  stopifnot(isTRUE(all.equal(resumo_prev()$v_2031, sum(dados_anuais()[["2031"]]))))
  session$setInputs(filtro_muni = c("Pará", "Belém"))
  estado <- dados_anuais()[dados_anuais()[["Município / Região"]] == "Pará", "2031"]
  stopifnot(isTRUE(all.equal(resumo_prev()$v_2031, as.numeric(estado[[1]]))))

  mensagens <- new.env(parent = emptyenv())
  raiz <- session$rootScope()
  raiz$sendInputMessage <- function(inputId, message) {
    assign(sub("^.*-", "", inputId), message, envir = mensagens)
  }
  session$setInputs(reset_prev = 1)
  stopifnot(identical(mensagens$filtro_modelo$value, "SARIMAX"))
  stopifnot(identical(mensagens$filtro_ri$value, "Todas as Regiões"))
  stopifnot(length(mensagens$filtro_muni$value) == 0L)
  cat("PREVISAO_FROTA_SEIS_MODELOS_ACUMULADO_TOTAIS_FILTROS_OK\n")
})

shiny::testServer(modelagem_server, {
  for (modelo in c("SARIMAX", "SARIMA", "ARIMA", "ETS", "LSTM", "GRU")) {
    for (local in c("Pará", "Belém")) {
      session$setInputs(filtro_modelo = modelo, select_local = local)
      idx <- dados_reais$Modelo == modelo &
        chave(dados_reais$Localidade) == chave(local) &
        is.finite(dados_reais$Real) & is.finite(dados_reais$Previsao)
      referencia <- dados_reais[idx, ]
      erro <- referencia$Previsao - referencia$Real
      r <- resumo_modelo()
      stopifnot(r$erros$n == nrow(referencia), nrow(tabela_previsto()) == nrow(referencia))
      stopifnot(isTRUE(all.equal(r$erros$mae, mean(abs(erro)))))
      stopifnot(isTRUE(all.equal(r$rmse, sqrt(mean(erro^2)))))
      nonzero <- referencia$Real != 0
      stopifnot(isTRUE(all.equal(r$erros$mape,
        mean(abs(erro[nonzero] / referencia$Real[nonzero])) * 100)))
      grafico <- jsonlite::fromJSON(output$grafico_modelo, simplifyVector = FALSE)
      nomes <- vapply(grafico$x$opts$series, function(x) x$name, character(1))
      stopifnot(identical(nomes, c("Valores Reais", paste("Previsões", modelo))))
      stopifnot(grepl(modelo, nome_modelagem(), fixed = TRUE))
      if (modelo %in% c("ETS", "LSTM", "GRU")) {
        stopifnot(grepl("Indisponível", output$metricas_modelo$html, fixed = TRUE))
      }
      invisible(output$tabela_real_previsto)
    }
  }
  erros <- calcular_erros_nativos(c(0, 10, 30, NA, Inf), c(5, 12, 27, 5, 2))
  stopifnot(erros$n == 3L, erros$n_mape == 2L, isTRUE(all.equal(erros$mape, 15)))
  stopifnot(isTRUE(all.equal(erros$mae, 10 / 3)))
  zeros <- calcular_erros_nativos(c(0, 0), c(1, 2))
  stopifnot(is.na(zeros$mape), is.na(zeros$r2), zeros$mae == 1.5)

  tabela <- as.data.frame(tabela_previsto())
  csv <- suppressWarnings(output[["modeldown-csv"]])
  xlsx <- output[["modeldown-xlsx"]]
  rds <- output[["modeldown-rds"]]
  stopifnot(isTRUE(all.equal(tabela,
    read.csv(csv, fileEncoding = "UTF-8-BOM", check.names = FALSE), check.attributes = FALSE)))
  stopifnot(isTRUE(all.equal(tabela,
    openxlsx::read.xlsx(xlsx, check.names = FALSE), check.attributes = FALSE)))
  stopifnot(isTRUE(all.equal(tabela, as.data.frame(readRDS(rds)), check.attributes = FALSE)))
  cat("MODELAGEM_METRICAS_ZEROS_GRAFICOS_EXPORTACOES_OK\n")
})

projecoes <- pt_dados_projecoes_comparacao(dados_reais, "Pará")
metricas <- pt_tabela_erros_projecoes(projecoes, dados_reais, "Pará")
stopifnot(setequal(metricas$Modelo, pt_modelos), all(metricas$Periodos_validos > 0L))
stopifnot(all(!is.na(metricas$Inicio)), all(!is.na(metricas$Fim)))
stopifnot(length(unique(metricas$Inicio)) == 1L, length(unique(metricas$Fim)) == 1L)
criar <- function(modelo, datas, real, pred) data.frame(
  Modelo = modelo, Localidade = "Pará", Data = as.Date(datas), Real = real, Previsao = pred
)
fixture <- rbind(
  criar("ARIMA", c("2025-01-01", "2025-02-01", "2025-03-01"), c(0, 10, 20), c(5, 12, 24)),
  criar("ETS", c("2025-02-01", "2025-03-01", "2025-04-01"), c(20, 40, 0), c(22, 44, 2)),
  criar("GRU", "2026-01-01", NA, 100)
)
p <- pt_dados_projecoes_comparacao(fixture, "Pará")
comum <- pt_tabela_erros_projecoes(p, fixture, "Pará", "comum")
individual <- pt_tabela_erros_projecoes(p, fixture, "Pará", "individual")
stopifnot(identical(comum$Periodos_validos, c(2L, 2L, 0L)))
stopifnot(identical(individual$Periodos_validos, c(3L, 3L, 0L)))
stopifnot(isTRUE(all.equal(comum$MAPE[1:2], c(20, 10))))

shiny::testServer(previsoes_todos_server, {
  session$setInputs(select_local = "Pará", recarregar = 0, periodo_metricas = "comum")
  stopifnot(setequal(unique(projecoes_comparacao()$projecoes$Modelo), pt_modelos))
  stopifnot(nrow(perdas_comparacao()) > 0L, nrow(gradientes_selecionados()) > 0L)
  stopifnot(setequal(unique(perdas_comparacao()$Serie),
    c("LSTM_Treino", "LSTM_Validacao", "GRU_Treino", "GRU_Validacao")))
  invisible(output$grafico_projecoes)
  invisible(output$tabela_erros_projecoes)
  invisible(output$tabela_resultados)
  invisible(output$grafico_perdas)
  invisible(output$grafico_gradientes)
  png <- output$baixar_perdas_png
  pdf <- output$baixar_perdas_pdf
  stopifnot(file.info(png)$size > 1000, file.info(pdf)$size > 1000)
  ausente <- setdiff(unique(dados_reais$Localidade[dados_reais$Modelo == "SARIMAX"]),
                     unique(dados_reais$Localidade[dados_reais$Modelo == "ARIMA"]))[1]
  # A cobertura real pode mudar apos gerar novas previsoes.
  # O caso de modelo ausente tambem e coberto pela fixture ARIMA/ETS/GRU acima.
  if (length(ausente) == 1L && !is.na(ausente)) {
    session$setInputs(select_local = ausente, periodo_metricas = "individual")
    stopifnot(!"ARIMA" %in% unique(projecoes_comparacao()$projecoes$Modelo))
    stopifnot(length(unique(projecoes_comparacao()$projecoes$Modelo)) > 0L)
    stopifnot(grepl("Sem previsão", output$status_projecoes$html, fixed = TRUE))
    invisible(output$grafico_projecoes)
    invisible(output$tabela_erros_projecoes)
  }
  cat("COMPARACAO_SEIS_MODELOS_PERIODOS_AUSENCIAS_PERDAS_GRADIENTES_OK\n")
})

aplicativo <- source("app.R", encoding = "UTF-8")$value
stopifnot(inherits(aplicativo, "shiny.appobj"))
html <- htmltools::renderTags(ui)$html
stopifnot(grepl("previsoes-filtro_modelo", html, fixed = TRUE))
stopifnot(grepl("modelagem-filtro_modelo", html, fixed = TRUE))
stopifnot(grepl("previsoes_todos-periodo_metricas", html, fixed = TRUE))
cat("APP_R_INTEGRACAO_UI_OK\n")

print(dados_reais |>
  dplyr::filter(chave(Localidade) == "PARA", Data >= as.Date("2025-01-01"),
                Data <= as.Date("2025-12-31")) |>
  dplyr::group_by(Modelo) |>
  dplyr::summarise(Soma_Real_2025 = sum(Real), .groups = "drop"))
cat("TESTES_FRONTEND_CONCLUIDOS\n")
