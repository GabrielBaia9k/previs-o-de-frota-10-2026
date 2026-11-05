# Regressao: o OutDec do painel nao deve impedir o treinamento do XGBoost.
# Executar na raiz: Rscript --vanilla tests/test_xgboost_outdec.R
testar_xgboost_outdec <- function() {
  if (!file.exists("pre_compilar_xgboost.r")) {
    stop("Execute este teste na raiz do projeto.")
  }
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Este teste requer o pacote xgboost instalado no R local.")
  }
  opcoes <- options(frota.xgboost.apenas_funcoes = TRUE, OutDec = ".")
  on.exit(options(opcoes), add = TRUE)
  source("pre_compilar_xgboost.r", local = TRUE, encoding = "UTF-8")

  historico <- as.numeric(seq_len(36) * 3)
  datas <- seq(as.Date("2022-01-01"), by = "month", length.out = 36L)
  x <- variaveis_xgboost(historico, datas, config_xgboost_local$lags)
  config <- config_xgboost_local
  config$nrounds <- 5L
  futuras <- seq(as.Date("2025-01-01"), by = "month", length.out = 84L)

  referencia <- ajustar_xgboost(x, historico, config)
  previsao_referencia <- prever_recursivo_xgboost(
    referencia, historico, futuras, config$lags
  )$previsao
  stopifnot(identical(getOption("OutDec"), "."))

  # Reproduz a opcao definida por global.R sem carregar o aplicativo.
  options(OutDec = ",")
  modelo <- ajustar_xgboost(x, historico, config)
  stopifnot(identical(getOption("OutDec"), ","))
  previsao <- prever_recursivo_xgboost(
    modelo, historico, futuras, config$lags
  )$previsao
  stopifnot(length(previsao) == 84L, all(is.finite(previsao)),
            isTRUE(all.equal(previsao, previsao_referencia, tolerance = 1e-8)),
            identical(getOption("OutDec"), ","))

  # Um erro na montagem do treino tambem precisa restaurar a opcao da sessao.
  erro <- tryCatch(
    ajustar_xgboost(x, historico[-1L], config), error = function(e) e
  )
  stopifnot(inherits(erro, "error"), identical(getOption("OutDec"), ","))
  cat("TESTES_XGBOOST_OUTDEC_CONCLUIDOS\n")
}

testar_xgboost_outdec()
