# XGBoost local: um modelo independente por municipio, executado offline.
# Rscript --vanilla pre_compilar_xgboost.r
# No console: source("pre_compilar_xgboost.r", encoding = "UTF-8")
#            executar_xgboost_local()
# Dependencia de treinamento: xgboost. Nenhum pacote e instalado pelo script.
#
# PREPARACAO DOCUMENTADA
# 1. Ler somente o CSV geral; somar os tipos de veiculo por municipio/mes.
#    Se uma quantidade do grupo for desconhecida, seu total permanece NA.
# 2. Recortar 2016-2025 e completar o calendario mensal com NA, nunca com zero.
#    Zeros explicitos sao validos. Linhas estaduais de entrada sao excluidas.
# 3. Usar apenas mes, lag de 1 mes e lag de 12 meses. As defasagens seguem
#    o calendario; nao pulam lacunas. Nao ha normalizacao nem tendencia ajustada.
#    XGBoost recebe NA nos preditores; alvos NA nao entram no treinamento.
# 4. Treinar ate dezembro/2024; prever todo 2025 recursivamente, sem ler os
#    valores reais de 2025 nos lags. Hiperparametros fixos, sem busca no teste.
# 5. Reajustar ate dezembro/2025 e prever 72 meses (2026-2031), tambem de forma
#    recursiva. O CSV contem 2026, mas esses registros ficam fora deste recorte.
# 6. Manter previsoes decimais; limitar negativas a zero e contar correcoes.
#    Exportar NA na previsao do treino e no Real futuro. Nao exportar acumulados.
# 7. Exigir 24 alvos conhecidos apos os primeiros 12 meses para cada ajuste.
#    Municipios sem esse historico sao registrados no diagnostico, sem fallback.
#
# ESCOPO: apenas municipios. Nao se estima uma serie estadual nesta primeira
# versao. Os resultados nao foram ainda adicionados aos seletores do Shiny.
# A avaliacao compara XGBoost e sazonal ingenuo nos mesmos pares observados.
# Um teste de 12 meses nao valida, sozinho, a precisao dos seis anos futuros.

config_xgboost_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_xgboost_pre_comp.csv",
  avaliacao = "data/avaliacao_xgboost_local.csv",
  diagnostico = "data/diagnostico_xgboost_local.csv",
  execucao = "data/execucao_xgboost_local.txt",
  ano_inicio = 2016L, ano_teste = 2025L, ano_fim = 2025L,
  passos_futuro = 72L, lags = c(1,2,3,6,12,13), minimo_treino = 24L,
  nrounds = 100L,
  parametros = list(
    objective = "reg:squarederror", tree_method = "hist",
    max_depth = 3L, eta = 0.05, min_child_weight = 3,
    subsample = 1, colsample_bytree = 1, nthread = 1L, seed = 42L
  )
)

ler_mensal_xgboost <- function(config) {
  dados <- utils::read.csv(
    config$entrada, colClasses = "character", check.names = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", "NaN", "NULL")
  )
  obrigatorias <- c("LOCAL", "ANO", "MES", "TIPO_VEICULO", "QTD_VEICULO")
  faltantes <- setdiff(obrigatorias, names(dados))
  if (length(faltantes)) stop("Colunas ausentes: ", paste(faltantes, collapse = ", "))
  numero <- function(x) suppressWarnings(as.numeric(x))
  ano <- numero(dados$ANO); mes <- numero(dados$MES)
  qtd <- numero(dados$QTD_VEICULO)
  local <- trimws(dados$LOCAL)
  ausente <- is.na(dados$QTD_VEICULO)
  invalido <- is.na(local) | !nzchar(local) |
    !is.finite(ano) | ano != floor(ano) | ano < 1 | ano > 9999 |
    !is.finite(mes) | mes != floor(mes) | mes < 1 | mes > 12 |
    (!ausente & (!is.finite(qtd) | qtd < 0))
  if (any(invalido)) {
    stop("Dados invalidos nas linhas do CSV: ",
         paste(head(which(invalido) + 1L, 10L), collapse = ", "))
  }
  chave <- chartr("ÁÀÂÃÉÊÍÓÔÕÚÇ", "AAAAEEIOOOUC", toupper(local))
  manter <- ano >= config$ano_inicio & ano <= config$ano_fim &
    !chave %in% c("PARA", "PARA (GERAL)")
  if (!any(manter)) stop("Sem dados municipais no periodo configurado.")
  base <- data.frame(
    Localidade = local[manter],
    Data = as.Date(sprintf("%04d-%02d-01", ano[manter], mes[manter])),
    Real = qtd[manter]
  )
  # aggregate com vetor/by preserva NA; a variante por formula pode remove-los.
  mensal <- stats::aggregate(
    base$Real, by = base[c("Localidade", "Data")],
    FUN = function(x) if (anyNA(x)) NA_real_ else sum(x)
  )
  names(mensal)[3L] <- "Real"
  datas <- seq(as.Date(sprintf("%d-01-01", config$ano_inicio)),
               as.Date(sprintf("%d-12-01", config$ano_fim)), by = "month")
  resultado <- lapply(sort(unique(mensal$Localidade)), function(nome) {
    parte <- mensal[mensal$Localidade == nome, ]
    data.frame(Localidade = nome, Data = datas,
               Real = parte$Real[match(datas, parte$Data)])
  })
  do.call(rbind, resultado)
}

variaveis_xgboost <- function(valores, datas, lags) {
  stopifnot(length(valores) == length(datas), length(valores) > 0L)
  x <- data.frame(mes = as.integer(format(datas, "%m")))
  for (k in lags) {
    x[[paste0("lag_", k)]] <- if (length(valores) > k) {
      c(rep(NA_real_, k), head(valores, -k))
    } else rep(NA_real_, length(valores))
  }
  as.matrix(x)
}

prever_recursivo_xgboost <- function(
    modelo, historico, datas, lags,
    preditor = function(modelo, x) as.numeric(stats::predict(modelo, newdata = x))
) {
  valores <- historico
  previsoes <- numeric(length(datas)); corrigidas <- 0L
  for (i in seq_along(datas)) {
    n <- length(valores)
    atrasos <- vapply(lags, function(k) {
      if (n + 1L > k) valores[n + 1L - k] else NA_real_
    }, numeric(1))
    x <- matrix(c(as.integer(format(datas[i], "%m")), atrasos), nrow = 1L)
    colnames(x) <- c("mes", paste0("lag_", lags))
    p <- preditor(modelo, x)
    if (length(p) != 1L || !is.finite(p)) stop("Previsao nao finita.")
    corrigidas <- corrigidas + as.integer(p < 0)
    previsoes[i] <- max(0, p)
    valores <- c(valores, previsoes[i])
  }
  list(previsao = previsoes, negativas_corrigidas = corrigidas)
}

metricas_xgboost <- function(real, previsto) {
  manter <- is.finite(real) & is.finite(previsto)
  r <- real[manter]; p <- previsto[manter]; erro <- p - r
  nz <- r != 0
  denominador <- sum((r - mean(r))^2)
  c(N = length(r), N_MAPE = sum(nz),
    MAE = if (length(r)) mean(abs(erro)) else NA_real_,
    RMSE = if (length(r)) sqrt(mean(erro^2)) else NA_real_,
    MAPE = if (any(nz)) mean(abs(erro[nz] / r[nz])) * 100 else NA_real_,
    R2 = if (length(r) >= 2L && denominador > 0) {
      1 - sum(erro^2) / denominador
    } else NA_real_)
}

ajustar_xgboost <- function(x, y, config) {
  treino <- xgboost::xgb.DMatrix(x, label = y, missing = NA_real_)
  xgboost::xgb.train(params = config$parametros, data = treino,
                     nrounds = config$nrounds, verbose = 0)
}

executar_xgboost_local <- function(config = config_xgboost_local) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Instale o pacote R xgboost antes de executar este produtor.")
  }
  stopifnot(config$ano_inicio < config$ano_teste,
            config$ano_teste == config$ano_fim, config$passos_futuro > 0,
            length(config$lags) > 0L, all(config$lags >= 1L),
            all(config$lags == as.integer(config$lags)),
            config$nrounds > 0, config$minimo_treino > 0)
  mensal <- ler_mensal_xgboost(config)
  nomes <- sort(unique(mensal$Localidade))
  resultados <- avaliacoes <- diagnosticos <- list()
  corte <- as.Date(sprintf("%d-01-01", config$ano_teste))
  for (nome in nomes) {
    serie <- mensal[mensal$Localidade == nome, ]
    x <- variaveis_xgboost(serie$Real, serie$Data, config$lags)
    # Primeiros max(lags) meses servem como contexto, nao como alvos de ajuste.
    elegivel <- seq_len(nrow(serie)) > max(config$lags) & is.finite(serie$Real)
    treino <- which(elegivel & serie$Data < corte)
    final <- which(elegivel)
    diag <- data.frame(
      Localidade = nome, Meses_observados = sum(is.finite(serie$Real)),
      Meses_ausentes = sum(is.na(serie$Real)), N_Treino = length(treino),
      N_Ajuste_Final = length(final), Status = "ok"
    )
    if (length(treino) < config$minimo_treino) {
      diag$Status <- "ignorado_historico_insuficiente"
      diagnosticos[[nome]] <- diag
      message(nome, ": ignorado; ", length(treino), " alvos de treino.")
      next
    }
    # Cada chamada cria modelos novos e independentes para este municipio.
    modelo_teste <- ajustar_xgboost(x[treino, , drop = FALSE], serie$Real[treino], config)
    teste <- serie$Data >= corte
    historico <- serie$Real[!teste]
    pred_teste <- prever_recursivo_xgboost(
      modelo_teste, historico, serie$Data[teste], config$lags
    )
    observado <- serie$Real[teste]
    m <- metricas_xgboost(observado, pred_teste$previsao)
    # Sazonal ingenuo para os 12 meses de 2025: repetir os meses de 2024.
    sazonal <- tail(historico, 12L)
    pares <- is.finite(observado) & is.finite(sazonal)
    comum_x <- metricas_xgboost(observado[pares], pred_teste$previsao[pares])
    comum_s <- metricas_xgboost(observado[pares], sazonal[pares])
    modelo_final <- ajustar_xgboost(x[final, , drop = FALSE], serie$Real[final], config)
    datas_futuras <- seq(tail(serie$Data, 1L), by = "month",
                         length.out = config$passos_futuro + 1L)[-1L]
    pred_futuro <- prever_recursivo_xgboost(
      modelo_final, serie$Real, datas_futuras, config$lags
    )
    previstos <- rep(NA_real_, nrow(serie))
    previstos[teste] <- pred_teste$previsao
    resultados[[nome]] <- data.frame(
      Localidade = nome, Data = c(serie$Data, datas_futuras),
      Real = c(serie$Real, rep(NA_real_, config$passos_futuro)),
      Previsao_XGBOOST = c(previstos, pred_futuro$previsao), Modelo = "XGBOOST",
      AIC = NA_real_, BIC = NA_real_,
      Particao = c(ifelse(teste, "teste", "treino"),
                   rep("futuro", config$passos_futuro))
    )
    avaliacoes[[nome]] <- data.frame(
      Localidade = nome, N_Teste = m[["N"]], N_MAPE = m[["N_MAPE"]],
      MAE = m[["MAE"]], RMSE = m[["RMSE"]], MAPE = m[["MAPE"]], R2 = m[["R2"]],
      N_Comum_Sazonal = comum_s[["N"]],
      RMSE_XGBOOST_Comum = comum_x[["RMSE"]], RMSE_Sazonal_Comum = comum_s[["RMSE"]],
      Melhora_RMSE_pct = if (is.finite(comum_s[["RMSE"]]) && comum_s[["RMSE"]] > 0) {
        100 * (1 - comum_x[["RMSE"]] / comum_s[["RMSE"]])
      } else NA_real_,
      Negativas_Teste = pred_teste$negativas_corrigidas,
      Negativas_Futuro = pred_futuro$negativas_corrigidas
    )
    diagnosticos[[nome]] <- diag
    message(nome, ": teste=", m[["N"]], "; RMSE=", signif(m[["RMSE"]], 5L))
  }
  diagnostico <- do.call(rbind, diagnosticos)
  dir.create(dirname(config$diagnostico), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(diagnostico, config$diagnostico, row.names = FALSE,
                   fileEncoding = "UTF-8", na = "NA")
  if (!length(resultados)) stop("Nenhum municipio atingiu o minimo de treinamento.")
  previsoes <- do.call(rbind, resultados)
  avaliacao <- do.call(rbind, avaliacoes)
  dir.create(dirname(config$saida), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(config$avaliacao), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(config$execucao), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(previsoes, config$saida, row.names = FALSE,
                   fileEncoding = "UTF-8", na = "NA")
  utils::write.csv(avaliacao, config$avaliacao, row.names = FALSE,
                   fileEncoding = "UTF-8", na = "NA")
  registro <- list(
    Concluido_em = format(Sys.time(), tz = "America/Sao_Paulo", usetz = TRUE),
    R = R.version.string,
    xgboost = as.character(utils::packageVersion("xgboost")),
    MD5_Entrada = unname(tools::md5sum(config$entrada)), Configuracao = config,
    Municipios_Processados = nrow(avaliacao), Municipios_Ignorados = sum(diagnostico$Status != "ok"),
    Politica_Ausencias = "Calendario com NA; zero explicito preservado; alvo NA excluido.",
    Escopo = "Modelos municipais independentes; sem agregado estadual."
  )
  dput(registro, file = config$execucao)
  message("XGBOOST_LOCAL_CONCLUIDO: ", nrow(avaliacao), " municipios; ", nrow(previsoes), " linhas.")
  invisible(list(previsoes = previsoes, avaliacao = avaliacao, diagnostico = diagnostico))
}

# source() apenas disponibiliza as funcoes; Rscript executa o produtor.
if (sys.nframe() == 0L) executar_xgboost_local()
