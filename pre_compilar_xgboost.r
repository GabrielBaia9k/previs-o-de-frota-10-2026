# XGBoost CRAN: modelos independentes por municipio e para PARA (Geral).
# Revisao: XGB-CRAN-LOCAL-ESTADO-2; somente CSV mensal, erros calculados no Shiny.
# Execucao EXCLUSIVAMENTE na maquina local Windows/VS Code, R 4.5.3.
# Rscript --vanilla pre_compilar_xgboost.r
# No console: source("pre_compilar_xgboost.r", encoding = "UTF-8")
# Dependencia: xgboost do CRAN (referencia 3.2.1.1; R >= 4.3.0).
# Instale separadamente: install.packages("xgboost", repos="http://cran.rstudio.com/")
# O produtor nao instala pacotes e nao deve ser executado na nuvem.
#
# PREPARACAO DOCUMENTADA
# 1. Ler somente o CSV geral; somar os tipos de veiculo por municipio/mes.
#    Se uma quantidade do grupo for desconhecida, seu total permanece NA.
# 2. Recortar 2016-2025 e completar o calendario mensal com NA, nunca com zero.
#    Zeros explicitos sao validos. Linhas estaduais de entrada sao excluidas.
#    PARA (Geral) e construido somando os totais municipais observados do CSV.
#    Cobertura incompleta e explicitada nas colunas de cobertura: essa soma nao
#    estima quantidades desconhecidas nem prova cobertura integral do estado.
# 3. Usar apenas mes, lag de 1 mes e lag de 12 meses. As defasagens seguem
#    o calendario; nao pulam lacunas. Nao ha normalizacao nem tendencia ajustada.
#    XGBoost recebe NA nos preditores; alvos NA nao entram no treinamento.
# 4. Treinar ate dezembro/2024; prever todo 2025 recursivamente, sem ler os
#    valores reais de 2025 nos lags. Hiperparametros fixos, sem busca no teste.
# 5. Manter o MESMO ajuste para os 72 meses futuros, como nos classicos.
#    A recursao e unica: 12 meses de teste + 72 futuros, sem reiniciar em 2026
#    nem usar Real de 2025. O CSV contem 2026, mas esses registros ficam fora.
# 6. Manter previsoes decimais; limitar negativas a zero e contar correcoes.
#    Exportar NA na previsao do treino e no Real futuro. Nao exportar acumulados.
# 7. Exigir 24 alvos conhecidos apos os primeiros 12 meses para cada ajuste.
#    Municipios sem esse historico sao informados no console, sem fallback.
#
# ESCOPO: cada municipio e PARA (Geral) recebem ajustes separados; nao ha modelo
# global nem soma de previsoes municipais para gerar a serie estadual.
# Os resultados ainda nao foram adicionados aos seletores do Shiny.
# O produtor gera somente o CSV mensal. Os erros de previsao sao calculados
# em runtime no painel, a partir de Real e Previsao, como nos demais modelos.
# Um teste de 12 meses nao valida, sozinho, a precisao dos seis anos futuros.

config_xgboost_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_xgboost_pre_comp.csv",
  ano_inicio = 2016L, ano_teste = 2025L, ano_fim = 2025L,
  n_teste = 12L, passos_futuro = 72L, lags = c(1L, 12L), minimo_treino = 24L,
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
  texto_qtd <- trimws(dados$QTD_VEICULO)
  ausente <- is.na(texto_qtd) | toupper(texto_qtd) %in% c("", "NA", "NAN", "NULL")
  qtd <- numero(texto_qtd)
  qtd[ausente] <- NA_real_
  local <- trimws(dados$LOCAL)
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
  municipios <- sort(unique(mensal$Localidade))
  completar <- function(nome, parte, esperados = 1L,
                       registros = rep(1L, nrow(parte)),
                       conhecidos = as.integer(is.finite(parte$Real))) {
    i <- match(datas, parte$Data)
    nr <- registros[i]; nr[is.na(i)] <- 0L
    nc <- conhecidos[i]; nc[is.na(i)] <- 0L
    data.frame(Localidade = nome, Data = datas, Real = parte$Real[i],
      Municipios_com_registro = nr, Municipios_com_total_conhecido = nc,
      Municipios_esperados = esperados)
  }
  resultado <- lapply(municipios, function(nome) {
    completar(nome, mensal[mensal$Localidade == nome, ])
  })
  # Somar observacoes antes de completar a grade evita imputar municipios com 0.
  estado <- stats::aggregate(mensal$Real, by = mensal["Data"],
    FUN = function(x) if (anyNA(x)) NA_real_ else sum(x))
  names(estado)[2L] <- "Real"
  registros <- stats::aggregate(rep(1L, nrow(mensal)), by = mensal["Data"], FUN = sum)
  conhecidos <- stats::aggregate(as.integer(is.finite(mensal$Real)),
                                 by = mensal["Data"], FUN = sum)
  estado <- completar("PARÁ (Geral)", estado, length(municipios),
    registros$x[match(estado$Data, registros$Data)],
    conhecidos$x[match(estado$Data, conhecidos$Data)])
  do.call(rbind, c(list(estado), resultado))
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
  previsoes <- numeric(length(datas)); negativas <- logical(length(datas))
  for (i in seq_along(datas)) {
    n <- length(valores)
    atrasos <- vapply(lags, function(k) {
      if (n + 1L > k) valores[n + 1L - k] else NA_real_
    }, numeric(1))
    x <- matrix(c(as.integer(format(datas[i], "%m")), atrasos), nrow = 1L)
    colnames(x) <- c("mes", paste0("lag_", lags))
    p <- preditor(modelo, x)
    if (length(p) != 1L || !is.finite(p)) stop("Previsao nao finita.")
    negativas[i] <- p < 0
    previsoes[i] <- max(0, p)
    valores <- c(valores, previsoes[i])
  }
  list(previsao = previsoes, negativas_corrigidas = sum(negativas),
       negativas_por_passo = negativas)
}

ajustar_xgboost <- function(x, y, config) {
  # Fixar tambem o gerador do R, alem da seed do motor XGBoost.
  set.seed(config$parametros$seed)
  treino <- xgboost::xgb.DMatrix(x, label = y, missing = NA_real_)
  xgboost::xgb.train(params = config$parametros, data = treino,
                     nrounds = config$nrounds, verbose = 0)
}

executar_xgboost_local <- function(config = config_xgboost_local) {
  if (getRversion() < "4.3.0") stop("O xgboost atual do CRAN exige R >= 4.3.0.")
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop('Instale no seu R local: install.packages("xgboost", repos="http://cran.rstudio.com/")')
  }
  stopifnot(config$ano_inicio < config$ano_teste,
            config$ano_teste == config$ano_fim, config$n_teste == 12L,
            config$passos_futuro > 0,
            length(config$lags) > 0L, all(config$lags >= 1L),
            all(config$lags == as.integer(config$lags)),
            config$nrounds > 0, config$minimo_treino > 0)
  mensal <- ler_mensal_xgboost(config)
  nomes <- unique(mensal$Localidade) # Estado primeiro, depois cada municipio.
  resultados <- list()
  corte <- as.Date(sprintf("%d-01-01", config$ano_teste))
  for (nome in nomes) {
    serie <- mensal[mensal$Localidade == nome, ]
    x <- variaveis_xgboost(serie$Real, serie$Data, config$lags)
    # Primeiros max(lags) meses servem como contexto, nao como alvos de ajuste.
    elegivel <- seq_len(nrow(serie)) > max(config$lags) & is.finite(serie$Real)
    treino <- which(elegivel & serie$Data < corte)
    if (length(treino) < config$minimo_treino) {
      message(nome, ": ignorado; ", length(treino), " alvos de treino.")
      next
    }
    resultado <- tryCatch({
      # UNICO ajuste desta localidade: somente alvos anteriores ao teste.
      modelo <- ajustar_xgboost(x[treino, , drop = FALSE], serie$Real[treino], config)
      teste <- serie$Data >= corte
      stopifnot(sum(teste) == config$n_teste)
      historico <- serie$Real[!teste]
      datas_futuras <- seq(tail(serie$Data, 1L), by = "month",
                           length.out = config$passos_futuro + 1L)[-1L]
      # Uma unica recursao de 84 passos: nao realimentar com Real de 2025.
      pred <- prever_recursivo_xgboost(modelo, historico,
        c(serie$Data[teste], datas_futuras), config$lags)
      pred_teste <- head(pred$previsao, config$n_teste)
      previstos <- rep(NA_real_, nrow(serie))
      previstos[teste] <- pred_teste
      data.frame(
        Localidade = nome, Data = c(serie$Data, datas_futuras),
        Real = c(serie$Real, rep(NA_real_, config$passos_futuro)),
        Previsao_XGBOOST = c(previstos, tail(pred$previsao, config$passos_futuro)),
        Modelo = "XGBOOST", AIC = NA_real_, BIC = NA_real_,
        Particao = c(ifelse(teste, "teste", "treino"), rep("futuro", config$passos_futuro)),
        Municipios_com_registro = c(serie$Municipios_com_registro, rep(NA, config$passos_futuro)),
        Municipios_com_total_conhecido = c(serie$Municipios_com_total_conhecido,
                                          rep(NA, config$passos_futuro)),
        Municipios_esperados = serie$Municipios_esperados[1L]
      )
    }, error = function(e) e)
    if (inherits(resultado, "error")) {
      message(nome, ": erro; ", conditionMessage(resultado))
      next
    }
    resultados[[nome]] <- resultado
    message(nome, ": previsoes mensais geradas.")
  }
  if (!length(resultados)) stop("Nenhuma localidade processada; consulte as mensagens do console.")
  previsoes <- do.call(rbind, resultados)
  dir.create(dirname(config$saida), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(previsoes, config$saida, row.names = FALSE,
                   fileEncoding = "UTF-8", na = "NA")
  message("XGBOOST_LOCAL_CONCLUIDO: ", length(resultados), " localidades; ", nrow(previsoes), " linhas.")
  invisible(list(previsoes = previsoes))
}

# Padrao dos outros produtores: source() e Rscript executam na maquina local.
# Os testes locais usam esta opcao para carregar funcoes sem rodar a base real.
if (!isTRUE(getOption("frota.xgboost.apenas_funcoes", FALSE))) executar_xgboost_local()