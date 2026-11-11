# source("Previsoes_pre_compilacao/pre_compilar_xgboost.r", encoding = "UTF-8")

# XGBoost CRAN: modelos independentes por municipio e para PARA (Geral).
# Revisao: XGB-CRAN-LOCAL-ESTADO-4-DIF1; somente CSV mensal, erros no Shiny.
# Execucao EXCLUSIVAMENTE na maquina local Windows/VS Code, R 4.5.3.
# Rscript --vanilla pre_compilar_xgboost.r
# No console: source("pre_compilar_xgboost.r", encoding = "UTF-8")
# Dependencia: xgboost do CRAN (referencia 3.2.1.1; R >= 4.3.0).
# Instale separadamente: install.packages("xgboost", repos="http://cran.rstudio.com/")
# O produtor nao instala pacotes e nao deve ser executado na nuvem.
#
# PIPELINE: configuracao -> leitura -> agregacao/calendario -> separacao temporal
# -> diferencas -> lags/alvos -> elegibilidade -> ajuste -> recursao/reconstrucao
# -> unico CSV. Cada etapa tem seu bloco junto ao codigo que a executa.
# O alvo e a variacao mensal d[t] = y[t] - y[t-1], nao a quantidade absoluta.
# Nao ha log, padronizacao, diferenca sazonal ou coeficiente de crescimento imposto.
# Referencia metodologica (adaptacao em R com apenas os lags 1 e 12):
# https://skforecast.org/latest/user_guides/time-series-differentiation.html
#
# ESCOPO: cada municipio e PARA (Geral) recebem ajustes separados; nao ha modelo
# global nem soma de previsoes municipais para gerar a serie estadual.
# O produtor gera somente o CSV mensal. Os erros de previsao sao calculados
# em runtime no painel, a partir de Real e Previsao, como nos demais modelos.
# Um teste de 12 meses nao valida, sozinho, a precisao dos seis anos futuros.

# =============================================================================
# ETAPA 01 — CONFIGURACAO E EXECUCAO LOCAL
# Entrada: CSV geral e parametros fixos abaixo, relativos a raiz do projeto.
# Saida: configuracao de um modelo por municipio e um proprio para PARA (Geral).
# Regra: diferenciar uma vez, com intervalo de UM mes; c(1,12) sao lags das
# diferencas. differentiation nao e um argumento de xgb.train no pacote R.
# A execucao usa ponto decimal internamente e restaura a opcao OutDec ao sair.
# =============================================================================
config_xgboost_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_xgboost_pre_comp.csv",
  ano_inicio = 2016L, ano_teste = 2025L, ano_fim = 2025L,
  n_teste = 12L, passos_futuro = 72L, lags = c(1,2,12),
  diferenciacao = 1L, minimo_treino = 24L,
  nrounds = 300L,
  parametros = list(
    objective = "reg:squarederror", tree_method = "hist",
    max_depth = 3L, eta = 0.05, min_child_weight = 3,
    subsample = 1, colsample_bytree = 1, nthread = 1L, seed = 41L
  )
) 

# =============================================================================
# ETAPA 02 — LEITURA E VALIDACAO DA ENTRADA
# Entrada: LOCAL, ANO, MES, TIPO_VEICULO e QTD_VEICULO do CSV geral.
# Operacao: validar colunas, datas, localidades e quantidades nao negativas.
# Saida: registros municipais de 2016–2025 na configuracao padrao.
# Regra: zero explicito e valido; quantidade desconhecida permanece NA.
# =============================================================================
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
  # ==========================================================================
  # ETAPA 03 — AGREGACAO E CALENDARIO MENSAL
  # Entrada: registros municipais validados, sem totais estaduais duplicados.
  # Operacao: somar tipos por municipio/mes e completar a grade mensal com NA.
  # Saida: series regulares, estado observado e colunas de cobertura.
  # Regra: NA em uma quantidade do grupo torna seu total NA. A soma estadual
  # usa registros observados; cobertura parcial nao e imputacao de municipios.
  # ==========================================================================
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

# =============================================================================
# ETAPA 05 — DIFERENCIACAO MENSAL DE PRIMEIRA ORDEM
# Entrada: quantidades do treinamento em calendario regular, antes de 2025.
# Operacao: d[t] = y[t] - y[t-1], uma vez e com intervalo de um mes.
# Saida: vetor de mesmo comprimento; primeira posicao e pares ausentes = NA.
# Regra: nao remover meses e nao substituir ausencias por zero. Diferencas
# negativas sao validas; o sinal representa diminuicao da quantidade mensal.
# =============================================================================
diferenciar_mensal_xgboost <- function(valores) {
  stopifnot(is.numeric(valores), length(valores) > 0L)
  c(NA_real_, diff(valores, lag = 1L, differences = 1L))
}

# =============================================================================
# ETAPA 06 — VARIAVEIS EXPLICATIVAS E ALVO
# Entrada: diferencas d[t], suas datas e lags c(1,12).
# Operacao: alinhar mes, d[t-1] e d[t-12] para prever d[t].
# Saida: matriz numerica com mes, lag_1 e lag_12, mantendo todas as datas.
# Regra: um lag ausente continua NA; nunca saltar a lacuna no calendario.
# =============================================================================
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

# =============================================================================
# ETAPA 08 — AJUSTE EFETIVO DO XGBOOST
# Entrada: matriz de variaveis e alvos diferenciados elegiveis do treinamento.
# Operacao: criar DMatrix e ajustar com xgb.train, parametros e sementes fixos.
# Saida: um unico modelo ajustado para esta localidade.
# Regra: nao treinar com Real de 2025 nem reajustar antes do horizonte futuro.
# =============================================================================
ajustar_xgboost <- function(x, y, config) {
  # Fixar tambem o gerador do R, alem da seed do motor XGBoost.
  set.seed(config$parametros$seed)
  treino <- xgboost::xgb.DMatrix(x, label = y, missing = NA_real_)
  xgboost::xgb.train(params = config$parametros, data = treino,
                     nrounds = config$nrounds, verbose = 0)
}

# =============================================================================
# ETAPA 09 — PREVISAO RECURSIVA E RECONSTRUCAO DAS QUANTIDADES
# Entrada: modelo, quantidades ate dezembro/2024 e datas de 2025–2031.
# Operacao: prever 84 diferencas; cada diferenca prevista alimenta os lags
# seguintes. Reconstruir y_prev[T+h] = y[T] + soma(d_prev[1:h]).
# Saida: quantidades mensais na escala original; 12 de teste e 72 futuras.
# Regra: nao inserir Real/2025, reiniciar em 2026 ou cortar diferencas negativas.
# A ancora y[T] deve ser conhecida. Limitar somente quantidades reconstruidas
# negativas na saida; essa correcao NAO altera o buffer de diferencas previstas.
# =============================================================================
prever_recursivo_xgboost <- function(
    modelo, historico, datas, lags,
    preditor = function(modelo, x) as.numeric(stats::predict(modelo, newdata = x))
) {
  if (!length(historico) || !is.finite(tail(historico, 1L))) {
    stop("Ultimo mes do treinamento desconhecido; sem ancora para reconstruir.")
  }
  ancora <- tail(historico, 1L)
  valores <- diferenciar_mensal_xgboost(historico)
  diferencas_previstas <- numeric(length(datas))
  for (i in seq_along(datas)) {
    n <- length(valores)
    atrasos <- vapply(lags, function(k) {
      if (n + 1L > k) valores[n + 1L - k] else NA_real_
    }, numeric(1))
    x <- matrix(c(as.integer(format(datas[i], "%m")), atrasos), nrow = 1L)
    colnames(x) <- c("mes", paste0("lag_", lags))
    p <- preditor(modelo, x)
    if (length(p) != 1L || !is.finite(p)) stop("Diferenca prevista nao finita.")
    diferencas_previstas[i] <- p
    valores <- c(valores, p) # Buffer de DIFERENCAS, inclusive valores negativos.
  }
  reconstruidas <- as.numeric(ancora) + cumsum(diferencas_previstas)
  if (any(!is.finite(reconstruidas))) stop("Quantidade reconstruida nao finita.")
  negativas <- reconstruidas < 0
  list(previsao = pmax(0, reconstruidas), negativas_corrigidas = sum(negativas),
       negativas_por_passo = negativas)
}

executar_xgboost_local <- function(config = config_xgboost_local) {
  # global.R usa OutDec="," para exibicao. O XGBoost converte parametros com
  # as.character(): eta=0.05 viraria "0,05", rejeitado pelo parser do motor.
  # Proteger ajuste, previsao e exportacao; restaurar a sessao mesmo com erro.
  opcoes_anteriores <- options(OutDec = ".")
  on.exit(options(opcoes_anteriores), add = TRUE)
  if (getRversion() < "4.3.0") stop("O xgboost atual do CRAN exige R >= 4.3.0.")
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop('Instale no seu R local: install.packages("xgboost", repos="http://cran.rstudio.com/")')
  }
  stopifnot(config$ano_inicio < config$ano_teste,
            config$ano_teste == config$ano_fim, config$n_teste == 12L,
            identical(config$diferenciacao, 1L),
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
    # ========================================================================
    # ETAPA 04 — SEPARACAO TEMPORAL SEM VAZAMENTO
    # Entrada: serie mensal regular desta localidade.
    # Operacao: separar o historico anterior a janeiro/2025 ANTES de diferenciar
    # e montar os lags. Os valores Real/2025 so serao copiados para a saida.
    # Saida: historico/datas do treino; mascara dos 12 meses reservados ao teste.
    # Regra: dezembro/2024 deve ser conhecido, sem imputacao ou uso de Real/2025.
    # ========================================================================
    periodo_treino <- serie$Data < corte
    teste <- !periodo_treino
    historico <- serie$Real[periodo_treino]
    datas_treino <- serie$Data[periodo_treino]
    if (!length(historico) || !is.finite(tail(historico, 1L))) {
      message(nome, ": ignorado; ultimo mes do treinamento desconhecido (sem ancora).")
      next
    }
    # Executar etapas 05 e 06 somente com o historico separado acima.
    diferencas <- diferenciar_mensal_xgboost(historico)
    x <- variaveis_xgboost(diferencas, datas_treino, config$lags)
    # ========================================================================
    # ETAPA 07 — ELEGIBILIDADE DOS ALVOS DIFERENCIADOS
    # Entrada: diferencas e matriz do treino, com calendario preservado.
    # Operacao: reservar max(lags) + 1 meses de contexto e excluir alvos NA.
    # Saida: indices de pelo menos 24 alvos diferenciados conhecidos.
    # Regra: preditores NA sao aceitos. Com 108 meses completos de 2016–2024,
    # ficam 95 alvos: fevereiro/2017 a dezembro/2024, apos 13 meses de contexto.
    # ========================================================================
    contexto <- max(config$lags) + config$diferenciacao
    treino <- which(seq_along(diferencas) > contexto & is.finite(diferencas))
    if (length(treino) < config$minimo_treino) {
      message(nome, ": ignorado; ", length(treino), " alvos diferenciados de treino.")
      next
    }
    resultado <- tryCatch({
      # UNICO ajuste desta localidade: somente alvos anteriores ao teste.
      modelo <- ajustar_xgboost(x[treino, , drop = FALSE], diferencas[treino], config)
      stopifnot(sum(teste) == config$n_teste)
      datas_futuras <- seq(tail(serie$Data, 1L), by = "month",
                           length.out = config$passos_futuro + 1L)[-1L]
      # Uma unica recursao de 84 passos: nao realimentar com Real de 2025.
      pred <- prever_recursivo_xgboost(modelo, historico,
        c(serie$Data[teste], datas_futuras), config$lags)
      if (pred$negativas_corrigidas > 0L) {
        message(nome, ": ", pred$negativas_corrigidas,
                " quantidades reconstruidas negativas limitadas a zero na saida.")
      }
      # ======================================================================
      # ETAPA 10 — CONSOLIDACAO E EXPORTACAO DO UNICO CSV
      # Entrada: quantidades reconstruidas do teste/futuro e Real original.
      # Operacao: preservar esquema e cobertura; reunir todas as localidades.
      # Saida: Previsoes_output/previsoes_xgboost_pre_comp.csv, em uma escrita.
      # Regra: previsao NA no treino e Real NA no futuro; manter decimais.
      # Nao exportar diferencas, frota acumulada, metricas, modelos ou logs.
      # Analise, calculo dos erros e exibicao pertencem aos scripts de runtime.
      # ======================================================================
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
