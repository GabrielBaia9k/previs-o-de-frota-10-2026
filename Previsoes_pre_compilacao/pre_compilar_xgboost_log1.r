# source("Previsoes_pre_compilacao/pre_compilar_xgboost.r", encoding = "UTF-8")
# XGBoost CRAN: modelos independentes por municipio e para PARA (Geral).
# Revisao: XGB-CRAN-LOCAL-ESTADO-5-LOG1P-DIF1; CSV mensal, erros no Shiny.
# Execucao EXCLUSIVAMENTE na maquina local Windows/VS Code, R 4.5.3.
# Rscript --vanilla pre_compilar_xgboost.r
# No console: source("pre_compilar_xgboost.r", encoding = "UTF-8")
# Dependencia: xgboost do CRAN (referencia 3.2.1.1; R >= 4.3.0).
# Instale separadamente: install.packages("xgboost", repos="http://cran.rstudio.com/")
# O produtor nao instala pacotes e nao deve ser executado na nuvem.
#
# PIPELINE: configuracao -> leitura -> agregacao/calendario -> separacao temporal
# -> log1p/diferencas -> lags/alvos -> elegibilidade -> ajuste
# -> recursao/reconstrucao/expm1 -> unico CSV. Cada etapa tem um bloco comentado.
# O alvo e g[t] = log1p(y[t]) - log1p(y[t-1]); os lags tambem usam essa escala.
# A transformacao ocorre APOS somar tipos e municipios na escala original.
# log1p admite zero; NA continua ausente. Nao ha diferenca sazonal,
# padronizacao ou coeficiente manual para ampliar crescimento/sazonalidade.
# A inversao direta nao e automaticamente a media na escala original.
# Referencias metodologicas (adaptacao em R com lags 1, 2 e 12):
# https://otexts.com/fpp3/transformations.html
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
# Regra: log1p primeiro, depois diferenciar uma vez com intervalo de UM mes.
# c(1,2,12) sao lags das diferencas logaritmicas. transformacao e diferenciacao
# documentam as unicas operacoes implementadas; nao sao parametros de xgb.train.
# A execucao usa ponto decimal internamente e restaura a opcao OutDec ao sair.
# =============================================================================
config_xgboost_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_xgboost_pre_comp.csv",
  ano_inicio = 2010L, ano_teste = 2025L, ano_fim = 2025L,
  n_teste = 12L, passos_futuro = 72L, lags = c(1L, 2L, 12L),
  transformacao = "log1p", diferenciacao = 1L, minimo_treino = 24L,
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
# ETAPA 05 — LOG1P E DIFERENCIACAO MENSAL DE PRIMEIRA ORDEM
# Entrada: quantidades agregadas do treino, em calendario regular antes de 2025.
# Operacao: z[t] = log1p(y[t]); g[t] = z[t] - z[t-1], com intervalo de um mes.
# Saida: vetor de mesmo comprimento; primeira posicao e pares ausentes = NA.
# Regra: zeros explicitos sao validos; nao remover meses nem imputar ausencias.
# Diferencas logaritmicas negativas sao validas e indicam diminuicao mensal.
# Usar a MESMA funcao no treino e ao inicializar os lags da recursao.
# =============================================================================
diferenciar_mensal_xgboost <- function(valores) {
  stopifnot(is.numeric(valores), length(valores) > 0L)
  invalidos <- !is.na(valores) & (!is.finite(valores) | valores < 0)
  if (any(invalidos)) stop("log1p exige quantidades finitas nao negativas ou NA.")
  valores[is.na(valores)] <- NA_real_
  transformados <- log1p(valores)
  c(NA_real_, diff(transformados, lag = 1L, differences = 1L))
}

# =============================================================================
# ETAPA 06 — VARIAVEIS EXPLICATIVAS E ALVO
# Entrada: diferencas logaritmicas g[t], suas datas e lags c(1,2,12).
# Operacao: alinhar mes, g[t-1], g[t-2] e g[t-12] para prever g[t].
# Saida: matriz numerica com mes, lag_1, lag_2 e lag_12; manter todas as datas.
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
# Entrada: matriz de atributos e alvos logaritmicos diferenciados elegiveis.
# Operacao: criar DMatrix e ajustar com xgb.train, parametros e sementes fixos.
# Saida: um unico modelo ajustado para esta localidade.
# Regra: nao treinar com Real de 2025 nem reajustar antes do horizonte futuro.
# O erro quadratico do ajuste e calculado sobre g[t], que pode ser negativo.
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
# Operacao: prever 84 diferencas logaritmicas, alimentando os lags seguintes.
# Reconstruir z_prev[T+h] = log1p(y[T]) + soma(g_prev[1:h]); depois
# y_prev[T+h] = expm1(z_prev[T+h]), devolvendo quantidades de veiculos.
# Saida: quantidades mensais na escala original; 12 de teste e 72 futuras.
# Regra: nao inserir Real/2025, reiniciar em 2026 ou cortar diferencas negativas.
# A ancora y[T] deve ser conhecida. Limitar somente quantidades reconstruidas
# negativas na saida; essa correcao NAO altera o buffer de diferencas previstas.
# Sem correcao de vies de retransformacao nesta versao experimental; avaliar
# MAE/RMSE em veiculos no painel. Nao limitar previsoes positivas artificialmente.
# =============================================================================
prever_recursivo_xgboost <- function(
    modelo, historico, datas, lags,
    preditor = function(modelo, x) as.numeric(stats::predict(modelo, newdata = x))
) {
  if (!length(historico) || !is.finite(tail(historico, 1L))) {
    stop("Ultimo mes do treinamento desconhecido; sem ancora para reconstruir.")
  }
  valores <- diferenciar_mensal_xgboost(historico)
  ancora_log <- log1p(tail(historico, 1L))
  diferencas_previstas <- numeric(length(datas))
  for (i in seq_along(datas)) {
    n <- length(valores)
    atrasos <- vapply(lags, function(k) {
      if (n + 1L > k) valores[n + 1L - k] else NA_real_
    }, numeric(1))
    x <- matrix(c(as.integer(format(datas[i], "%m")), atrasos), nrow = 1L)
    colnames(x) <- c("mes", paste0("lag_", lags))
    p <- preditor(modelo, x)
    if (length(p) != 1L || !is.finite(p)) stop("Diferenca logaritmica prevista nao finita.")
    diferencas_previstas[i] <- p
    valores <- c(valores, p) # Buffer de DIFERENCAS LOGARITMICAS, inclusive negativas.
  }
  reconstruidas_log <- as.numeric(ancora_log) + cumsum(diferencas_previstas)
  if (any(!is.finite(reconstruidas_log))) stop("Reconstrucao logaritmica nao finita.")
  reconstruidas <- expm1(reconstruidas_log)
  if (any(!is.finite(reconstruidas))) {
    stop("Quantidade reconstruida nao finita apos expm1; verificar a estabilidade da recursao.")
  }
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
            identical(config$transformacao, "log1p"),
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
    # Operacao: separar o historico anterior a janeiro/2025 ANTES de log1p,
    # diferenciar e montar lags. Real/2025 so sera copiado para a saida.
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
    # Entrada: diferencas logaritmicas e matriz do treino; calendario preservado.
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
      # Uma unica recursao de 84 passos em g: nao realimentar com Real de 2025.
      pred <- prever_recursivo_xgboost(modelo, historico,
        c(serie$Data[teste], datas_futuras), config$lags)
      if (pred$negativas_corrigidas > 0L) {
        message(nome, ": ", pred$negativas_corrigidas,
                " quantidades reconstruidas negativas limitadas a zero na saida.")
      }
      # ======================================================================
      # ETAPA 10 — CONSOLIDACAO E EXPORTACAO DO UNICO CSV
      # Entrada: quantidades apos expm1 no teste/futuro e Real original.
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