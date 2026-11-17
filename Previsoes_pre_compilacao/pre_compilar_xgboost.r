# source("Previsoes_pre_compilacao/pre_compilar_xgboost.r", encoding = "UTF-8")

# XGBoost CRAN: modelos independentes por municipio e para PARA (Geral).
# Revisao: XGB-CRAN-LOCAL-ESTADO-6-DIF1-VALIDACAO-PARALELO; somente CSV mensal, erros no Shiny.
# Execucao EXCLUSIVAMENTE na maquina local Windows/VS Code, R 4.5.3.
# Rscript --vanilla Previsoes_pre_compilacao/pre_compilar_xgboost.r
# No console: source("Previsoes_pre_compilacao/pre_compilar_xgboost.r", encoding = "UTF-8")
# Dependencia: xgboost do CRAN (referencia 3.2.1.1; R >= 4.3.0).
# Instale separadamente: install.packages("xgboost", repos="http://cran.rstudio.com/")
# O produtor nao instala pacotes e nao deve ser executado na nuvem.
#
# PIPELINE: configuracao -> leitura -> agregacao/calendario -> separacao temporal
# -> diferencas -> lags/alvos -> elegibilidade -> ajuste -> recursao/reconstrucao
# -> unico CSV. Cada etapa tem seu bloco junto ao codigo que a executa.
# O alvo e a variacao mensal d[t] = y[t] - y[t-1], nao a quantidade absoluta.
# Nao ha log, padronizacao, diferenca sazonal ou coeficiente de crescimento imposto.
# Referencia metodologica (adaptacao em R com os lags 1, 2 e 12):
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
# Regra: diferenciar uma vez, com intervalo de UM mes; c(1,2,12) sao lags das
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
# Entrada: diferencas d[t], suas datas e lags c(1,2,12).
# Operacao: alinhar mes e lags das diferencas para prever d[t].
# Saida: matriz numerica com mes e lag_k, mantendo todas as datas.
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

# ---- 1. RECORTES E LOCALIDADES: MESMA API PARA PRODUCAO E VALIDACAO ------------
# Os anos/parametros padrao acima permanecem iguais ao checkpoint. O controlador
# fornece inicio/fim e meses_teste; o source direto conserva teste2025+72meses.
xgboost_chave_localidade <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC", x)
  x <- trimws(gsub("[^A-Z0-9]+", " ", x))
  x[x %in% c("PARA", "PARA GERAL")] <- "PARA GERAL"
  x
}

xgboost_localidades <- function(mensal, selecionadas = NULL) {
  disponiveis <- unique(as.character(mensal$Localidade))
  if (is.null(selecionadas)) return(disponiveis)
  if (!length(selecionadas) || anyNA(selecionadas)) stop("Selecione ao menos uma localidade.")
  idx <- match(xgboost_chave_localidade(selecionadas), xgboost_chave_localidade(disponiveis))
  if (anyNA(idx)) stop("Localidades nao encontradas: ", paste(selecionadas[is.na(idx)], collapse = ", "))
  unique(disponiveis[idx])
}

xgboost_periodos <- function(config) {
  inicio <- config[["inicio_treino"]]
  fim <- config[["fim_treino"]]
  meses <- config[["meses_teste"]]
  if (is.null(inicio)) inicio <- as.Date(sprintf("%04d-01-01", config$ano_inicio))
  if (is.null(fim)) fim <- as.Date(sprintf("%04d-01-01", config$ano_teste)) - 1L
  if (is.null(meses)) meses <- config$n_teste
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  recortes <- new.env(parent = baseenv())
  source(caminho, local = recortes, encoding = "UTF-8")
  recortes$definir_periodos_validacao(list(inicio_treino = inicio,
    fim_treino = fim, meses_teste = meses))
}

xgboost_validar_mensal <- function(mensal) {
  cobertura <- c("Municipios_com_registro", "Municipios_com_total_conhecido", "Municipios_esperados")
  if (!is.data.frame(mensal) || !all(c("Localidade", "Data", "Real", cobertura) %in% names(mensal))) {
    stop("Dados mensais devem conter Localidade, Data, Real e as tres colunas de cobertura do leitor.")
  }
  mensal$Localidade <- as.character(mensal$Localidade)
  mensal$Data <- as.Date(mensal$Data)
  if (anyNA(mensal$Localidade) || any(!nzchar(trimws(mensal$Localidade))) ||
      anyNA(mensal$Data) || any(format(mensal$Data, "%d") != "01") ||
      !is.numeric(mensal$Real) || any(!is.na(mensal$Real) & (!is.finite(mensal$Real) | mensal$Real < 0))) {
    stop("Localidades, datas ou quantidades invalidas nas series mensais.")
  }
  chave <- data.frame(Localidade = xgboost_chave_localidade(mensal$Localidade), Data = mensal$Data)
  if (anyDuplicated(chave)) stop("Serie mensal com localidade/data duplicada.")
  for (nome in cobertura) {
    v <- mensal[[nome]]
    if (!is.numeric(v) || any(!is.na(v) & (!is.finite(v) | v < 0 | v != floor(v)))) {
      stop("Cobertura invalida em ", nome)
    }
  }
  mensal
}

# Regularizar uma localidade no calendario da rodada. Nao agregar novamente,
# especialmente para o estado, que ja foi somado antes do filtro municipal.
xgboost_serie_local <- function(mensal, localidade, periodos) {
  parte <- mensal[xgboost_chave_localidade(mensal$Localidade) ==
    xgboost_chave_localidade(localidade), , drop = FALSE]
  datas <- seq(periodos$inicio_treino, tail(periodos$datas_teste, 1L), by = "month")
  idx <- match(datas, parte$Data)
  serie <- parte[idx, , drop = FALSE]
  serie$Localidade <- rep(localidade, length(datas))
  serie$Data <- datas
  ausentes <- is.na(idx)
  serie$Municipios_com_registro[ausentes] <- 0L
  serie$Municipios_com_total_conhecido[ausentes] <- 0L
  esperados <- unique(parte$Municipios_esperados[is.finite(parte$Municipios_esperados)])
  if (length(esperados) != 1L) stop("Cobertura esperada ausente/inconsistente para ", localidade)
  serie$Municipios_esperados[ausentes] <- esperados
  rownames(serie) <- NULL
  serie
}

# ---- 2. PAI: GRAVAR UM CSV APOS REUNIR TODAS AS LOCALIDADES --------------------
# Escrita temporaria/renomeacao preservam a saida anterior se a gravacao falhar.
gravar_csv_xgboost <- function(previsoes, saida) {
  dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile("xgboost_novo_", tmpdir = dirname(saida), fileext = ".csv")
  anterior <- NULL
  on.exit({
    unlink(temporario)
    if (!is.null(anterior) && file.exists(anterior) && !file.exists(saida)) file.rename(anterior, saida)
  }, add = TRUE)
  utils::write.csv(previsoes, temporario, row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
  if (file.exists(saida)) {
    anterior <- tempfile("xgboost_anterior_", tmpdir = dirname(saida), fileext = ".csv")
    if (!file.rename(saida, anterior)) stop("Nao foi possivel preservar a saida anterior: ", saida)
  }
  if (!file.rename(temporario, saida)) stop("Nao foi possivel concluir o CSV: ", saida)
  if (!is.null(anterior)) unlink(anterior)
  invisible(saida)
}

# ---- 3. TAREFA: UM AJUSTE E UMA RECURSAO PARA UMA LOCALIDADE -------------------
# WORKER no modo paralelo, PAI no sequencial. Nenhuma escrita de arquivo aqui.
xgboost_log_progresso <- function(localidade, periodos, ...) {
  texto <- trimws(gsub("[\r\n]+", " | ", paste0(..., collapse = "")))
  cat(paste0("[PID ", Sys.getpid(), " | teste ", format(periodos$inicio_teste, "%m/%Y"),
    " a ", format(periodos$fim_teste, "%m/%Y"), "] ", localidade, ": ", texto, "\n"))
  flush(stdout())
  invisible(NULL)
}

prever_localidade_xgboost <- function(mensal, localidade, periodos, futuro, config) {
  inicio <- proc.time()[["elapsed"]]
  opcoes <- options(OutDec = ".")
  on.exit(options(opcoes), add = TRUE)
  erro <- NULL
  avisos <- character()
  xgboost_log_progresso(localidade, periodos, "iniciado")
  resultado <- tryCatch(withCallingHandlers({
    # ETAPA04: treino separado ANTES da diferenciacao e construcao dos lags.
    # Real do teste sera copiado na saida, sem alimentar ajuste ou recursao.
    serie <- xgboost_serie_local(mensal, localidade, periodos)
    periodo_treino <- serie$Data <= periodos$fim_treino
    teste <- serie$Data %in% periodos$datas_teste
    historico <- serie$Real[periodo_treino]
    datas_treino <- serie$Data[periodo_treino]
    if (!length(historico) || !is.finite(tail(historico, 1L))) {
      stop("ultimo mes do treinamento desconhecido; sem ancora para reconstruir.")
    }
    # ETAPAS05/06 preservadas: primeira diferenca absoluta e lags configurados.
    diferencas <- diferenciar_mensal_xgboost(historico)
    x <- variaveis_xgboost(diferencas, datas_treino, config$lags)
    # ETAPA07: mesmo contexto max(lags)+1; nao remover meses com NA da serie.
    contexto <- max(config$lags) + config$diferenciacao
    treino <- which(seq_along(diferencas) > contexto & is.finite(diferencas))
    if (length(treino) < config$minimo_treino) {
      stop(length(treino), " alvos diferenciados conhecidos; minimo ", config$minimo_treino, ".")
    }
    xgboost_log_progresso(localidade, periodos, length(treino), " alvos de treino")
    # ETAPAS08/09: somente treino; uma recursao continua por todo o horizonte.
    modelo <- ajustar_xgboost(x[treino, , drop = FALSE], diferencas[treino], config)
    datas_prever <- c(periodos$datas_teste, futuro)
    pred <- prever_recursivo_xgboost(modelo, historico, datas_prever, config$lags)
    if (length(pred$previsao) != length(datas_prever) || any(!is.finite(pred$previsao))) {
      stop("Previsoes com tamanho ou valores invalidos.")
    }
    if (pred$negativas_corrigidas > 0L) message(pred$negativas_corrigidas,
      " quantidades reconstruidas negativas limitadas a zero apenas na saida.")
    previstos <- rep(NA_real_, nrow(serie))
    stopifnot(sum(teste) == periodos$meses_teste, identical(serie$Data[teste], periodos$datas_teste))
    previstos[teste] <- head(pred$previsao, periodos$meses_teste)
    # ETAPA10: manter esquema mensal e cobertura; somente o PAI gravara o CSV.
    data.frame(Localidade = localidade, Data = c(serie$Data, futuro),
      Real = c(serie$Real, rep(NA_real_, length(futuro))),
      Previsao_XGBOOST = c(previstos, tail(pred$previsao, length(futuro))),
      Modelo = "XGBOOST", AIC = NA_real_, BIC = NA_real_,
      Particao = c(ifelse(teste, "teste", "treino"), rep("futuro", length(futuro))),
      Municipios_com_registro = c(serie$Municipios_com_registro, rep(NA, length(futuro))),
      Municipios_com_total_conhecido = c(serie$Municipios_com_total_conhecido, rep(NA, length(futuro))),
      Municipios_esperados = serie$Municipios_esperados[1L])
  }, message = function(condicao) {
    avisos <<- c(avisos, conditionMessage(condicao))
    xgboost_log_progresso(localidade, periodos, conditionMessage(condicao))
    invokeRestart("muffleMessage")
  }, warning = function(condicao) {
    avisos <<- c(avisos, paste0("Aviso: ", conditionMessage(condicao)))
    xgboost_log_progresso(localidade, periodos, "Aviso: ", conditionMessage(condicao))
    invokeRestart("muffleWarning")
  }), error = function(condicao) {
    erro <<- conditionMessage(condicao)
    NULL
  })
  segundos <- unname(proc.time()[["elapsed"]] - inicio)
  xgboost_log_progresso(localidade, periodos,
    if (is.null(erro)) "concluido" else paste0("ignorado; ", erro),
    "; ", round(segundos, 2L), " s")
  list(dados = resultado, erro = erro, avisos = avisos, localidade = localidade, segundos = segundos)
}

# ---- 4. CLUSTER PSOCK: INICIALIZAR UMA VEZ PARA AS CINCO JANELAS ---------------
# PAI cria; WORKERS importam apenas funcoes. Em sucesso o controlador assume
# a responsabilidade de encerrar o cluster; falha na inicializacao o encerra.
criar_cluster_xgboost <- function(processos, script_pre_compilacao, raiz = getwd()) {
  if (!is.numeric(processos) || length(processos) != 1L || !is.finite(processos) ||
      processos < 1 || processos > .Machine$integer.max || processos != floor(processos)) {
    stop("processos deve ser um inteiro positivo.")
  }
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  script <- normalizePath(script_pre_compilacao, winslash = "/", mustWork = TRUE)
  nomes <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS")
  anteriores <- Sys.getenv(nomes, unset = NA_character_)
  on.exit({
    ausentes <- is.na(anteriores)
    if (any(ausentes)) Sys.unsetenv(nomes[ausentes])
    if (any(!ausentes)) do.call(Sys.setenv, as.list(anteriores[!ausentes]))
  }, add = TRUE)
  do.call(Sys.setenv, as.list(stats::setNames(rep("1", length(nomes)), nomes)))
  cluster <- parallel::makePSOCKcluster(as.integer(processos), outfile = "")
  pronto <- FALSE
  on.exit(if (!pronto) try(parallel::stopCluster(cluster), silent = TRUE), add = TRUE)
  inicializar <- function(raiz, script) {
    setwd(raiz)
    if (!requireNamespace("xgboost", quietly = TRUE)) stop("Pacote xgboost ausente no processo R.")
    opcoes <- options(frota.xgboost.apenas_funcoes = TRUE)
    on.exit(options(opcoes), add = TRUE)
    options(OutDec = ".")
    produtor <- new.env(parent = globalenv())
    source(script, local = produtor, encoding = "UTF-8")
    if (!is.function(produtor$prever_localidade_xgboost)) stop("Atualize o produtor XGBoost por localidade.")
    assign(".frota_xgboost_produtor", produtor, envir = globalenv())
    invisible(NULL)
  }
  environment(inicializar) <- baseenv()
  invisible(parallel::clusterCall(cluster, inicializar, raiz, script))
  pronto <- TRUE
  cluster
}

# ---- 5. PAI: VALIDAR A RODADA, DISTRIBUIR TAREFAS E GRAVAR UM CSV --------------
# O cluster e emprestado; nunca e criado ou encerrado por esta funcao.
executar_xgboost_local <- function(config = config_xgboost_local, dados_mensais = NULL, cluster = NULL) {
  inicio <- proc.time()[["elapsed"]]
  opcoes <- options(OutDec = ".")
  on.exit({
    options(opcoes)
    segundos <- proc.time()[["elapsed"]] - inicio
    inteiros <- floor(segundos)
    message("TEMPO TOTAL DO PRE-COMPILADOR XGBOOST: ",
      sprintf("%02.0f:%02.0f:%02.0f", inteiros %/% 3600, (inteiros %% 3600) %/% 60, inteiros %% 60),
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  config <- utils::modifyList(config_xgboost_local, config, keep.null = TRUE)
  inteiro <- function(x, minimo) is.numeric(x) && length(x) == 1L && is.finite(x) &&
    x >= minimo && x <= .Machine$integer.max && x == floor(x)
  if (!inteiro(config$passos_futuro, 0) || !inteiro(config$nrounds, 1) ||
      !inteiro(config$minimo_treino, 1) || !identical(config$diferenciacao, 1L) ||
      !is.numeric(config$lags) || !length(config$lags) || any(!is.finite(config$lags)) ||
      any(config$lags < 1 | config$lags != floor(config$lags)) || anyDuplicated(config$lags)) {
    stop("Conferir horizonte, nrounds, minimo_treino, diferenciacao=1L e lags inteiros positivos unicos.")
  }
  if (!is.character(config$saida) || length(config$saida) != 1L || is.na(config$saida) || !nzchar(config$saida)) {
    stop("Informe um caminho de saida valido.")
  }
  periodos <- xgboost_periodos(config)
  config$n_teste <- periodos$meses_teste
  # Seis processos fazem a paralelizacao; evitar varias threads XGBoost em
  # cada worker. Isso nao modifica lags, alvo, arvores ou taxas do modelo.
  if (!is.null(cluster)) config$parametros$nthread <- 1L
  if (getRversion() < "4.3.0") stop("O xgboost atual do CRAN exige R >= 4.3.0.")
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop('Instale no seu R local: install.packages("xgboost", repos="http://cran.rstudio.com/")')
  }
  if (is.null(dados_mensais)) {
    leitura <- config
    leitura$ano_inicio <- as.integer(format(periodos$inicio_treino, "%Y"))
    leitura$ano_fim <- as.integer(format(periodos$fim_teste, "%Y"))
    dados_mensais <- ler_mensal_xgboost(leitura)
  }
  mensal <- xgboost_validar_mensal(dados_mensais)
  localidades <- xgboost_localidades(mensal, config$localidades)
  mensal <- mensal[mensal$Data >= periodos$inicio_treino & mensal$Data <= periodos$fim_teste, , drop = FALSE]
  futuro <- if (config$passos_futuro > 0L) seq(periodos$fim_teste + 1L, by = "month",
    length.out = as.integer(config$passos_futuro)) else as.Date(character())
  message("XGBOOST: treino ", periodos$inicio_treino, " a ", periodos$fim_treino,
    "; teste ", periodos$inicio_teste, " a ", periodos$fim_teste, ".")
  if (is.null(cluster)) {
    calculados <- lapply(localidades, function(localidade) {
      prever_localidade_xgboost(mensal, localidade, periodos, futuro, config)
    })
  } else {
    # ---- INICIO DA DISTRIBUICAO PARALELA ------------------------------------
    if (!inherits(cluster, "cluster")) stop("cluster deve ser um cluster do pacote parallel.")
    contexto <- list(mensal = mensal, periodos = periodos, futuro = futuro, config = config)
    preparar <- function(contexto) {
      assign(".frota_xgboost_contexto", contexto, envir = globalenv())
      invisible(NULL)
    }
    trabalhar <- function(localidade) {
      contexto <- get(".frota_xgboost_contexto", envir = globalenv(), inherits = FALSE)
      produtor <- get(".frota_xgboost_produtor", envir = globalenv(), inherits = FALSE)
      produtor$prever_localidade_xgboost(contexto$mensal, localidade, contexto$periodos,
        contexto$futuro, contexto$config)
    }
    # Enviar somente contexto/funcoes sem serializar conexoes PSOCK.
    environment(preparar) <- environment(trabalhar) <- baseenv()
    invisible(parallel::clusterCall(cluster, preparar, contexto))
    message("XGBOOST: ", length(cluster), " processos; fila dinamica de ", length(localidades), " localidades.")
    calculados <- parallel::parLapplyLB(cluster, localidades, trabalhar, chunk.size = 1L)
    # ---- FIM DA DISTRIBUICAO PARALELA ---------------------------------------
  }
  resultados <- list()
  for (r in calculados) if (is.null(r$erro)) resultados[[r$localidade]] <- r$dados
  message("XGBOOST: ", length(resultados), "/", length(calculados), " localidades concluidas.")
  if (!length(resultados)) stop("Nenhuma localidade processada; CSV anterior preservado.")
  previsoes <- do.call(rbind, resultados)
  rownames(previsoes) <- NULL
  if (anyDuplicated(previsoes[c("Localidade", "Data")])) stop("Previsoes com datas duplicadas.")
  gravar_csv_xgboost(previsoes, config$saida)
  message("XGBOOST_LOCAL_CONCLUIDO: ", length(resultados), " localidades; ", nrow(previsoes), " linhas.")
  invisible(list(previsoes = previsoes, diagnosticos = calculados))
}

# ---- 6. ENTRADA: SOURCE DIRETO OU IMPORTACAO SOMENTE DAS FUNCOES ---------------
if (!isTRUE(getOption("frota.xgboost.apenas_funcoes", FALSE))) executar_xgboost_local()

