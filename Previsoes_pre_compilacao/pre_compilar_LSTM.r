# ==============================================================================
# pre_compilar_LSTM.r | LSTM-LOCAL-3-CAUSAL-VALIDACAO
# Base: GitHub publicacao-codigo, 31e3667bc0f291db3c4ae0e2ff811d4712aa67f1.
# Executar somente no Windows/VS Code, na raiz do projeto:
# source("Previsoes_pre_compilacao/pre_compilar_LSTM.r", encoding = "UTF-8")
# Nunca executar produtores/treinamentos/testes de previsao na nuvem.
# Instalar dependencias separadamente na maquina local, se necessario:
# install.packages(c("keras3", "tensorflow", "reticulate"), repos="http://cran.rstudio.com/")
# O Python com TensorFlow/Keras deve ser o mesmo ja usado pelo usuario.
#
# PIPELINE: CSV -> calendario mensal -> corte externo -> corte interno ->
# tendencia/escala somente no treino interno -> tensores -> LSTM -> recursao
# continua teste+futuro -> CSV de previsao e historicos opcionais, somente no PAI.
# Um modelo por municipio e outro para PARA (Geral), sem soma de previsoes.
# Erros de previsao sao calculados pelo frontend; nao sao exportados aqui.
#
# Correcao metodologica: o teste nunca participa de tendencia, normalizacao,
# atributos observados ou ajuste. A validacao interna tambem nao estima a
# preparacao. O contexto inicial da recursao usa somente os ultimos 24 meses
# conhecidos ate o corte externo; nao reinicia com Real/2025 em janeiro/2026.
# Rede32/Adam0.01/100epocas/lote8/10%validacao preservados. A validacao interna
# usa um corte cronologico explicito, shuffle=FALSE; nao ha reajuste posterior.
# A nova politica de sementes por localidade torna a fila reprodutivel, mas
# os resultados antigos nao sao uma referencia numerica equivalente.
#
# Paralelismo: PSOCK/CPU, seis processos e uma localidade por tarefa. Cada
# processo inicializa seu proprio Python/TF, com uma thread nativa por operacao.
# Modelos/tensores/callbacks nunca sao enviados entre processos.
# ==============================================================================

# ---- 1. CONFIGURACAO CENTRAL DO PRODUTOR --------------------------------------
lstm_versao <- "LSTM-LOCAL-3-CAUSAL-VALIDACAO"

config_lstm_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_lstm_pre_comp.csv",
  historico_perdas = "data/historico_perdas_lstm.csv",
  historico_gradientes = "data/historico_gradientes_lstm.csv",
  salvar_historicos = TRUE, # FALSE na validacao temporal; preserva historicos de producao.
  script_pre_compilacao = "Previsoes_pre_compilacao/pre_compilar_LSTM.r",
  inicio_treino = as.Date("2016-01-01"), fim_treino = as.Date("2024-12-31"),
  meses_teste = 12L, passos_futuro = 72L,
  lookback = 24L, minimo_treino = 24L, fracao_validacao = 0.1,
  seed = 42L, localidades = NULL, paralelo = TRUE, processos = 6L,
  python = NULL, # Caminho opcional do Python local; nao instala/cria ambientes.
  parametros = list(unidades = 32L, taxa_aprendizado = 0.01,
    epocas = 100L, tamanho_lote = 8L, verbose = 0L)
)

lstm_inteiro <- function(x, minimo = 0L) {
  is.numeric(x) && length(x) == 1L && is.finite(x) &&
    x >= minimo && x <= .Machine$integer.max && x == floor(x)
}

lstm_periodos <- function(config) {
  inicio <- config$inicio_treino
  fim <- config$fim_treino
  meses <- config$meses_teste
  if (!inherits(inicio, "Date") || length(inicio) != 1L || is.na(inicio) ||
      !inherits(fim, "Date") || length(fim) != 1L || is.na(fim) || inicio > fim ||
      format(inicio, "%d") != "01" || !lstm_inteiro(meses, 1L)) {
    stop("Informe inicio/fim de treino como Date e meses_teste inteiro positivo.")
  }
  proximo <- seq(as.Date(format(fim, "%Y-%m-01")), by = "month", length.out = 2L)[2L]
  if (fim != proximo - 1L) stop("fim_treino deve ser o ultimo dia do mes.")
  limites <- seq(proximo, by = "month", length.out = as.integer(meses) + 1L)
  list(inicio_treino = inicio, fim_treino = fim,
    inicio_teste = limites[1L], fim_teste = tail(limites, 1L) - 1L,
    meses_teste = as.integer(meses), datas_teste = head(limites, -1L))
}

lstm_chave_localidade <- function(x) {
  x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC",
    toupper(trimws(as.character(x))))
  x <- trimws(gsub("[^A-Z0-9]+", " ", x))
  x[x %in% c("PARA", "PARA GERAL")] <- "PARA GERAL"
  x
}

lstm_localidades <- function(mensal, selecionadas = NULL) {
  disponiveis <- unique(as.character(mensal$Localidade))
  if (!length(disponiveis)) stop("Nenhuma localidade disponivel.")
  if (is.null(selecionadas)) return(disponiveis)
  if (!length(selecionadas) || anyNA(selecionadas)) stop("Selecione ao menos uma localidade.")
  idx <- match(lstm_chave_localidade(selecionadas), lstm_chave_localidade(disponiveis))
  if (anyNA(idx)) stop("Localidades nao encontradas: ", paste(selecionadas[is.na(idx)], collapse = ", "))
  unique(disponiveis[idx])
}

# ---- 2. LEITURA: UMA VEZ NO PAI, ANTES DO FILTRO DE LOCALIDADES ----------------
# Completar o calendario nao imputa quantidade: mes ausente permanece NA.
# Quantidade desconhecida em um grupo torna seu total desconhecido. O estado
# soma os registros observados, com cobertura informada, antes de completar.
ler_mensal_lstm <- function(config) {
  p <- lstm_periodos(config)
  bruto <- utils::read.csv(config$entrada, colClasses = "character", check.names = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", "NaN", "NULL"))
  campos <- c("LOCAL", "ANO", "MES", "TIPO_VEICULO", "QTD_VEICULO")
  if (!all(campos %in% names(bruto))) stop("CSV sem colunas: ", paste(setdiff(campos, names(bruto)), collapse = ", "))
  local <- trimws(bruto$LOCAL)
  tipo <- trimws(bruto$TIPO_VEICULO)
  ano <- suppressWarnings(as.numeric(bruto$ANO))
  mes <- suppressWarnings(as.numeric(bruto$MES))
  texto <- trimws(bruto$QTD_VEICULO)
  ausente <- is.na(texto) | toupper(texto) %in% c("", "NA", "NAN", "NULL")
  qtd <- suppressWarnings(as.numeric(texto)); qtd[ausente] <- NA_real_
  ruim <- is.na(local) | !nzchar(local) | is.na(tipo) | !nzchar(tipo) |
    !is.finite(ano) | ano != floor(ano) | ano < 1 | ano > 9999 |
    !is.finite(mes) | mes != floor(mes) | mes < 1 | mes > 12 |
    (!ausente & (!is.finite(qtd) | qtd < 0))
  if (any(ruim)) stop("Dados invalidos nas linhas: ", paste(head(which(ruim) + 1L, 10L), collapse = ", "))
  datas <- as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes)))
  manter <- datas >= p$inicio_treino & datas <= p$fim_teste &
    lstm_chave_localidade(local) != "PARA GERAL"
  if (!any(manter)) stop("Sem registros municipais no periodo configurado.")
  chave <- data.frame(Localidade = lstm_chave_localidade(local[manter]),
    Data = datas[manter], Tipo = tipo[manter])
  if (anyDuplicated(chave)) stop("CSV com LOCAL/ANO/MES/TIPO_VEICULO duplicados; confira a extracao.")
  base <- data.frame(Localidade = local[manter], Data = datas[manter], Real = qtd[manter])
  somar <- function(x) if (anyNA(x)) NA_real_ else sum(x)
  mensal <- stats::aggregate(base$Real, by = base[c("Localidade", "Data")], FUN = somar)
  names(mensal)[3L] <- "Real"
  calendario <- seq(p$inicio_treino, tail(p$datas_teste, 1L), by = "month")
  municipios <- sort(unique(mensal$Localidade))
  completar <- function(nome, parte, esperados = 1L, registros = rep(1L, nrow(parte)),
      conhecidos = as.integer(is.finite(parte$Real))) {
    idx <- match(calendario, parte$Data)
    nr <- registros[idx]; nr[is.na(idx)] <- 0L
    nc <- conhecidos[idx]; nc[is.na(idx)] <- 0L
    data.frame(Localidade = nome, Data = calendario, Real = parte$Real[idx],
      Municipios_com_registro = nr, Municipios_com_total_conhecido = nc,
      Municipios_esperados = esperados)
  }
  resultados <- lapply(municipios, function(nome) completar(nome, mensal[mensal$Localidade == nome, ]))
  estado <- stats::aggregate(mensal$Real, by = mensal["Data"], FUN = somar)
  names(estado)[2L] <- "Real"
  registros <- stats::aggregate(rep(1L, nrow(mensal)), by = mensal["Data"], FUN = sum)
  conhecidos <- stats::aggregate(as.integer(is.finite(mensal$Real)), by = mensal["Data"], FUN = sum)
  estado <- completar("PARÁ (Geral)", estado, length(municipios),
    registros$x[match(estado$Data, registros$Data)], conhecidos$x[match(estado$Data, conhecidos$Data)])
  do.call(rbind, c(list(estado), resultados))
}

lstm_validar_mensal <- function(mensal) {
  campos <- c("Localidade", "Data", "Real", "Municipios_com_registro",
    "Municipios_com_total_conhecido", "Municipios_esperados")
  if (!is.data.frame(mensal) || !all(campos %in% names(mensal))) stop("Series mensais sem calendario/cobertura do leitor LSTM.")
  mensal$Data <- as.Date(mensal$Data)
  mensal$Localidade <- as.character(mensal$Localidade)
  if (anyNA(mensal$Data) || any(format(mensal$Data, "%d") != "01") ||
      anyNA(mensal$Localidade) || any(!nzchar(trimws(mensal$Localidade))) ||
      !is.numeric(mensal$Real) || any(!is.na(mensal$Real) & (!is.finite(mensal$Real) | mensal$Real < 0)) ||
      anyDuplicated(data.frame(Localidade = lstm_chave_localidade(mensal$Localidade), Data = mensal$Data))) {
    stop("Series mensais invalidas ou duplicadas por localidade/data.")
  }
  mensal
}

lstm_serie_local <- function(mensal, localidade, periodos) {
  parte <- mensal[lstm_chave_localidade(mensal$Localidade) == lstm_chave_localidade(localidade), , drop = FALSE]
  datas <- seq(periodos$inicio_treino, tail(periodos$datas_teste, 1L), by = "month")
  idx <- match(datas, parte$Data)
  serie <- parte[idx, , drop = FALSE]
  serie$Data <- datas; serie$Localidade <- rep(localidade, length(datas))
  for (nome in c("Municipios_com_registro", "Municipios_com_total_conhecido")) serie[[nome]][is.na(idx)] <- 0L
  esperados <- unique(parte$Municipios_esperados[is.finite(parte$Municipios_esperados)])
  if (length(esperados) != 1L) stop("Cobertura esperada inconsistente para ", localidade)
  serie$Municipios_esperados[is.na(idx)] <- esperados
  rownames(serie) <- NULL
  serie
}

# ---- 3. BACKEND: INICIALIZACAO INDEPENDENTE EM CADA PROCESSO ------------------
# Descoberta compartilhada pelo produtor e controlador ANTES de criar PSOCK.
# Consultar py_config apenas se Python ja estiver ativo; nao inicializar o
# interpretador/TF no PAI para descobrir o caminho. O Python ativo prevalece
# sobre variaveis de ambiente que podem ter sido alteradas depois do inicio.
resolver_python_lstm <- function(config) {
  validar <- function(python) {
    if (!is.null(python) && (!is.character(python) || length(python) != 1L ||
        is.na(python) || !nzchar(trimws(python)))) {
      stop("python deve ser NULL ou caminho do interpretador local.")
    }
    python
  }
  python <- validar(config[["python"]])
  if (!is.null(python)) return(python)
  if (requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_available(initialize = FALSE)) {
    return(validar(reticulate::py_config()$python))
  }
  for (nome in c("RETICULATE_PYTHON", "KERAS_PYTHON")) {
    definido <- Sys.getenv(nome, unset = "")
    if (nzchar(definido)) return(validar(definido))
  }
  NULL
}

inicializar_backend_lstm <- function(config) {
  # Selecionar variaveis antes de carregar namespaces/importar Python. Keras
  # tambem consulta KERAS_PYTHON; ambos precisam indicar o mesmo interpretador.
  if (is.null(config$python)) {
    selecionado <- Sys.getenv("RETICULATE_PYTHON", unset = "")
    if (!nzchar(selecionado)) selecionado <- Sys.getenv("KERAS_PYTHON", unset = "")
    if (nzchar(selecionado)) config$python <- selecionado
  }
  Sys.setenv(KERAS_BACKEND = "tensorflow", CUDA_VISIBLE_DEVICES = "-1",
    TF_NUM_INTRAOP_THREADS = "1", TF_NUM_INTEROP_THREADS = "1",
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  if (!is.null(config$python)) Sys.setenv(RETICULATE_PYTHON = config$python, KERAS_PYTHON = config$python)
  for (p in c("reticulate", "tensorflow")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Pacote ausente no R local: ", p)
  }
  if (!is.null(config$python)) reticulate::use_python(config$python, required = TRUE)
  tf <- tensorflow::tf
  tryCatch({
    if (as.integer(tf$config$threading$get_intra_op_parallelism_threads()) != 1L) {
      tf$config$threading$set_intra_op_parallelism_threads(1L)
    }
    if (as.integer(tf$config$threading$get_inter_op_parallelism_threads()) != 1L) {
      tf$config$threading$set_inter_op_parallelism_threads(1L)
    }
    if (length(tf$config$get_visible_devices("GPU"))) tf$config$set_visible_devices(list(), "GPU")
  }, error = function(e) stop("Nao foi possivel configurar o TensorFlow/CPU: ",
    conditionMessage(e), ". Reinicie a sessao R ou utilize o modo paralelo em processos novos.", call. = FALSE))
  if (!requireNamespace("keras3", quietly = TRUE)) stop("Pacote ausente no R local: keras3")
  invisible(NULL)
}

# Hash limitado a inteiros positivos; independe da ordem/filtro da fila.
lstm_seed_localidade <- function(localidade, seed_base) {
  valor <- as.double(seed_base)
  for (codigo in utf8ToInt(enc2utf8(lstm_chave_localidade(localidade)))) valor <- (valor * 131 + codigo) %% 2147483647
  as.integer(valor)
}

inicializar_semente_lstm <- function(localidade, config) {
  seed <- lstm_seed_localidade(localidade, config$seed)
  set.seed(seed)
  keras3::set_random_seed(seed)
  seed
}

# ---- 4. CONSOLIDACAO E GRAVACAO: SOMENTE NO PAI -------------------------------
lstm_perdas_vazias <- function() data.frame(Modelo = character(), Localidade = character(),
  Epoca = integer(), Loss = numeric(), Val_Loss = numeric())
lstm_gradientes_vazios <- function() data.frame(Localidade = character(), Epoca = integer(),
  Loss_Gradiente = numeric(), Norma_Gradiente = numeric(), Max_Gradiente = numeric(),
  Gradiente_NaN = logical(), Gradiente_Inf = logical())

gravar_csv_lstm <- function(dados, saida) {
  dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile("lstm_novo_", tmpdir = dirname(saida), fileext = ".csv")
  anterior <- NULL
  on.exit({
    unlink(temporario)
    if (!is.null(anterior) && file.exists(anterior) && !file.exists(saida)) file.rename(anterior, saida)
  }, add = TRUE)
  utils::write.csv(dados, temporario, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  if (file.exists(saida)) {
    anterior <- tempfile("lstm_anterior_", tmpdir = dirname(saida), fileext = ".csv")
    if (!file.rename(saida, anterior)) stop("Nao foi possivel preservar a saida anterior: ", saida)
  }
  if (!file.rename(temporario, saida)) stop("Nao foi possivel concluir a gravacao: ", saida)
  if (!is.null(anterior)) unlink(anterior)
  invisible(saida)
}

gravar_resultados_lstm <- function(previsoes, perdas, gradientes, config) {
  gravar_csv_lstm(previsoes, config$saida)
  if (isTRUE(config$salvar_historicos)) {
    gravar_csv_lstm(perdas, config$historico_perdas)
    gravar_csv_lstm(gradientes, config$historico_gradientes)
  }
  invisible(NULL)
}

# ---- 5. TAREFA: PREPARAR, AJUSTAR E PREVER UMA LOCALIDADE ----------------------
lstm_log_progresso <- function(localidade, periodos, ...) {
  texto <- trimws(gsub("[\r\n]+", " | ", paste0(..., collapse = "")))
  cat(paste0("[PID ", Sys.getpid(), " | teste ", format(periodos$inicio_teste, "%m/%Y"),
    " a ", format(periodos$fim_teste, "%m/%Y"), "] ", localidade, ": ", texto, "\n"))
  flush(stdout())
  invisible(NULL)
}

prever_localidade_lstm <- function(mensal, localidade, periodos, futuro, config) {
  inicio <- proc.time()[["elapsed"]]
  opcoes <- options(OutDec = ".")
  on.exit(options(opcoes), add = TRUE)
  on.exit({
    try(keras3::clear_session(), silent = TRUE)
    invisible(gc(verbose = FALSE))
  }, add = TRUE)
  erro <- NULL; avisos <- character(); seed <- NA_integer_
  previsoes <- NULL; perdas <- lstm_perdas_vazias(); gradientes <- lstm_gradientes_vazios()
  lstm_log_progresso(localidade, periodos, "iniciado")
  tryCatch(withCallingHandlers({
    keras3::clear_session()
    seed <- inicializar_semente_lstm(localidade, config)
    serie <- lstm_serie_local(mensal, localidade, periodos)
    preparado <- preparar_treino_lstm(serie, periodos, config)
    lstm_log_progresso(localidade, periodos, dim(preparado$x_treino)[1L],
      " amostras de treino; ", dim(preparado$x_validacao)[1L], " de validacao interna; semente ", seed)
    ajuste <- ajustar_lstm(preparado, config)
    # Guardar os historicos antes da recursao. Se a previsao falhar depois do
    # fit, os diagnosticos disponiveis continuam aproveitados, como no original.
    perdas <- ajuste$perdas
    perdas$Modelo <- rep("LSTM", nrow(perdas)); perdas$Localidade <- rep(localidade, nrow(perdas))
    perdas <- perdas[c("Modelo", "Localidade", "Epoca", "Loss", "Val_Loss")]
    gradientes <- ajuste$gradientes
    gradientes$Localidade <- rep(localidade, nrow(gradientes))
    gradientes <- gradientes[c("Localidade", "Epoca", "Loss_Gradiente", "Norma_Gradiente", "Max_Gradiente", "Gradiente_NaN", "Gradiente_Inf")]
    datas <- c(periodos$datas_teste, futuro)
    pred <- prever_recursivo_lstm(ajuste$modelo, preparado, datas, config)
    if (length(pred) != length(datas) || any(!is.finite(pred))) stop("Previsoes nao finitas ou fora do horizonte.")
    if (any(pred < 0)) message(sum(pred < 0), " previsoes negativas; preservadas como no produtor anterior.")
    previstos <- rep(NA_real_, nrow(serie))
    previstos[match(periodos$datas_teste, serie$Data)] <- head(pred, periodos$meses_teste)
    previsoes <- data.frame(Localidade = localidade, Data = c(serie$Data, futuro),
      Real = c(serie$Real, rep(NA_real_, length(futuro))),
      Previsao_LSTM = c(previstos, tail(pred, length(futuro))),
      AIC = NA_real_, BIC = NA_real_, Modelo = "LSTM")
  }, message = function(c) {
    avisos <<- c(avisos, conditionMessage(c)); lstm_log_progresso(localidade, periodos, conditionMessage(c))
    invokeRestart("muffleMessage")
  }, warning = function(c) {
    avisos <<- c(avisos, paste0("Aviso: ", conditionMessage(c)))
    lstm_log_progresso(localidade, periodos, "Aviso: ", conditionMessage(c)); invokeRestart("muffleWarning")
  }), error = function(e) { erro <<- conditionMessage(e); NULL })
  segundos <- unname(proc.time()[["elapsed"]] - inicio)
  lstm_log_progresso(localidade, periodos, if (is.null(erro)) "concluido" else paste0("ignorado; ", erro),
    "; ", round(segundos, 2L), " s")
  list(previsoes = previsoes, perdas = perdas, gradientes = gradientes,
    erro = erro, avisos = avisos, localidade = localidade, segundos = segundos, seed = seed)
}

# ---- 6. CLUSTER: CRIACAO/INICIALIZACAO UMA VEZ, SEM SERIALIZAR PYTHON -----------
criar_cluster_lstm <- function(processos, script_pre_compilacao, raiz = getwd(), python = NULL) {
  if (!lstm_inteiro(processos, 1L)) stop("processos deve ser inteiro positivo.")
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  script <- normalizePath(script_pre_compilacao, winslash = "/", mustWork = TRUE)
  nomes <- c("KERAS_BACKEND", "CUDA_VISIBLE_DEVICES", "TF_NUM_INTRAOP_THREADS", "TF_NUM_INTEROP_THREADS",
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS")
  anteriores <- Sys.getenv(nomes, unset = NA_character_)
  on.exit({
    ausentes <- is.na(anteriores)
    if (any(ausentes)) Sys.unsetenv(nomes[ausentes])
    if (any(!ausentes)) do.call(Sys.setenv, as.list(anteriores[!ausentes]))
  }, add = TRUE)
  novos <- stats::setNames(rep("1", length(nomes)), nomes)
  novos[["KERAS_BACKEND"]] <- "tensorflow"; novos[["CUDA_VISIBLE_DEVICES"]] <- "-1"
  do.call(Sys.setenv, as.list(novos))
  cluster <- parallel::makePSOCKcluster(as.integer(processos), outfile = "")
  pronto <- FALSE
  on.exit(if (!pronto) try(parallel::stopCluster(cluster), silent = TRUE), add = TRUE)
  inicializar <- function(raiz, script, python, bibliotecas) {
    setwd(raiz)
    .libPaths(bibliotecas)
    opcoes <- options(frota.lstm.apenas_funcoes = TRUE, OutDec = ".")
    on.exit(options(opcoes), add = TRUE)
    produtor <- new.env(parent = globalenv())
    source(script, local = produtor, encoding = "UTF-8")
    produtor$inicializar_backend_lstm(list(python = python))
    assign(".frota_lstm_produtor", produtor, envir = globalenv())
    invisible(NULL)
  }
  environment(inicializar) <- baseenv()
  invisible(parallel::clusterCall(cluster, inicializar, raiz, script, python, .libPaths()))
  pronto <- TRUE
  cluster
}

# ---- 7. PAI: ORQUESTRAR RODADA E CONSOLIDAR OS TRES RESULTADOS -----------------
# Cluster recebido e emprestado. Cluster criado aqui e encerrado aqui.
executar_lstm_local <- function(config = config_lstm_local, dados_mensais = NULL, cluster = NULL) {
  inicio <- proc.time()[["elapsed"]]
  proprio <- NULL
  opcoes <- options(OutDec = ".")
  nomes_ambiente <- c("KERAS_BACKEND", "KERAS_PYTHON", "RETICULATE_PYTHON", "CUDA_VISIBLE_DEVICES",
    "TF_NUM_INTRAOP_THREADS", "TF_NUM_INTEROP_THREADS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS")
  ambiente_anterior <- Sys.getenv(nomes_ambiente, unset = NA_character_)
  on.exit({
    if (!is.null(proprio)) tryCatch(parallel::stopCluster(proprio), error = function(e) message(conditionMessage(e)))
    options(opcoes)
    ausentes <- is.na(ambiente_anterior)
    if (any(ausentes)) Sys.unsetenv(nomes_ambiente[ausentes])
    if (any(!ausentes)) do.call(Sys.setenv, as.list(ambiente_anterior[!ausentes]))
    segundos <- proc.time()[["elapsed"]] - inicio
    n <- floor(segundos)
    message("TEMPO TOTAL DO PRE-COMPILADOR LSTM: ", sprintf("%02.0f:%02.0f:%02.0f", n %/% 3600, (n %% 3600) %/% 60, n %% 60),
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  config <- utils::modifyList(config_lstm_local, config, keep.null = TRUE)
  if (!is.logical(config$salvar_historicos) || length(config$salvar_historicos) != 1L ||
      is.na(config$salvar_historicos)) stop("salvar_historicos deve ser TRUE ou FALSE.")
  p <- lstm_periodos(config)
  if (!lstm_inteiro(config$passos_futuro, 0L) || !lstm_inteiro(config$lookback, 1L) ||
      !lstm_inteiro(config$minimo_treino, 2L) || !lstm_inteiro(config$seed, 0L) ||
      !lstm_inteiro(config$processos, 1L) || !is.logical(config$paralelo) ||
      length(config$paralelo) != 1L || is.na(config$paralelo) ||
      !is.numeric(config$fracao_validacao) || length(config$fracao_validacao) != 1L ||
      !is.finite(config$fracao_validacao) || config$fracao_validacao <= 0 || config$fracao_validacao >= 1) {
    stop("Configuracao invalida: confira horizontes, lookback, minimo, semente, processos e fracao_validacao.")
  }
  config$python <- resolver_python_lstm(config)
  parametros <- config$parametros
  if (!is.list(parametros) || !lstm_inteiro(parametros$unidades, 1L) ||
      !lstm_inteiro(parametros$epocas, 1L) || !lstm_inteiro(parametros$tamanho_lote, 1L) ||
      !lstm_inteiro(parametros$verbose, 0L) || parametros$verbose > 2L ||
      !is.numeric(parametros$taxa_aprendizado) || length(parametros$taxa_aprendizado) != 1L ||
      !is.finite(parametros$taxa_aprendizado) || parametros$taxa_aprendizado <= 0) stop("Hiperparametros da rede invalidos.")
  for (campo in c("entrada", "saida", "historico_perdas", "historico_gradientes", "script_pre_compilacao")) {
    valor <- config[[campo]]
    if (!is.character(valor) || length(valor) != 1L || is.na(valor) || !nzchar(valor)) stop("Caminho invalido: ", campo)
  }
  canonico <- function(x) tolower(normalizePath(x, winslash = "/", mustWork = FALSE))
  destinos <- vapply(config[c("saida", "historico_perdas", "historico_gradientes")], canonico, character(1L))
  if (anyDuplicated(destinos) || canonico(config$entrada) %in% destinos) stop("Entrada e tres saidas devem ter caminhos distintos.")
  if (!is.null(cluster) && !inherits(cluster, "cluster")) stop("cluster deve pertencer ao pacote parallel.")
  mensal <- lstm_validar_mensal(if (is.null(dados_mensais)) ler_mensal_lstm(config) else dados_mensais)
  mensal <- mensal[mensal$Data >= p$inicio_treino & mensal$Data <= p$fim_teste, , drop = FALSE]
  localidades <- lstm_localidades(mensal, config$localidades)
  futuro <- seq(tail(p$datas_teste, 1L), by = "month", length.out = as.integer(config$passos_futuro) + 1L)[-1L]
  processos <- if (config$paralelo) min(as.integer(config$processos), length(localidades)) else 1L
  if (is.null(cluster) && processos > 1L) {
    proprio <- criar_cluster_lstm(processos, config$script_pre_compilacao, getwd(), config$python)
    cluster <- proprio
  }
  message("LSTM: treino ", p$inicio_treino, " a ", p$fim_treino, "; teste ", p$inicio_teste,
    " a ", p$fim_teste, "; ", config$passos_futuro, " meses futuros; ", length(localidades), " localidades.")
  if (is.null(cluster)) {
    inicializar_backend_lstm(config)
    calculados <- lapply(localidades, function(localidade) prever_localidade_lstm(mensal, localidade, p, futuro, config))
  } else {
    # ---- INICIO DO BLOCO PARALELO: PAI ENVIA CONTEXTO, WORKERS CALCULAM --------
    contexto <- list(mensal = mensal, periodos = p, futuro = futuro, config = config)
    preparar <- function(contexto) { assign(".frota_lstm_contexto", contexto, envir = globalenv()); invisible(NULL) }
    trabalhar <- function(localidade) {
      produtor <- get(".frota_lstm_produtor", envir = globalenv(), inherits = FALSE)
      contexto <- get(".frota_lstm_contexto", envir = globalenv(), inherits = FALSE)
      produtor$prever_localidade_lstm(contexto$mensal, localidade, contexto$periodos, contexto$futuro, contexto$config)
    }
    environment(preparar) <- environment(trabalhar) <- baseenv()
    invisible(parallel::clusterCall(cluster, preparar, contexto))
    message("LSTM: ", length(cluster), " processos; fila dinamica, uma localidade por tarefa.")
    calculados <- parallel::parLapplyLB(cluster, localidades, trabalhar, chunk.size = 1L)
    # ---- FIM DO BLOCO PARALELO: SOMENTE OBJETOS R RETORNAM AO PAI -------------
  }
  resultados <- lapply(Filter(function(r) is.null(r$erro) && !is.null(r$previsoes), calculados), function(r) r$previsoes)
  if (!length(resultados)) stop("Nenhuma localidade processada; os tres CSVs anteriores foram preservados.")
  previsoes <- do.call(rbind, resultados); rownames(previsoes) <- NULL
  perdas <- do.call(rbind, lapply(calculados, function(r) r$perdas)); rownames(perdas) <- NULL
  gradientes <- do.call(rbind, lapply(calculados, function(r) r$gradientes)); rownames(gradientes) <- NULL
  if (anyDuplicated(previsoes[c("Localidade", "Data")])) stop("Previsoes duplicadas por localidade/data.")
  gravar_resultados_lstm(previsoes, perdas, gradientes, config)
  message("LSTM_LOCAL_CONCLUIDO: ", length(resultados), "/", length(localidades),
    " localidades; previsoes: ", config$saida,
    if (config$salvar_historicos) paste0("; perdas: ", config$historico_perdas,
      "; gradientes: ", config$historico_gradientes) else
      "; historicos disponiveis somente no retorno desta execucao", ".")
  invisible(list(previsoes = previsoes, perdas = perdas, gradientes = gradientes, diagnosticos = calculados))
}

# ---- MODELAGEM 1. RECORTE CAUSAL, VALIDACAO INTERNA E TENSORES -----------------
# Somente este helper estima tendencia/normalizacao. Ele recebe a serie mensal
# completa para conservar Real na saida, mas seleciona o treino ANTES de ler
# quantidades, estimar coeficientes ou construir as janelas da rede.
preparar_treino_lstm <- function(serie, periodos, config) {
  historico <- serie[serie$Data >= periodos$inicio_treino &
    serie$Data <= periodos$fim_treino, c("Data", "Real"), drop = FALSE]
  historico <- historico[order(historico$Data), , drop = FALSE]
  calendario <- seq(periodos$inicio_treino,
    as.Date(format(periodos$fim_treino, "%Y-%m-01")), by = "month")
  if (!identical(as.numeric(historico$Data), as.numeric(calendario))) {
    stop("Treino precisa de calendario mensal completo; meses ausentes devem permanecer NA.")
  }
  valores <- historico$Real
  n <- length(valores)
  l <- as.integer(config$lookback)
  if (n <= l) stop("Treino menor ou igual ao lookback.")
  if (any(!is.finite(tail(valores, l)))) {
    stop("Ultimos ", l, " meses do treino precisam ser conhecidos para iniciar a recursao; sem imputacao.")
  }

  # Elegibilidade usa apenas o calendario e valores observados do treino.
  # Nunca eliminar meses e aproximar uma janela de 24 linhas de 24 meses.
  candidatos <- seq.int(l + 1L, n)
  elegiveis <- candidatos[vapply(candidatos, function(i) {
    all(is.finite(valores[seq.int(i - l, i)]))
  }, logical(1L))]
  fracao <- config$fracao_validacao
  if (!is.numeric(fracao) || length(fracao) != 1L || !is.finite(fracao) ||
      fracao <= 0 || fracao >= 1) stop("fracao_validacao deve estar entre zero e um.")
  n_treino <- as.integer(floor(length(elegiveis) * (1 - fracao)))
  n_validacao <- length(elegiveis) - n_treino
  if (n_treino < config$minimo_treino || n_treino < 2L || n_validacao < 1L) {
    stop("Janelas insuficientes: ", n_treino, " de treino e ", n_validacao,
      " de validacao interna; minimo de treino ", config$minimo_treino, ".")
  }
  amostras_treino <- seq_len(n_treino)
  amostras_validacao <- seq.int(n_treino + 1L, length(elegiveis))
  indices_treino <- elegiveis[amostras_treino]
  indices_validacao <- elegiveis[amostras_validacao]

  # A separacao interna tambem precede as estimativas. O corte e o ultimo
  # ALVO usado no ajuste dos pesos. Os ultimos 10% permanecem como validacao
  # cronologica, com tendencia/escala estimadas somente no bloco anterior.
  # Mantem-se um unico ajuste da rede; nao ha refit posterior com a validacao.
  fim_ajuste <- tail(indices_treino, 1L)
  t_index <- seq_len(n)
  conhecidos_ajuste <- which(t_index <= fim_ajuste & is.finite(valores))
  if (length(conhecidos_ajuste) < 3L) stop("Historico insuficiente para tendencia quadratica.")
  dados_tendencia <- data.frame(valor = valores[conhecidos_ajuste],
    t_index = t_index[conhecidos_ajuste])
  modelo_tendencia <- stats::lm(valor ~ t_index + I(t_index^2), data = dados_tendencia)
  if (modelo_tendencia$rank < 3L || any(!is.finite(stats::coef(modelo_tendencia)))) {
    stop("Tendencia quadratica sem coeficientes finitos e identificados.")
  }
  tendencia <- as.numeric(stats::predict(modelo_tendencia,
    newdata = data.frame(t_index = t_index)))
  if (any(!is.finite(tendencia))) stop("Tendencia historica nao finita.")
  residuos <- valores - tendencia
  residuos_ajuste <- residuos[conhecidos_ajuste]
  media_res <- mean(residuos_ajuste)
  desvio_res <- stats::sd(residuos_ajuste)
  # Uma serie ajustada exatamente pela tendencia pode ter residuo constante
  # ou apenas arredondamento numerico. Escala 1 evita dividir por zero; nao
  # acrescenta crescimento, ruido nem informacao de outros periodos.
  escala_constante <- !is.finite(desvio_res) ||
    desvio_res <= sqrt(.Machine$double.eps) * max(1, max(abs(residuos_ajuste)))
  if (escala_constante) desvio_res <- 1
  residual_norm <- (residuos - media_res) / desvio_res

  meses <- as.integer(format(historico$Data, "%m"))
  meses_onehot <- diag(12L)[meses, , drop = FALSE]
  x <- array(NA_real_, dim = c(length(elegiveis), l, 13L))
  y <- residual_norm[elegiveis]
  for (k in seq_along(elegiveis)) {
    janela <- seq.int(elegiveis[k] - l, elegiveis[k] - 1L)
    # Preenchimento explicito [amostra,tempo,atributo]. unlist() de uma
    # lista de matrizes seguido de array() intercalaria essas dimensoes.
    x[k, , ] <- cbind(residual_norm[janela], meses_onehot[janela, , drop = FALSE])
  }
  if (any(!is.finite(x)) || any(!is.finite(y))) stop("Tensor de treino/validacao nao finito.")
  list(
    x_treino = x[amostras_treino, , , drop = FALSE],
    y_treino = y[amostras_treino],
    x_validacao = x[amostras_validacao, , , drop = FALSE],
    y_validacao = y[amostras_validacao],
    preparacao = list(modelo_tendencia = modelo_tendencia, media_res = media_res,
      desvio_res = desvio_res, inicio = calendario[1L],
      fim_ajuste = calendario[fim_ajuste], tamanho_historico = n),
    historico_residual_norm = residual_norm,
    datas_historico = historico$Data,
    indices_treino = indices_treino,
    indices_validacao = indices_validacao,
    metadados = list(meses_historico = n, amostras = length(elegiveis),
      amostras_treino = n_treino, amostras_validacao = n_validacao,
      fim_ajuste_preparacao = calendario[fim_ajuste],
      inicio_validacao = calendario[indices_validacao[1L]],
      fim_validacao = calendario[tail(indices_validacao, 1L)],
      escala_constante = escala_constante))
}

# ---- MODELAGEM 2. DIAGNOSTICO DOS GRADIENTES, SOMENTE NO BLOCO DE TREINO --------
# Mesmo diagnostico do produtor publicado: gradientes no estado atual, sem
# apply_gradients(). Os tensores e o callback nunca saem do processo da rede.
medir_gradientes_lstm <- function(modelo, x_treino, y_treino) {
  tf <- tensorflow::tf
  `%as%` <- reticulate::`%as%`
  x_tf <- tf$convert_to_tensor(x_treino, dtype = tf$float32)
  y_tf <- tf$convert_to_tensor(y_treino, dtype = tf$float32)
  y_tf <- tf$reshape(y_tf, shape = c(-1L, 1L))
  with(tf$GradientTape() %as% fita, {
    previsoes <- modelo(x_tf, training = TRUE)
    perda <- tf$reduce_mean(tf$square(y_tf - previsoes))
  })
  gradientes <- fita$gradient(perda, modelo$trainable_weights)
  gradientes <- gradientes[!vapply(gradientes, is.null, logical(1L))]
  if (!length(gradientes)) return(data.frame(Loss_Gradiente = NA_real_,
    Norma_Gradiente = NA_real_, Max_Gradiente = NA_real_,
    Gradiente_NaN = NA, Gradiente_Inf = NA))
  soma_quadrados <- vapply(gradientes, function(g) {
    as.numeric(tf$reduce_sum(tf$square(g))$numpy())
  }, numeric(1L))
  maiores <- vapply(gradientes, function(g) {
    as.numeric(tf$reduce_max(tf$abs(g))$numpy())
  }, numeric(1L))
  possui_nan <- any(vapply(gradientes, function(g) {
    isTRUE(as.logical(tf$reduce_any(tf$math$is_nan(g))$numpy()))
  }, logical(1L)))
  possui_inf <- any(vapply(gradientes, function(g) {
    isTRUE(as.logical(tf$reduce_any(tf$math$is_inf(g))$numpy()))
  }, logical(1L)))
  data.frame(Loss_Gradiente = as.numeric(perda$numpy()),
    Norma_Gradiente = sqrt(sum(soma_quadrados)), Max_Gradiente = max(maiores),
    Gradiente_NaN = possui_nan, Gradiente_Inf = possui_inf)
}

criar_callback_gradientes_lstm <- function(modelo, x_treino, y_treino) {
  estado <- new.env(parent = emptyenv())
  estado$dados <- data.frame(Epoca = integer(), Loss_Gradiente = numeric(),
    Norma_Gradiente = numeric(), Max_Gradiente = numeric(),
    Gradiente_NaN = logical(), Gradiente_Inf = logical())
  callback <- keras3::callback_lambda(on_epoch_end = function(epoch, logs = NULL) {
    diagnostico <- tryCatch(medir_gradientes_lstm(modelo, x_treino, y_treino),
      error = function(e) {
        warning("Diagnostico de gradientes indisponivel na epoca ",
          as.integer(epoch), ": ", conditionMessage(e), call. = FALSE)
        data.frame(Loss_Gradiente = NA_real_, Norma_Gradiente = NA_real_,
          Max_Gradiente = NA_real_, Gradiente_NaN = NA, Gradiente_Inf = NA)
      })
    # callback_lambda do keras3 ja entrega indices R a partir de 1.
    # Acrescentar 1 novamente deslocaria os gradientes para as epocas 2:101.
    diagnostico$Epoca <- as.integer(epoch)
    diagnostico <- diagnostico[c("Epoca", "Loss_Gradiente", "Norma_Gradiente",
      "Max_Gradiente", "Gradiente_NaN", "Gradiente_Inf")]
    estado$dados <- rbind(estado$dados, diagnostico)
  })
  list(callback = callback, estado = estado)
}

# ---- MODELAGEM 3. AJUSTE UNICO, PERDAS E GRADIENTES POR EPOCA ------------------
ajustar_lstm <- function(preparado, config) {
  p <- config$parametros
  modelo <- keras3::keras_model_sequential(input_shape = c(as.integer(config$lookback), 13L))
  modelo <- keras3::layer_lstm(modelo, units = as.integer(p$unidades))
  modelo <- keras3::layer_dense(modelo, units = 1L)
  keras3::compile(modelo, loss = "mse",
    optimizer = keras3::optimizer_adam(learning_rate = p$taxa_aprendizado))
  # As mesmas amostras usadas pelo optimizer sao usadas no diagnostico;
  # nao calcular outro arredondamento do validation_split para o callback.
  monitor <- criar_callback_gradientes_lstm(modelo,
    preparado$x_treino, preparado$y_treino)
  history <- keras3::fit(modelo, x = preparado$x_treino,
    y = matrix(preparado$y_treino, ncol = 1L),
    epochs = as.integer(p$epocas), batch_size = as.integer(p$tamanho_lote),
    validation_data = list(preparado$x_validacao,
      matrix(preparado$y_validacao, ncol = 1L)),
    callbacks = list(monitor$callback), shuffle = FALSE,
    verbose = as.integer(p$verbose))
  metricas <- history$metrics
  loss <- as.numeric(metricas[["loss"]])
  val_loss <- if ("val_loss" %in% names(metricas)) as.numeric(metricas[["val_loss"]]) else
    rep(NA_real_, length(loss))
  if (!length(loss) || length(loss) != length(val_loss)) stop("Historico de perdas inconsistente.")
  perdas <- data.frame(Epoca = seq_along(loss), Loss = loss, Val_Loss = val_loss)
  gradientes <- monitor$estado$dados
  if (anyDuplicated(gradientes$Epoca)) stop("Historico de gradientes com epocas duplicadas.")
  list(modelo = modelo, perdas = perdas, gradientes = gradientes)
}

# ---- MODELAGEM 4. RECURSAO CONTINUA, SEM REALIMENTACAO COM O TESTE -------------
# predict_on_batch evita criar uma pipeline predict() para cada mes. Uma unica
# amostra e inferida em cada passo, com a mesma semantica de previsao da rede.
prever_passo_lstm <- function(modelo, x) {
  as.numeric(keras3::predict_on_batch(modelo, x = x))
}

prever_recursivo_lstm <- function(modelo, preparado, datas, config) {
  if (!inherits(datas, "Date") || anyNA(datas)) stop("Datas de previsao invalidas.")
  if (!length(datas)) return(numeric())
  historico <- preparado$historico_residual_norm
  datas_historico <- preparado$datas_historico
  l <- as.integer(config$lookback)
  esperadas <- seq(tail(datas_historico, 1L), by = "month",
    length.out = length(datas) + 1L)[-1L]
  if (!identical(as.numeric(datas), as.numeric(esperadas))) {
    stop("A recursao deve comecar no mes seguinte ao treino e seguir sem interrupcoes.")
  }
  if (length(historico) < l || any(!is.finite(tail(historico, l)))) {
    stop("Contexto final do treinamento incompleto; sem imputacao.")
  }
  meses_historico <- as.integer(format(tail(datas_historico, l), "%m"))
  sequencia <- cbind(tail(historico, l), diag(12L)[meses_historico, , drop = FALSE])
  prep <- preparado$preparacao
  tendencia <- as.numeric(stats::predict(prep$modelo_tendencia,
    newdata = data.frame(t_index = length(historico) + seq_along(datas))))
  if (any(!is.finite(tendencia))) stop("Tendencia extrapolada nao finita.")
  previstos <- numeric(length(datas))
  for (i in seq_along(datas)) {
    x <- array(sequencia, dim = c(1L, l, 13L))
    pred <- prever_passo_lstm(modelo, x)
    if (length(pred) != 1L || !is.finite(pred)) stop("Saida nao finita da rede no horizonte ", i, ".")
    previstos[i] <- tendencia[i] + pred * prep$desvio_res + prep$media_res
    # Realimentar somente o residuo normalizado PREVISTO e o mes conhecido.
    # Real de 2025 nao e parametro desta funcao. O passo de janeiro/2026
    # continua a mesma sequencia, sem reiniciar com observacoes daquele teste.
    mes <- as.integer(format(datas[i], "%m"))
    nova_linha <- c(pred, diag(12L)[mes, ])
    sequencia <- rbind(sequencia[-1L, , drop = FALSE], nova_linha)
  }
  if (any(!is.finite(previstos))) stop("Previsao reconstruida nao finita.")
  # Preservar a escala original sem log, truncamento ou crescimento imposto.
  previstos
}

# ---- ENTRADA: SOURCE LOCAL OU IMPORTACAO SOMENTE DAS FUNCOES -------------------
if (!isTRUE(getOption("frota.lstm.apenas_funcoes", FALSE))) executar_lstm_local()
