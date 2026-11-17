# Meta Prophet: registros mensais por municipio e PARA (Geral).
# Revisao: PROPHET-CRAN-LOCAL-ESTADO-3-VALIDACAO-PARALELA; CSV mensal, erros em runtime.
# Base consultada: GitHub publicacao-codigo, 8686bc2679b2d0ec2d500f01eb54e74f1dcdd8ab.
# Executar EXCLUSIVAMENTE no R local, na raiz do projeto Windows/VS Code:
# source("Previsoes_pre_compilacao/pre_compilar_prophet.r", encoding = "UTF-8")
# Ou: Rscript --vanilla Previsoes_pre_compilacao/pre_compilar_prophet.r
# Dependencia local: install.packages("prophet", repos = "http://cran.rstudio.com/")
# Nao instala pacotes, nao carrega global.R e nao altera os modulos Shiny.
#
# PIPELINE: configurar -> validar -> agregar/calendario -> separar treino
# -> verificar cobertura -> ds/y -> ajustar -> prever datas -> consolidar -> CSV.
# Alvo: quantidade de REGISTROS no mes, somando os tipos presentes; nao estoque.
# Ajusta y em nivel. Nao ha log, diferenciacao, lags ou capacidade maxima imposta.
# Um modelo independente para cada municipio e outro para a soma estadual observada.
# Um ajuste ate 2024 atende teste/2025 e futuro/2026-2031; nao usa Real/2025 no ajuste.
# Prophet preve as datas diretamente: nao ha realimentacao recursiva de previsoes.
# Na validacao, o controlador substitui inicio/fim do treino, meses de teste e
# destino; reutiliza seis processos PSOCK por localidade entre as janelas.
# So o PAI grava CSV. O cluster emprestado e encerrado pelo controlador.
#
# Regra operacional: minimo 24 meses conhecidos, abrangendo os 12 meses do ano.
# Isso nao e requisito do pacote nem garantia estatistica de dados suficientes.
# Preservar os parametros do checkpoint; esta revisao nao otimiza o modelo.
# Fontes: https://github.com/meta/prophet/blob/main/docs/_docs/non-daily_data.md
# https://github.com/meta/prophet/blob/main/docs/_docs/trend_changepoints.md

# =============================================================================
# ETAPA 01 — CONFIGURACAO E DEPENDENCIA LOCAL
# Entrada: caminhos relativos a raiz, periodo e parametros explicitos.
# Saida: configuracao de um ajuste por localidade, sem otimizacao automatica.
# Regra: previsoes e testes que ajustam/preveem pertencem a maquina do usuario.
# =============================================================================
config_prophet_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_prophet_pre_comp.csv",
  ano_inicio = 2016L, ano_teste = 2025L, ano_fim = 2025L,
  n_teste = 12L, passos_futuro = 72L, minimo_treino = 24L, seed = 42L,
  parametros = list(
    growth = "linear", yearly.seasonality = 6,
    weekly.seasonality = FALSE, daily.seasonality = FALSE,
    seasonality.mode = "multiplicative", seasonality.prior.scale = 10,
    changepoint.prior.scale = 0.5, changepoint.range = 0.9,
    n.changepoints = 25L, mcmc.samples = 0L, uncertainty.samples = 0L
  )
)

# =============================================================================
# ETAPA 02 — LEITURA E VALIDACAO DO CSV
# Entrada: LOCAL, ANO, MES, TIPO_VEICULO e QTD_VEICULO.
# Operacao: validar identificadores/datas/quantidades e rejeitar chaves duplicadas.
# Regra: zero explicito e valido; quantidade desconhecida continua NA.
# =============================================================================
ler_mensal_prophet <- function(config) {
  if (!file.exists(config$entrada)) {
    stop("Entrada nao encontrada: ", config$entrada, ". Execute na raiz do projeto.")
  }
  dados <- utils::read.csv(config$entrada, colClasses = "character",
    check.names = FALSE, fileEncoding = "UTF-8-BOM",
    na.strings = c("", "NA", "NaN", "NULL"))
  obrigatorias <- c("LOCAL", "ANO", "MES", "TIPO_VEICULO", "QTD_VEICULO")
  faltantes <- setdiff(obrigatorias, names(dados))
  if (length(faltantes)) stop("Colunas ausentes: ", paste(faltantes, collapse = ", "))
  local <- trimws(dados$LOCAL); tipo <- trimws(dados$TIPO_VEICULO)
  ano <- suppressWarnings(as.numeric(dados$ANO))
  mes <- suppressWarnings(as.numeric(dados$MES))
  texto_qtd <- trimws(dados$QTD_VEICULO)
  ausente <- is.na(texto_qtd) | toupper(texto_qtd) %in% c("", "NA", "NAN", "NULL")
  qtd <- suppressWarnings(as.numeric(texto_qtd)); qtd[ausente] <- NA_real_
  invalido <- is.na(local) | !nzchar(local) | is.na(tipo) | !nzchar(tipo) |
    !is.finite(ano) | ano != floor(ano) | ano < 1 | ano > 9999 |
    !is.finite(mes) | mes != floor(mes) | mes < 1 | mes > 12 |
    (!ausente & (!is.finite(qtd) | qtd < 0 | qtd != floor(qtd)))
  if (any(invalido)) {
    stop("Dados invalidos nas linhas do CSV: ",
      paste(head(which(invalido) + 1L, 10L), collapse = ", "))
  }
  chave_estado <- chartr("ÁÀÂÃÉÊÍÓÔÕÚÇ", "AAAAEEIOOOUC", toupper(local))
  manter <- ano >= config$ano_inicio & ano <= config$ano_fim &
    !chave_estado %in% c("PARA", "PARA (GERAL)")
  if (!any(manter)) stop("Sem dados municipais no periodo configurado.")
  chaves <- data.frame(LOCAL = local[manter], ANO = ano[manter], MES = mes[manter],
    TIPO_VEICULO = tipo[manter])
  if (anyDuplicated(chaves)) {
    stop("Chave duplicada LOCAL/ANO/MES/TIPO_VEICULO; investigar antes de somar.")
  }
  base <- data.frame(Localidade = local[manter],
    Data = as.Date(sprintf("%04d-%02d-01", ano[manter], mes[manter])),
    Real = qtd[manter])

  # ==========================================================================
  # ETAPA 03 — AGREGACAO E CALENDARIO REGULAR NA ESCALA ORIGINAL
  # Operacao: somar tipos, completar meses com NA e construir estado observado.
  # Saida: uma linha por localidade/mes com quantidade e cobertura municipal.
  # Regra: NA presente no grupo torna seu total NA; nao imputar faltantes com 0.
  # ==========================================================================
  mensal <- stats::aggregate(base$Real, by = base[c("Localidade", "Data")],
    FUN = function(x) if (anyNA(x)) NA_real_ else sum(x))
  names(mensal)[3L] <- "Real"
  datas <- seq(as.Date(sprintf("%d-01-01", config$ano_inicio)),
    as.Date(sprintf("%d-12-01", config$ano_fim)), by = "month")
  municipios <- sort(unique(mensal$Localidade))
  completar <- function(nome, parte, esperados = 1L,
      registros = rep(1L, nrow(parte)), conhecidos = as.integer(is.finite(parte$Real))) {
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
  # Agregar antes de completar preserva a soma observada, sem zerar municipios.
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
# ETAPA 07 — AJUSTE EFETIVO DO PROPHET
# Entrada: tabela ds/y contendo apenas quantidades conhecidas anteriores ao teste.
# Operacao: ajustar tendencia linear por trechos e a sazonalidade configurada.
# Saida: um modelo independente, sem intervalos simulados ou arquivos de modelo.
# Regra: manter y em nivel, aceitar zero, nao adicionar feriados ou regressores.
# =============================================================================
ajustar_prophet <- function(treino, config) {
  set.seed(config$seed)
  argumentos <- c(list(df = treino), config$parametros,
    list(seed = config$seed, refresh = 0L))
  do.call(prophet::prophet, argumentos)
}

# =============================================================================
# ETAPA 08 — PREVISAO DIRETA DA GRADE MENSAL FIXA
# Entrada: modelo ajustado e datas explicitas do teste/futuro.
# Operacao: prever yhat nessas datas; conferir alinhamento e valores finitos.
# Saida: quantidades mensais; corte em zero apenas na saida e contagem no console.
# Regra: nao usar grade diaria, integrar diferencas, aplicar expm1 ou realimentar y.
# =============================================================================
prever_mensal_prophet <- function(modelo, datas) {
  datas <- as.Date(datas)
  futuro <- data.frame(ds = datas)
  # predict.prophet recebe df. newdata fica em ... e seria ignorado: nesse caso
  # o pacote preve o proprio historico, e nao as datas de teste/futuro pedidas.
  previsto <- stats::predict(modelo, df = futuro)
  if (!all(c("ds", "yhat") %in% names(previsto))) stop("Saida Prophet sem ds/yhat.")
  datas_retornadas <- as.Date(previsto$ds, tz = "UTC")
  if (nrow(previsto) != length(datas)) {
    stop("Grade de previsao Prophet com tamanho inesperado: ",
      length(datas), " datas solicitadas; ", nrow(previsto), " linhas retornadas.")
  }
  if (anyNA(datas_retornadas)) stop("Prophet retornou datas ausentes ou invalidas.")
  if (anyDuplicated(datas_retornadas)) stop("Prophet retornou datas mensais duplicadas.")
  i <- match(datas, datas_retornadas)
  if (anyNA(i)) stop("Datas previstas nao correspondem a grade mensal solicitada.")
  yhat <- as.numeric(previsto$yhat[i])
  if (any(!is.finite(yhat))) stop("Previsao Prophet nao finita.")
  list(previsao = pmax(0, yhat), negativas_corrigidas = sum(yhat < 0))
}

# ---- 4. RECORTES: CALENDARIO E LOCALIDADES DA RODADA --------------------------
prophet_chave_localidade <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC", x)
  x <- trimws(gsub("[^A-Z0-9]+", " ", x))
  x[x %in% c("PARA", "PARA GERAL")] <- "PARA GERAL"
  x
}

prophet_periodos <- function(config) {
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  recortes <- new.env(parent = baseenv())
  source(caminho, local = recortes, encoding = "UTF-8")
  inicio <- if (is.null(config$inicio_treino)) {
    as.Date(sprintf("%04d-01-01", config$ano_inicio))
  } else config$inicio_treino
  fim <- if (is.null(config$fim_treino)) {
    as.Date(sprintf("%04d-01-01", config$ano_teste)) - 1L
  } else config$fim_treino
  meses <- if (is.null(config$meses_teste)) config$n_teste else config$meses_teste
  recortes$definir_periodos_validacao(list(inicio_treino = inicio,
    fim_treino = fim, meses_teste = meses))
}

prophet_localidades <- function(mensal, selecionadas = NULL) {
  disponiveis <- unique(as.character(mensal$Localidade))
  if (is.null(selecionadas)) return(disponiveis)
  if (!length(selecionadas) || anyNA(selecionadas) || any(!nzchar(selecionadas))) {
    stop("Selecione ao menos uma localidade.")
  }
  idx <- match(prophet_chave_localidade(selecionadas), prophet_chave_localidade(disponiveis))
  if (anyNA(idx)) stop("Localidades nao encontradas: ", paste(selecionadas[is.na(idx)], collapse = ", "))
  unique(disponiveis[idx])
}

prophet_serie_local <- function(mensal, localidade, periodos) {
  parte <- mensal[prophet_chave_localidade(mensal$Localidade) ==
    prophet_chave_localidade(localidade), , drop = FALSE]
  datas <- seq(periodos$inicio_treino,
    as.Date(format(periodos$fim_teste, "%Y-%m-01")), by = "month")
  idx <- match(datas, parte$Data)
  serie <- data.frame(Localidade = localidade, Data = datas, Real = parte$Real[idx])
  cobertura <- c("Municipios_com_registro", "Municipios_com_total_conhecido", "Municipios_esperados")
  for (nome in cobertura) serie[[nome]] <- if (nome %in% names(parte)) parte[[nome]][idx] else NA_real_
  serie
}

# ---- 5. PAI: GRAVACAO UNICA E SUBSTITUICAO SEGURA DO CSV ----------------------
gravar_csv_prophet <- function(previsoes, saida) {
  if (!nrow(previsoes) || anyDuplicated(previsoes[c("Localidade", "Data")])) {
    stop("Previsoes vazias ou com datas duplicadas; CSV anterior preservado.")
  }
  dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile("prophet_novo_", tmpdir = dirname(saida), fileext = ".csv")
  anterior <- NULL
  on.exit({
    unlink(temporario)
    if (!is.null(anterior) && file.exists(anterior) && !file.exists(saida)) file.rename(anterior, saida)
  }, add = TRUE)
  utils::write.csv(previsoes, temporario, row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
  if (file.exists(saida)) {
    anterior <- tempfile("prophet_anterior_", tmpdir = dirname(saida), fileext = ".csv")
    if (!file.rename(saida, anterior)) stop("Nao foi possivel preservar a saida anterior: ", saida)
  }
  if (!file.rename(temporario, saida)) stop("Nao foi possivel concluir o CSV: ", saida)
  if (!is.null(anterior)) unlink(anterior)
  invisible(saida)
}

# ---- 6. WORKER/SEQUENCIAL: UM AJUSTE INDEPENDENTE PARA UMA LOCALIDADE ----------
prophet_log_progresso <- function(localidade, periodos, ...) {
  prefixo <- paste0("[PID ", Sys.getpid(), " | teste ",
    format(periodos$inicio_teste, "%m/%Y"), " a ", format(periodos$fim_teste, "%m/%Y"),
    "] ", localidade, ": ")
  texto <- trimws(gsub("[\r\n]+", " | ", paste0(..., collapse = "")))
  cat(paste0(prefixo, texto, "\n"))
  flush(stdout())
  invisible(NULL)
}

prever_localidade_prophet <- function(mensal, localidade, periodos, futuro, config) {
  inicio <- proc.time()[["elapsed"]]
  erro <- NULL
  avisos <- character()
  prophet_log_progresso(localidade, periodos, "iniciado")
  dados <- tryCatch(withCallingHandlers({
    # 6.1. Separar antes de construir ds/y: Real do teste so entra no CSV.
    serie <- prophet_serie_local(mensal, localidade, periodos)
    teste <- serie$Data >= periodos$inicio_teste
    historico <- serie[!teste, , drop = FALSE]
    conhecidos <- is.finite(historico$Real)
    meses <- unique(as.integer(format(historico$Data[conhecidos], "%m")))
    if (sum(conhecidos) < config$minimo_treino || length(meses) < 12L) {
      stop("historico insuficiente: ", sum(conhecidos), " meses conhecidos e ",
        length(meses), "/12 meses do ano representados.")
    }
    ultimo <- max(historico$Data[conhecidos])
    if (any(!conhecidos)) message(sum(!conhecidos), " meses de treino ausentes; sem imputacao.")
    message("treino com ", sum(conhecidos), " observacoes; ultimo mes conhecido ", ultimo, ".")
    if (ultimo < tail(historico$Data, 1L)) {
      message("historico termina antes do corte; horizonte permanece fixo em ", periodos$inicio_teste, ".")
    }
    if (prophet_chave_localidade(localidade) == "PARA GERAL" &&
        any(historico$Municipios_com_total_conhecido < historico$Municipios_esperados, na.rm = TRUE)) {
      message("soma estadual observada com cobertura municipal parcial.")
    }
    # 6.2. Mesmo modelo do checkpoint, somente valores conhecidos do treino.
    # Nao comprimir datas, imputar NA, diferenciar ou inserir valores do teste.
    treino <- data.frame(ds = historico$Data[conhecidos], y = historico$Real[conhecidos])
    modelo <- ajustar_prophet(treino, config)
    datas_prever <- c(periodos$datas_teste, futuro)
    pred <- prever_mensal_prophet(modelo, datas_prever)
    if (length(pred$previsao) != length(datas_prever) || any(!is.finite(pred$previsao))) {
      stop("Previsoes nao correspondem ao horizonte solicitado.")
    }
    if (pred$negativas_corrigidas > 0L) message(pred$negativas_corrigidas,
      " previsoes negativas limitadas a zero na saida.")
    # 6.3. Reunir historia e teste/futuro em memoria. So o PAI grava o CSV.
    previstos <- rep(NA_real_, nrow(serie))
    stopifnot(sum(teste) == periodos$meses_teste,
      identical(serie$Data[teste], periodos$datas_teste))
    previstos[teste] <- head(pred$previsao, periodos$meses_teste)
    data.frame(Localidade = localidade, Data = c(serie$Data, futuro),
      Real = c(serie$Real, rep(NA_real_, length(futuro))),
      Previsao_PROPHET = c(previstos, tail(pred$previsao, length(futuro))),
      Modelo = "PROPHET", AIC = NA_real_, BIC = NA_real_,
      Particao = c(ifelse(teste, "teste", "treino"), rep("futuro", length(futuro))),
      Municipios_com_registro = c(serie$Municipios_com_registro, rep(NA, length(futuro))),
      Municipios_com_total_conhecido = c(serie$Municipios_com_total_conhecido, rep(NA, length(futuro))),
      Municipios_esperados = c(serie$Municipios_esperados,
        rep(serie$Municipios_esperados[1L], length(futuro))))
  }, message = function(condicao) {
    avisos <<- c(avisos, conditionMessage(condicao))
    prophet_log_progresso(localidade, periodos, conditionMessage(condicao))
    invokeRestart("muffleMessage")
  }, warning = function(condicao) {
    avisos <<- c(avisos, paste0("Aviso: ", conditionMessage(condicao)))
    prophet_log_progresso(localidade, periodos, "Aviso: ", conditionMessage(condicao))
    invokeRestart("muffleWarning")
  }), error = function(condicao) {
    erro <<- conditionMessage(condicao)
    NULL
  })
  segundos <- unname(proc.time()[["elapsed"]] - inicio)
  if (is.null(erro)) {
    prophet_log_progresso(localidade, periodos, "concluido; ", sum(conhecidos),
      " observacoes no treino; ", round(segundos, 2L), " s")
  } else prophet_log_progresso(localidade, periodos, "ignorado; ", erro,
    " (", round(segundos, 2L), " s)")
  list(dados = dados, erro = erro, avisos = avisos, localidade = localidade, segundos = segundos)
}

# ---- 7. CLUSTER PSOCK: INICIALIZACAO UMA VEZ POR EXECUCAO ---------------------
# O controlador e dono do cluster. Reutiliza-o nas janelas, fechando-o ao sair.
criar_cluster_prophet <- function(processos, script_pre_compilacao, raiz = getwd()) {
  if (!is.numeric(processos) || length(processos) != 1L || !is.finite(processos) ||
      processos < 1L || processos > .Machine$integer.max || processos != floor(processos)) {
    stop("processos deve ser um inteiro positivo.")
  }
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  script <- normalizePath(script_pre_compilacao, winslash = "/", mustWork = TRUE)
  nomes <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS", "STAN_NUM_THREADS")
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
  # WORKER: funcao serializada sem capturar conexoes/ambiente do PAI.
  inicializar <- function(raiz, script) {
    setwd(raiz)
    options(mc.cores = 1L, OutDec = ".")
    if (!requireNamespace("prophet", quietly = TRUE)) stop("Pacote ausente no processo R: prophet")
    opcoes <- options(frota.prophet.apenas_funcoes = TRUE)
    on.exit(options(opcoes), add = TRUE)
    produtor <- new.env(parent = globalenv())
    source(script, local = produtor, encoding = "UTF-8")
    if (!is.function(produtor$prever_localidade_prophet)) stop("Atualize o pre-compilador Prophet: tarefa ausente.")
    assign(".frota_prophet_produtor", produtor, envir = globalenv())
    invisible(NULL)
  }
  environment(inicializar) <- baseenv()
  invisible(parallel::clusterCall(cluster, inicializar, raiz, script))
  pronto <- TRUE
  cluster
}

# ---- 8. PAI: VALIDAR A RODADA, DISTRIBUIR TAREFAS E CONSOLIDAR ----------------
executar_prophet_local <- function(config = config_prophet_local, dados_mensais = NULL, cluster = NULL) {
  inicio <- proc.time()[["elapsed"]]
  anteriores <- options(OutDec = ".")
  on.exit({
    options(anteriores)
    segundos <- proc.time()[["elapsed"]] - inicio
    inteiros <- floor(segundos)
    message("TEMPO TOTAL DO PRE-COMPILADOR PROPHET: ",
      sprintf("%02.0f:%02.0f:%02.0f", inteiros %/% 3600, (inteiros %% 3600) %/% 60, inteiros %% 60),
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  config <- utils::modifyList(config_prophet_local, config, keep.null = TRUE)
  periodos <- prophet_periodos(config)
  config$n_teste <- periodos$meses_teste
  config$ano_inicio <- as.integer(format(periodos$inicio_treino, "%Y"))
  config$ano_fim <- as.integer(format(periodos$fim_teste, "%Y"))
  inteiro <- function(x, minimo) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= minimo && x <= .Machine$integer.max && x == floor(x)
  if (!inteiro(config$passos_futuro, 0L) || !inteiro(config$minimo_treino, 24L) ||
      !inteiro(config$seed, 0L)) stop("Conferir passos_futuro>=0, minimo_treino>=24 e seed inteiro.")
  for (campo in c("entrada", "saida")) {
    valor <- config[[campo]]
    if (!is.character(valor) || length(valor) != 1L || is.na(valor) || !nzchar(valor)) stop("Caminho invalido: ", campo)
  }
  canonico <- function(x) tolower(normalizePath(x, winslash = "/", mustWork = FALSE))
  if (canonico(config$entrada) == canonico(config$saida)) stop("Entrada e saida devem ser arquivos distintos.")
  if (!requireNamespace("prophet", quietly = TRUE)) {
    stop('Instale no seu R local: install.packages("prophet", repos = "http://cran.rstudio.com/")')
  }
  # 8.1. Dados injetados evitam nova leitura. A agregacao estadual precede o
  # filtro municipal, no leitor compartilhado com a execucao de producao.
  mensal <- if (is.null(dados_mensais)) ler_mensal_prophet(config) else dados_mensais
  if (!is.data.frame(mensal) || !all(c("Localidade", "Data", "Real") %in% names(mensal))) {
    stop("Dados mensais precisam de Localidade, Data e Real.")
  }
  mensal$Localidade <- trimws(as.character(mensal$Localidade))
  mensal$Data <- as.Date(mensal$Data)
  if (anyNA(mensal$Localidade) || any(!nzchar(mensal$Localidade)) || anyNA(mensal$Data) ||
      any(format(mensal$Data, "%d") != "01") || !is.numeric(mensal$Real) ||
      any(!is.na(mensal$Real) & (!is.finite(mensal$Real) | mensal$Real < 0)) ||
      anyDuplicated(data.frame(Localidade = prophet_chave_localidade(mensal$Localidade), Data = mensal$Data))) {
    stop("Dados mensais invalidos ou duplicados por localidade/mes.")
  }
  localidades <- prophet_localidades(mensal, config$localidades)
  mensal <- mensal[mensal$Data >= periodos$inicio_treino & mensal$Data <= periodos$fim_teste, , drop = FALSE]
  ultimo_teste <- tail(periodos$datas_teste, 1L)
  futuro <- seq(ultimo_teste, by = "month", length.out = config$passos_futuro + 1L)[-1L]
  message("PROPHET: treino ", periodos$inicio_treino, " a ", periodos$fim_treino,
    "; teste ", periodos$inicio_teste, " a ", periodos$fim_teste, ".")
  if (is.null(cluster)) {
    # 8.2. MODO SEQUENCIAL: mesma tarefa do worker, na sessao principal.
    calculados <- lapply(localidades, function(localidade) {
      prever_localidade_prophet(mensal, localidade, periodos, futuro, config)
    })
  } else {
    # ---- INICIO DO BLOCO PARALELO ------------------------------------------
    # 8.3. PAI envia contexto uma vez por rodada e distribui nomes da fila.
    if (!inherits(cluster, "cluster")) stop("cluster deve pertencer ao pacote parallel.")
    contexto <- list(mensal = mensal, periodos = periodos, futuro = futuro, config = config)
    preparar <- function(contexto) {
      assign(".frota_prophet_contexto", contexto, envir = globalenv())
      invisible(NULL)
    }
    # 8.4. WORKER calcula uma localidade e devolve somente dados/diagnosticos.
    trabalhar <- function(localidade) {
      contexto <- get(".frota_prophet_contexto", envir = globalenv(), inherits = FALSE)
      produtor <- get(".frota_prophet_produtor", envir = globalenv(), inherits = FALSE)
      produtor$prever_localidade_prophet(contexto$mensal, localidade,
        contexto$periodos, contexto$futuro, contexto$config)
    }
    environment(preparar) <- environment(trabalhar) <- baseenv()
    invisible(parallel::clusterCall(cluster, preparar, contexto))
    message("PROPHET: ", length(cluster), " processos; fila dinamica de ", length(localidades), " localidades.")
    calculados <- parallel::parLapplyLB(cluster, localidades, trabalhar, chunk.size = 1L)
    # ---- FIM DO BLOCO PARALELO ---------------------------------------------
  }
  # 8.5. PAI coleta, ordena e grava uma unica saida. Nao encerra cluster emprestado.
  resultados <- list()
  for (resultado in calculados) if (is.null(resultado$erro)) resultados[[resultado$localidade]] <- resultado$dados
  message("PROPHET: ", length(resultados), "/", length(calculados), " localidades concluidas; ",
    length(calculados) - length(resultados), " ignoradas/com erro.")
  if (!length(resultados)) stop("Nenhuma localidade processada; CSV anterior preservado.")
  previsoes <- do.call(rbind, resultados)
  rownames(previsoes) <- NULL
  gravar_csv_prophet(previsoes, config$saida)
  message("PROPHET_LOCAL_CONCLUIDO: ", length(resultados), " localidades; ", nrow(previsoes), " linhas.")
  invisible(list(previsoes = previsoes))
}

# ---- 9. SOURCE DIRETO OU IMPORTACAO APENAS DAS FUNCOES ------------------------
# O controlador e os workers carregam com apenas_funcoes=TRUE; nenhuma rodada
# de producao e disparada durante essa importacao. Nao executar na nuvem.
if (!isTRUE(getOption("frota.prophet.apenas_funcoes", FALSE))) executar_prophet_local()

