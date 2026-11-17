# SARIMAX-LOCAL-4-LOG-AO-VIVO-TEMPO
# Base: GitHub publicacao-codigo, 8686bc2679b2d0ec2d500f01eb54e74f1dcdd8ab.
# Recebe recorte, localidades, horizonte e saida do controlador temporal.
# Executar apenas na maquina local, a partir da raiz do projeto.
# Dependencia do projeto: previsoes_validacao_temporal/config_validacao_temporal.r.
# Pacotes locais, se necessarios:
# install.packages(c("dplyr", "forecast"), repos = "http://cran.rstudio.com/")
# Source direto conserva o padrao publicado: treino 2015--2024, teste 2025,
# futuro 2026--2031 e CSV de producao. O controlador carrega apenas as funcoes.
# O controlador pode fornecer cluster PSOCK persistente. Somente o processo
# principal consolida/grava o CSV; cada worker calcula uma localidade por vez.
# Revisao de leitura (16/11/2026): blocos numerados; calculos preservados.
# PAI = sessao R que controla a execucao. WORKER = processo R de calculo.
# Fluxo: controlador cria cluster [5] -> executa rodada [6] -> fecha cluster.
# Na rodada: preparar contexto -> distribuir localidades -> coletar -> gravar.
# Mensagens municipais sao emitidas durante o calculo, identificadas por PID.
# O tempo total desta funcao inclui leitura, calculos e gravacao do CSV.

# ---- 1. CONFIGURACAO DE PRODUCAO E PREPARACAO DOS DADOS ------------------------
# Configuracao da validacao fica no auxiliar compartilhado, nao neste bloco.
config_sarimax_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_sarimax_pre_comp.csv",
  inicio_treino = as.Date("2015-01-01"),
  fim_treino = as.Date("2024-12-31"),
  meses_teste = 12L, passos_futuro = 72L,
  localidades = NULL, minimo_treino = 24L
)

sarimax_chave_localidade <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC", x)
  x <- trimws(gsub("[^A-Z0-9]+", " ", x))
  x[x %in% c("PARA", "PARA GERAL")] <- "PARA GERAL"
  x
}

sarimax_periodos <- function(config) {
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  ambiente <- new.env(parent = baseenv())
  source(caminho, local = ambiente, encoding = "UTF-8")
  ambiente$definir_periodos_validacao(config)
}

ler_dados_sarimax <- function(entrada) {
  dados <- utils::read.csv(entrada, check.names = FALSE, fileEncoding = "UTF-8-BOM",
    stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"))
  obrigatorias <- c("LOCAL", "ANO", "MES", "QTD_VEICULO")
  if (!all(obrigatorias %in% names(dados))) stop("CSV sem LOCAL/ANO/MES/QTD_VEICULO.")
  for (nome in c("ANO", "MES", "QTD_VEICULO")) {
    original <- as.character(dados[[nome]])
    valor <- suppressWarnings(as.numeric(original))
    if (any(!is.na(original) & is.na(valor))) stop("Valor nao numerico em ", nome)
    dados[[nome]] <- valor
  }
  if (any(!is.finite(dados$ANO) | dados$ANO != floor(dados$ANO) |
          dados$ANO < 1 | dados$ANO > 9999 | !is.finite(dados$MES) |
          dados$MES != floor(dados$MES) | dados$MES < 1 | dados$MES > 12)) {
    stop("ANO/MES invalidos no CSV.")
  }
  if (any(!is.na(dados$QTD_VEICULO) &
          (!is.finite(dados$QTD_VEICULO) | dados$QTD_VEICULO < 0))) {
    stop("QTD_VEICULO deve ser nao negativa ou NA.")
  }
  dados$LOCAL <- trimws(as.character(dados$LOCAL))
  if (anyNA(dados$LOCAL) || any(!nzchar(dados$LOCAL))) stop("LOCAL ausente no CSV.")
  dados$Data <- as.Date(sprintf("%04d-%02d-01", as.integer(dados$ANO), as.integer(dados$MES)))
  dados
}

sarimax_localidades <- function(dados, selecionadas = NULL) {
  disponiveis <- unique(c("PARÁ (Geral)", dados$LOCAL))
  if (is.null(selecionadas)) return(disponiveis)
  if (!length(selecionadas) || anyNA(selecionadas)) stop("Selecione ao menos uma localidade.")
  idx <- match(sarimax_chave_localidade(selecionadas), sarimax_chave_localidade(disponiveis))
  if (anyNA(idx)) stop("Localidades nao encontradas: ", paste(selecionadas[is.na(idx)], collapse = ", "))
  unique(disponiveis[idx])
}

sarimax_serie_local <- function(dados, localidade, periodos) {
  parte <- dados[dados$Data >= periodos$inicio_treino &
    dados$Data <= periodos$fim_teste, , drop = FALSE]
  if (sarimax_chave_localidade(localidade) != "PARA GERAL") {
    parte <- parte[sarimax_chave_localidade(parte$LOCAL) ==
      sarimax_chave_localidade(localidade), , drop = FALSE]
  }
  # Soma dos tipos presentes. Quantidade explicitamente desconhecida continua
  # desconhecida; ausencia de linha nao vira zero. O estado e a soma observada.
  agregado <- parte |>
    dplyr::group_by(Data) |>
    dplyr::summarise(Real = sum(QTD_VEICULO), .groups = "drop")
  calendario <- data.frame(Data = seq(periodos$inicio_treino,
    as.Date(format(periodos$fim_teste, "%Y-%m-01")), by = "month"))
  dplyr::left_join(calendario, agregado, by = "Data")
}

# ---- 2. PAI: GRAVACAO DO CSV CONSOLIDADO --------------------------------------
# Esta funcao e chamada apos reunir as localidades. Workers nao gravam CSV.
gravar_csv_sarimax <- function(dados, saida) {
  dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile("sarimax_novo_", tmpdir = dirname(saida), fileext = ".csv")
  anterior <- NULL
  on.exit({
    unlink(temporario)
    if (!is.null(anterior) && file.exists(anterior) && !file.exists(saida)) {
      file.rename(anterior, saida)
    }
  }, add = TRUE)
  utils::write.csv(dados, temporario, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  if (file.exists(saida)) {
    anterior <- tempfile("sarimax_anterior_", tmpdir = dirname(saida), fileext = ".csv")
    if (!file.rename(saida, anterior)) stop("Nao foi possivel preservar a saida anterior: ", saida)
  }
  if (!file.rename(temporario, saida)) stop("Nao foi possivel concluir o CSV: ", saida)
  if (!is.null(anterior)) unlink(anterior)
  invisible(saida)
}

# ---- 3. MODELAGEM: AJUSTE E PREVISAO SARIMAX ----------------------------------
# Mesma modelagem tanto no processo principal quanto nos workers.
ajustar_prever_sarimax <- function(serie_treino, dummy_treino, dummy_previsao, horizonte) {
  # Mesmo regressor de 2013--2017 e mesmas buscas principal/fallback do checkpoint.
  # Regressor constante nao e identificavel; omiti-lo equivale a remocao feita
  # pelo proprio auto.arima. O calendario e as observacoes nao sao alterados.
  if (length(unique(dummy_treino)) < 2L) {
    dummy_treino <- dummy_previsao <- NULL
  }
  ajustar <- function(simplificado) {
    argumentos <- list(y = serie_treino, xreg = dummy_treino,
      stepwise = simplificado, approximation = simplificado,
      seasonal = TRUE, D = 1)
    if (!simplificado) argumentos$lambda <- 0
    modelo <- do.call(forecast::auto.arima, argumentos)
    previsao <- forecast::forecast(modelo, xreg = dummy_previsao, h = horizonte)
    valores <- as.numeric(previsao$mean)
    if (length(valores) != horizonte || any(!is.finite(valores))) {
      stop("Previsao SARIMAX com tamanho ou valores invalidos.")
    }
    list(previsao = valores, AIC = stats::AIC(modelo), BIC = stats::BIC(modelo))
  }
  tryCatch(ajustar(FALSE), error = function(e) {
    message("  Busca principal falhou; usando fallback: ", conditionMessage(e))
    ajustar(TRUE)
  })
}

# ---- 3.A. CONSOLE: EMITIR UMA LINHA DE PROGRESSO IMEDIATAMENTE -----------------
# cat/flush usam stdout sem cair no capturador de message() do bloco 4.
# No PSOCK local, outfile="" permite que os workers escrevam no terminal.
sarimax_log_progresso <- function(localidade, periodos, ...) {
  prefixo <- paste0("[PID ", Sys.getpid(), " | teste ",
    format(periodos$inicio_teste, "%m/%Y"), " a ",
    format(periodos$fim_teste, "%m/%Y"), "] ", localidade, ": ")
  texto <- trimws(gsub("[\r\n]+", " | ", paste0(..., collapse = "")))
  cat(paste0(prefixo, texto, "\n"))
  flush(stdout())
  invisible(NULL)
}

# ---- 4. TAREFA: CALCULAR UMA LOCALIDADE ---------------------------------------
# Executada pelo WORKER no modo paralelo, ou pelo PAI no modo sequencial.
# Recebe dados/recorte; devolve resultado e diagnosticos em memoria, sem CSV.
prever_localidade_sarimax <- function(dados_basicos, localidade, periodos, futuro, minimo_treino) {
  # 4.1. Iniciar cronometro e recipientes de mensagens/erros.
  inicio <- proc.time()[["elapsed"]]
  avisos <- character()
  erro <- NULL
  sarimax_log_progresso(localidade, periodos, "iniciado")
  # Mensagens/avisos sao emitidos no momento em que ocorrem e tambem devolvidos
  # como diagnosticos. Erros continuam isolados por localidade nos dois modos.
  dados <- tryCatch(withCallingHandlers({
    # 4.2. Montar o calendario ate o fim do teste e separar o treino ate o corte.
    historico <- sarimax_serie_local(dados_basicos, localidade, periodos)
    treino <- historico[historico$Data <= periodos$fim_treino, , drop = FALSE]
    if (sum(is.finite(treino$Real)) < minimo_treino) {
      stop("menos de ", minimo_treino, " meses conhecidos no treino.")
    }
    # NA internos permanecem na serie regular; o pacote trata observacoes
    # ausentes na estimacao. Nao compactar datas nem preencher com zeros.
    primeira <- which(is.finite(treino$Real))[1L]
    treino <- treino[seq.int(primeira, nrow(treino)), , drop = FALSE]
    # 4.3. Construir ts/regressor e chamar a modelagem do bloco 3.
    serie <- stats::ts(treino$Real,
      start = c(as.integer(format(treino$Data[1L], "%Y")),
        as.integer(format(treino$Data[1L], "%m"))), frequency = 12)
    datas_previsao <- c(periodos$datas_teste, futuro)
    dummy <- function(datas) as.numeric(datas >= as.Date("2013-01-01") &
      datas <= as.Date("2017-12-31"))
    ajuste <- ajustar_prever_sarimax(serie, dummy(treino$Data),
      dummy(datas_previsao), length(datas_previsao))
    # 4.4. Montar as linhas do CSV; a gravacao sera feita depois pelo PAI.
    saida <- data.frame(Localidade = localidade,
      Data = c(historico$Data, futuro),
      Real = c(historico$Real, rep(NA_real_, length(futuro))),
      Previsao_SARIMAX = NA_real_, AIC = ajuste$AIC, BIC = ajuste$BIC,
      Modelo = "SARIMAX", stringsAsFactors = FALSE)
    saida$Previsao_SARIMAX[match(datas_previsao, saida$Data)] <- ajuste$previsao
    saida
  }, message = function(condicao) {
    # 4.5. Emitir e reunir mensagens/avisos/erro desta localidade.
    avisos <<- c(avisos, conditionMessage(condicao))
    sarimax_log_progresso(localidade, periodos, conditionMessage(condicao))
    invokeRestart("muffleMessage")
  }, warning = function(condicao) {
    avisos <<- c(avisos, paste0("Aviso: ", conditionMessage(condicao)))
    sarimax_log_progresso(localidade, periodos, "Aviso: ", conditionMessage(condicao))
    invokeRestart("muffleWarning")
  }), error = function(condicao) {
    erro <<- conditionMessage(condicao)
    NULL
  })
  # 4.6. Devolver a tarefa concluida ou com erro, sem alterar outras localidades.
  segundos <- unname(proc.time()[["elapsed"]] - inicio)
  if (is.null(erro)) {
    sarimax_log_progresso(localidade, periodos, "concluido; ", nrow(treino),
      " meses no calendario de treino; ", round(segundos, 2L), " s")
  } else {
    sarimax_log_progresso(localidade, periodos, "ignorado; ", erro,
      " (", round(segundos, 2L), " s)")
  }
  list(dados = dados, erro = erro, avisos = avisos, localidade = localidade,
    segundos = segundos)
}

# ---- 5. CLUSTER: CRIACAO E INICIALIZACAO DOS PROCESSOS -------------------------
# Chamado uma vez pelo controlador e reutilizado nas janelas em sequencia.
criar_cluster_sarimax <- function(processos, script_pre_compilacao, raiz = getwd()) {
  # 5.1. PAI: conferir quantidade de processos e caminhos.
  if (!is.numeric(processos) || length(processos) != 1L || !is.finite(processos) ||
      processos < 1 || processos > .Machine$integer.max ||
      processos != floor(processos)) stop("processos deve ser um inteiro positivo.")
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  script <- normalizePath(script_pre_compilacao, winslash = "/", mustWork = TRUE)
  # 5.2. PAI: limitar threads nativas herdadas pelos novos processos.
  # Seis workers significam seis processos de calculo, alem da sessao principal.
  # Os novos processos herdam estes limites antes de carregar bibliotecas.
  # Restaurar o ambiente da sessao principal ao terminar a inicializacao.
  nomes <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS")
  anteriores <- Sys.getenv(nomes, unset = NA_character_)
  on.exit({
    ausentes <- is.na(anteriores)
    if (any(ausentes)) Sys.unsetenv(nomes[ausentes])
    if (any(!ausentes)) do.call(Sys.setenv, as.list(anteriores[!ausentes]))
  }, add = TRUE)
  do.call(Sys.setenv, as.list(stats::setNames(rep("1", length(nomes)), nomes)))
  # 5.3. PAI: criar PSOCK e registrar limpeza caso a inicializacao falhe.
  # Se tudo der certo, o encerramento passa a ser responsabilidade do controlador.
  # Liberar stdout/stderr dos workers locais para o terminal do R/VS Code.
  # A ordem das linhas acompanha as tarefas; linhas de processos podem se intercalar.
  cluster <- parallel::makePSOCKcluster(as.integer(processos), outfile = "")
  pronto <- FALSE
  on.exit(if (!pronto) try(parallel::stopCluster(cluster), silent = TRUE), add = TRUE)
  # 5.4. WORKER: corpo de inicializacao executado uma vez em cada processo.
  # Aqui o PAI apenas define a funcao; clusterCall abaixo executara seu corpo.
  inicializar <- function(raiz, script) {
    setwd(raiz)
    for (pacote in c("dplyr", "forecast")) {
      if (!requireNamespace(pacote, quietly = TRUE)) stop("Pacote ausente no processo R: ", pacote)
    }
    opcoes <- options(frota.sarimax.apenas_funcoes = TRUE)
    on.exit(options(opcoes), add = TRUE)
    options(OutDec = ".") # Processos exclusivos desta execucao; CSV e gravado pelo pai.
    produtor <- new.env(parent = globalenv())
    source(script, local = produtor, encoding = "UTF-8")
    if (!exists("prever_localidade_sarimax", envir = produtor, inherits = FALSE) ||
        !is.function(produtor$prever_localidade_sarimax)) {
      stop("Atualize o pre-compilador SARIMAX: funcao por localidade ausente.")
    }
    assign(".frota_sarimax_produtor", produtor, envir = globalenv())
    invisible(NULL)
  }
  # FIM do corpo executado no WORKER; os comandos seguintes rodam no PAI.

  # 5.5. PAI: enviar a inicializacao, esperar confirmacao e devolver o cluster.
  # Nao serializar o frame que contem as conexoes do cluster.
  environment(inicializar) <- baseenv()
  invisible(parallel::clusterCall(cluster, inicializar, raiz, script))
  pronto <- TRUE
  cluster
}

# ---- 6. PAI: EXECUTAR UMA RODADA E CONSOLIDAR SUAS LOCALIDADES -----------------
# O cluster e emprestado pelo controlador; esta funcao nao o cria nem encerra.
executar_sarimax_local <- function(
    config = config_sarimax_local, dados_basicos = NULL, cluster = NULL
) {
  # 6.0. Cronometrar TODA a chamada, inclusive leitura e gravacao. O cluster
  # emprestado continua sendo encerrado pelo controlador, nao por esta funcao.
  inicio_execucao <- proc.time()[["elapsed"]]
  opcoes <- NULL
  on.exit({
    if (!is.null(opcoes)) options(opcoes)
    segundos <- proc.time()[["elapsed"]] - inicio_execucao
    inteiros <- floor(segundos)
    duracao <- sprintf("%02.0f:%02.0f:%02.0f", inteiros %/% 3600,
      (inteiros %% 3600) %/% 60, inteiros %% 60)
    message("TEMPO TOTAL DO PRE-COMPILADOR SARIMAX: ", duracao,
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  # 6.1. PAI: validar configuracao/dependencias e preservar opcoes da sessao.
  config <- utils::modifyList(config_sarimax_local, config)
  periodos <- sarimax_periodos(config)
  inteiro <- function(x, minimo) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= minimo && x == floor(x)
  if (!inteiro(config$passos_futuro, 0) || !inteiro(config$minimo_treino, 24)) {
    stop("Conferir passos_futuro>=0 e minimo_treino>=24.")
  }
  for (pacote in c("dplyr", "forecast")) {
    if (!requireNamespace(pacote, quietly = TRUE)) {
      stop('Instale no R local: install.packages("', pacote, '", repos = "http://cran.rstudio.com/")')
    }
  }
  opcoes <- options(OutDec = ".")
  # 6.2. PAI: receber/ler a base e selecionar localidades.
  # A serie estadual utiliza a base completa; o filtro municipal e por tarefa.
  if (is.null(dados_basicos)) {
    dados_basicos <- ler_dados_sarimax(config$entrada)
    localidades <- sarimax_localidades(dados_basicos, config$localidades)
  } else {
    # O controlador ja conferiu os nomes na base completa. Uma localidade
    # pode nao ter linhas no recorte antigo: nesse caso, sera ignorada abaixo.
    localidades <- if (is.null(config$localidades)) {
      sarimax_localidades(dados_basicos)
    } else unique(as.character(config$localidades))
    if (!length(localidades) || anyNA(localidades) || any(!nzchar(localidades))) {
      stop("Selecione ao menos uma localidade valida.")
    }
  }
  # 6.3. PAI: definir o horizonte desta rodada (validacao usa passos_futuro=0).
  futuro <- if (config$passos_futuro > 0L) {
    seq(as.Date(format(periodos$fim_teste + 1L, "%Y-%m-01")), by = "month",
      length.out = as.integer(config$passos_futuro))
  } else as.Date(character())
  message("SARIMAX: treino ", periodos$inicio_treino, " a ", periodos$fim_treino,
    "; teste ", periodos$inicio_teste, " a ", periodos$fim_teste, ".")
  # 6.4. EXECUCAO SEQUENCIAL: o PAI calcula cada localidade pelo bloco 4.
  if (is.null(cluster)) {
    calculados <- lapply(localidades, function(localidade) {
      prever_localidade_sarimax(dados_basicos, localidade, periodos, futuro, config$minimo_treino)
    })
  } else {
    # ---- INICIO DA DISTRIBUICAO PARALELA ------------------------------------
    # 6.5. PAI: montar o contexto compartilhado de dados/recorte desta rodada.
    if (!inherits(cluster, "cluster")) stop("cluster deve ser um cluster do pacote parallel.")
    contexto <- list(dados_basicos = dados_basicos, periodos = periodos,
      futuro = futuro, minimo_treino = config$minimo_treino)
    # 6.6. WORKER: receber o contexto. Definicao aqui; execucao por clusterCall.
    preparar <- function(contexto) {
      assign(".frota_sarimax_contexto", contexto, envir = globalenv())
      invisible(NULL)
    }
    # 6.7. WORKER: calcular UMA localidade com o contexto recebido e devolver
    # sua lista de dados/avisos/erro. O proximo municipio sera enviado pelo PAI.
    trabalhar <- function(localidade) {
      contexto <- get(".frota_sarimax_contexto", envir = globalenv(), inherits = FALSE)
      produtor <- get(".frota_sarimax_produtor", envir = globalenv(), inherits = FALSE)
      produtor$prever_localidade_sarimax(contexto$dados_basicos, localidade,
        contexto$periodos, contexto$futuro, contexto$minimo_treino)
    }
    # FIM das funcoes de WORKER; a coordenacao abaixo e executada pelo PAI.
    # 6.8. PAI: enviar o contexto uma vez a cada worker, sem capturar sockets.
    environment(preparar) <- environment(trabalhar) <- baseenv()
    invisible(parallel::clusterCall(cluster, preparar, contexto))
    message("SARIMAX: ", length(cluster), " processos; fila dinamica de ",
      length(localidades), " localidades.")
    # 6.9. PAI: fila dinamica, uma localidade por tarefa. Um worker livre recebe
    # a proxima pendente. Esta chamada espera a coleta completa da rodada.
    # A lista retorna na ordem solicitada, mesmo que as tarefas terminem fora dela.
    calculados <- parallel::parLapplyLB(cluster, localidades, trabalhar, chunk.size = 1L)
    # ---- FIM DA DISTRIBUICAO PARALELA ---------------------------------------
  }

  # 6.10. PAI: reunir resultados. Cada tarefa ja emitiu seus eventos ao vivo;
  # nao repetir aqui as mensagens nem simular uma ordem de conclusao.
  resultados <- list()
  for (i in seq_along(calculados)) {
    resultado <- calculados[[i]]
    if (is.null(resultado$erro)) {
      resultados[[resultado$localidade]] <- resultado$dados
    }
  }
  message("SARIMAX: ", length(resultados), "/", length(calculados),
    " localidades concluidas; ", length(calculados) - length(resultados), " ignoradas/com erro.")
  if (!length(resultados)) stop("Nenhuma localidade processada; CSV anterior preservado.")
  # 6.11. PAI: consolidar localidades e gravar UM arquivo para esta rodada.
  saida <- do.call(rbind, resultados)
  rownames(saida) <- NULL
  gravar_csv_sarimax(saida, config$saida)
  message("CSV SARIMAX: ", config$saida)
  invisible(saida)
}

# ---- 7. ENTRADA: SOURCE DIRETO OU IMPORTACAO SOMENTE DAS FUNCOES ---------------
# O controlador e os workers importam com apenas_funcoes=TRUE.
if (!isTRUE(getOption("frota.sarimax.apenas_funcoes", FALSE))) {
  executar_sarimax_local()
}
