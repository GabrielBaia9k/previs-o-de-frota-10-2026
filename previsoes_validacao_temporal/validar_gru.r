# VALIDACAO-GRU-GLOBAL-1-PSOCK-ORIGENS-CAUSAL
# Executar SOMENTE no R local, com a raiz do projeto como diretorio:
# source("previsoes_validacao_temporal/validar_gru.r", encoding = "UTF-8")
# Copiar juntos este controlador, config_validacao_temporal.r e o produtor
# Previsoes_pre_compilacao/pre_compilar_GRU_global.r, revisao causal global1.
# UMA rede global por origem; a unidade da fila PSOCK e a origem, nao municipio.
# processos=6 e teto: as cinco origens usam ate cinco workers, uma thread cada.
# paralelo=FALSE usa um unico worker PSOCK novo, sem TensorFlow na sessao PAI.
# O PAI le a base uma vez e retém o Real/cobertura de teste para compor os CSVs.
# Workers recebem somente tres colunas do treino permitido e datas/configuracao.
# Nao gerar perdas, metricas ou outros arquivos; nao reusar o teste legado2025.
# Importacao, backend, ajustes e testes pertencem ao Windows; nao rodar na nuvem.

# ---- 1. CONFIGURACAO: BLOCO CENTRAL DO AUXILIAR COMPARTILHADO ------------------
config_validacao_gru <- local({
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  comum <- new.env(parent = baseenv())
  source(caminho, local = comum, encoding = "UTF-8")
  if (!is.function(comum$configurar_validacao_temporal)) stop("Atualize o auxiliar de configuracao: ", caminho)
  comum$configurar_validacao_temporal("GRU")
})

# ---- 2. COMPATIBILIDADE: CONFERIR AST ANTES DE SOURCE/PYTHON/TREINAMENTO -------
vlg_carregar_produtor <- function(caminho) {
  # O legado ignora apenas_funcoes e treina ao importar. Verificar declaracoes
  # antes de source evita executar acidentalmente essa copia desatualizada.
  expressoes <- as.list(parse(file = caminho, encoding = "UTF-8", keep.source = FALSE))
  atribuicoes <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
    is.symbol(x[[2L]]), expressoes)
  definicoes <- stats::setNames(lapply(atribuicoes, function(x) x[[3L]]),
    vapply(atribuicoes, function(x) as.character(x[[2L]]), character(1L)))
  revisao <- "GRU-GLOBAL-1-CAUSAL-VALIDACAO"
  obrigatorias <- c("ler_mensal_gru", "gru_periodos", "gru_localidades",
    "resolver_python_gru", "criar_cluster_gru", "calcular_origem_gru",
    "compor_csv_gru", "gravar_csv_gru", "executar_gru_global")
  funcoes <- vapply(obrigatorias, function(n) {
    x <- definicoes[[n]]
    is.call(x) && identical(x[[1L]], as.name("function"))
  }, logical(1L))
  guarda <- quote(if (!isTRUE(getOption("frota.gru.apenas_funcoes", FALSE))) executar_gru_global())
  possui_guarda <- any(vapply(expressoes, function(x) identical(x, guarda), logical(1L)))
  config_ast <- definicoes[["config_gru_global"]]
  possui_config <- is.call(config_ast) && identical(config_ast[[1L]], as.name("list")) &&
    all(c("salvar_historicos", "threads_treino") %in% names(as.list(config_ast)))
  if (!identical(definicoes[["gru_versao"]], revisao) || !all(funcoes) ||
      !possui_guarda || !possui_config) {
    stop("Substitua ", caminho, " pela revisao ", revisao,
      " e atualize os tres scripts GRU juntos. Produtor sem interface causal global/controle de historicos; ",
      "nenhum script do produtor foi executado.", call. = FALSE)
  }
  interfaces <- list(calcular_origem_gru = c("config", "treino_mensal"),
    compor_csv_gru = c("dados_mensais", "previsoes", "config"),
    criar_cluster_gru = c("processos", "script_pre_compilacao", "raiz", "python", "threads"))
  for (nome in names(interfaces)) {
    if (!all(interfaces[[nome]] %in% names(definicoes[[nome]][[2L]]))) {
      stop("Pre-compilador GRU sem API compativel em ", nome,
        "; nenhum script do produtor foi executado.", call. = FALSE)
    }
  }
  opcoes <- options(frota.gru.apenas_funcoes = TRUE)
  on.exit(options(opcoes), add = TRUE)
  produtor <- new.env(parent = globalenv())
  source(caminho, local = produtor, encoding = "UTF-8")
  if (!identical(produtor$gru_versao, revisao) || !is.list(produtor$config_gru_global) ||
      !all(vapply(obrigatorias, function(n) is.function(produtor[[n]]), logical(1L)))) {
    stop("Pre-compilador GRU incompativel apos importacao; nenhum ajuste iniciado.")
  }
  produtor
}

# Resolver caminhos existentes e componentes ainda inexistentes, inclusive ..,
# antes de comparar destinos. Nao depender de correspondencia parcial por $.
vlg_caminho_canonico <- function(caminho) {
  caminho <- path.expand(caminho)
  caminho <- gsub("\\", "/", caminho, fixed = TRUE)
  if (!grepl("^(/|[A-Za-z]:/)", caminho)) caminho <- file.path(getwd(), caminho)
  partes <- strsplit(caminho, "/", fixed = TRUE)[[1L]]
  pilha <- character()
  for (parte in partes) {
    if (parte %in% c("", ".")) next
    if (parte == "..") {
      if (length(pilha) && !grepl("^[A-Za-z]:$", tail(pilha, 1L))) pilha <- head(pilha, -1L)
    } else pilha <- c(pilha, parte)
  }
  prefixo <- if (startsWith(caminho, "//")) "//" else if (startsWith(caminho, "/")) "/" else ""
  caminho <- paste0(prefixo, paste(pilha, collapse = "/"))
  faltantes <- character()
  existente <- caminho
  while (!file.exists(existente) && !dir.exists(existente)) {
    pai <- dirname(existente)
    if (identical(pai, existente)) break
    faltantes <- c(basename(existente), faltantes)
    existente <- pai
  }
  existente <- normalizePath(existente, winslash = "/", mustWork = FALSE)
  if (length(faltantes)) existente <- paste(c(existente, faltantes), collapse = "/")
  tolower(existente)
}

# ---- 3. PAI: LER/CONFERIR UMA VEZ E CONSTRUIR TAREFAS SEM REAL DE TESTE -------
executar_validacao_gru <- function(config = config_validacao_gru) {
  inicio_total <- proc.time()[["elapsed"]]
  cluster <- NULL
  opcoes <- options(OutDec = ".")
  on.exit({
    if (!is.null(cluster)) {
      message("Encerrando os processos GRU...")
      tryCatch(parallel::stopCluster(cluster), error = function(e) message("Falha ao encerrar cluster: ", conditionMessage(e)))
    }
    options(opcoes)
    segundos <- proc.time()[["elapsed"]] - inicio_total
    n <- floor(segundos)
    message("TEMPO TOTAL DA VALIDACAO GRU: ",
      sprintf("%02.0f:%02.0f:%02.0f", n %/% 3600, (n %% 3600) %/% 60, n %% 60),
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  # 3.1. Configuracao comum, recortes e unidade paralela: uma origem global.
  if (!file.exists(config$script_recortes)) stop("Arquivo necessario: ", config$script_recortes)
  recortes <- new.env(parent = baseenv())
  source(config$script_recortes, local = recortes, encoding = "UTF-8")
  recortes$validar_arquivos_validacao(config)
  periodos <- recortes$definir_rodadas_validacao(config)
  anos <- as.integer(names(periodos))
  fim <- as.integer(config$ano_fim_teste)
  if (!is.logical(config$reutilizar_2025) || length(config$reutilizar_2025) != 1L ||
      is.na(config$reutilizar_2025)) stop("reutilizar_2025 deve ser TRUE ou FALSE.")
  if (config$reutilizar_2025) stop("Mantenha reutilizar_2025=FALSE para GRU: recalcular todas as origens, ",
    "inclusive2025, pelo protocolo causal. O CSV legado nao comprova esse protocolo.")
  if (!is.logical(config$paralelo) || length(config$paralelo) != 1L || is.na(config$paralelo)) {
    stop("paralelo deve ser TRUE ou FALSE.")
  }
  if (!is.numeric(config$processos) || length(config$processos) != 1L || !is.finite(config$processos) ||
      config$processos < 1 || config$processos > .Machine$integer.max || config$processos != floor(config$processos)) {
    stop("processos deve ser um inteiro positivo.")
  }
  produtor <- vlg_carregar_produtor(config$script_pre_compilacao)
  message("Produtor: ", produtor$gru_versao,
    "; uma rede global por origem; sem reuso ou exportacao de historicos.")
  # 3.2. Leitura/agregacao completa uma vez no PAI. Filtro localidades escolhe
  # somente saidas; nao reduzir as series disponiveis ao treinamento global.
  leitura <- utils::modifyList(produtor$config_gru_global, list(
    entrada = config$entrada, inicio_treino = config$inicio_treino,
    fim_treino = periodos[[1L]]$fim_treino, meses_teste = periodos[[1L]]$meses_teste,
    passos_futuro = 0L, salvar_historicos = FALSE, threads_treino = 1L,
    script_pre_compilacao = config$script_pre_compilacao), keep.null = TRUE)
  if ("python" %in% names(config)) leitura$python <- config$python
  dados <- produtor$ler_mensal_gru(leitura)
  obrigatorias <- c("Localidade", "Data", "Real", "Municipios_com_registro",
    "Municipios_com_total_conhecido", "Municipios_esperados")
  if (!is.data.frame(dados) || !all(obrigatorias %in% names(dados))) {
    stop("Leitor GRU deve retornar series mensais e cobertura: ", paste(obrigatorias, collapse = ", "))
  }
  dados$Data <- as.Date(dados$Data)
  dados$Localidade <- trimws(as.character(dados$Localidade))
  if (anyNA(dados$Data) || any(format(dados$Data, "%d") != "01") ||
      anyNA(dados$Localidade) || any(!nzchar(dados$Localidade)) || !is.numeric(dados$Real) ||
      any(!is.na(dados$Real) & (!is.finite(dados$Real) | dados$Real < 0)) ||
      anyDuplicated(dados[c("Localidade", "Data")])) stop("Series mensais GRU invalidas.")
  dados <- dados[dados$Data >= config$inicio_treino &
    dados$Data <= as.Date(sprintf("%04d-12-01", fim)), , drop = FALSE]
  conhecidos <- dados$Data[is.finite(dados$Real)]
  if (!length(conhecidos) || max(conhecidos) < as.Date(sprintf("%04d-12-01", fim))) {
    stop("A entrada nao possui observacoes ate dezembro de ano_fim_teste. Confira o recorte.")
  }
  selecionadas <- if (is.null(config$localidades)) NULL else produtor$gru_localidades(dados, config$localidades)
  leitura$localidades <- selecionadas
  message("Filtro de SAIDA: ", if (is.null(selecionadas)) "todas as localidades elegiveis em cada origem" else
    paste(selecionadas, collapse = ", "), ". O pool global sera definido somente pelo treino de cada origem.")

  # 3.3. Conferir TODOS os destinos antes de Python, workers, ajustes ou escrita.
  saidas <- stats::setNames(vapply(anos, function(ano) file.path(config$pasta_saida,
    recortes$nome_arquivo_validacao("GRU", ano, fim)), character(1L)), as.character(anos))
  protegidos <- unique(c(config$entrada, config$script_recortes, config$script_pre_compilacao,
    config$referencia_2025, unlist(produtor$config_gru_global[c("entrada", "saida", "historico_perdas")], use.names = FALSE)))
  protegidos <- protegidos[!is.na(protegidos) & nzchar(protegidos)]
  destinos <- vapply(saidas, vlg_caminho_canonico, character(1L))
  if (anyDuplicated(destinos) || any(destinos %in% vapply(protegidos, vlg_caminho_canonico, character(1L))) ||
      any(dir.exists(saidas))) {
    stop("Saida da validacao nao pode substituir entrada, producao, historico, scripts ou outra rodada.")
  }
  # 3.4. Retirar QUANTIDADES E COBERTURAS do teste antes de serializar os jobs.
  # Nem o Real do teste nem Municipios_esperados do periodo completo chegam ao
  # worker. Somente as tres colunas necessarias para preparar/ajustar o modelo.
  jobs <- lapply(anos, function(ano) {
    p <- periodos[[as.character(ano)]]
    configuracao <- utils::modifyList(leitura, list(
      saida = saidas[[as.character(ano)]], inicio_treino = p$inicio_treino,
      fim_treino = p$fim_treino, meses_teste = p$meses_teste,
      passos_futuro = 0L, salvar_historicos = FALSE, threads_treino = 1L,
      localidades = selecionadas), keep.null = TRUE)
    treino <- dados[dados$Data >= p$inicio_treino & dados$Data <= p$fim_treino,
      c("Localidade", "Data", "Real"), drop = FALSE]
    message("Origem ", ano, ": treino ", p$inicio_treino, " a ", p$fim_treino,
      "; teste ", p$inicio_teste, " a ", p$fim_teste, " (", p$meses_teste, " meses); um ajuste global.")
    list(ano = ano, config = configuracao, treino_mensal = treino)
  })

  # ---- 4. CLUSTER: SEMPRE PROCESSOS FRESCOS; SEM BACKEND NO PAI --------------
  # 4.1. Mesmo Python em todos os workers. Resolver nao inicializa TensorFlow.
  leitura$python <- produtor$resolver_python_gru(leitura)
  for (i in seq_along(jobs)) jobs[[i]]$config$python <- leitura$python
  processos <- if (config$paralelo) min(as.integer(config$processos), length(jobs)) else 1L
  cluster <- produtor$criar_cluster_gru(processos, config$script_pre_compilacao,
    raiz = getwd(), python = leitura$python, threads = 1L)
  message("Validacao GRU: ", length(jobs), " origens; ", length(cluster),
    " worker(s) PSOCK de uma thread; uma rede global por tarefa; sem cluster aninhado.")
  # 4.2. WORKER: executar uma origem inteira, sem IO ou dados do teste.
  trabalhar <- function(job) {
    inicio <- proc.time()[["elapsed"]]
    prefixo <- paste0("[PID ", Sys.getpid(), " | origem ", job$ano, "] ")
    cat(prefixo, "origem iniciada; uma GRU global.\n", sep = ""); flush(stdout())
    erro <- NULL
    resultado <- tryCatch({
      produtor <- get(".frota_gru_produtor", envir = globalenv(), inherits = FALSE)
      r <- produtor$calcular_origem_gru(job$config, job$treino_mensal)
      colunas <- c("Localidade", "Data", "Previsao_GRU")
      if (!is.list(r) || !is.data.frame(r$previsoes) || !nrow(r$previsoes) ||
          !all(colunas %in% names(r$previsoes))) stop("Calculo da origem nao retornou previsoes.")
      # Transportar somente dados R. Perdas, modelos, tensores e callbacks nao
      # sao enviados ao PAI nem usados para gravar historicos nesta validacao.
      list(previsoes = r$previsoes[colunas], avisos = as.character(r$avisos),
        localidades_treino = as.character(r$localidades_treino))
    }, error = function(e) {
      erro <<- conditionMessage(e)
      NULL
    })
    segundos <- unname(proc.time()[["elapsed"]] - inicio)
    cat(prefixo, if (is.null(erro)) "origem calculada" else paste0("origem falhou: ", erro),
      "; ", round(segundos, 2L), " s.\n", sep = ""); flush(stdout())
    list(ano = job$ano, resultado = resultado, erro = erro,
      segundos = segundos, pid = Sys.getpid())
  }
  environment(trabalhar) <- baseenv()
  # 4.3. Mesma tarefa nos dois modos: paraleloFALSE ainda usa um worker PSOCK.
  calculados <- parallel::parLapplyLB(cluster, jobs, trabalhar, chunk.size = 1L)
  # ---- FIM DA FILA PARALELA: SOMENTE O PAI COMPOE REAL E GRAVA CSV ------------

  # ---- 5. PAI: COMPOSICAO E ESCRITA, COM FALHAS ISOLADAS POR ORIGEM ------------
  resultados <- list()
  falhas <- character()
  for (calculado in calculados) {
    ano <- as.integer(calculado$ano)
    saida <- saidas[[as.character(ano)]]
    if (!is.null(calculado$erro)) {
      falhas <- c(falhas, paste0(ano, ": ", calculado$erro))
      message("Origem ", ano, " nao concluida; CSV anterior nao representa esta execucao: ", saida)
      next
    }
    resultado <- tryCatch({
      job <- jobs[[match(ano, anos)]]
      p <- periodos[[as.character(ano)]]
      if (length(calculado$resultado$avisos)) {
        for (aviso in unique(calculado$resultado$avisos)) message("[Origem ", ano, "] ", aviso)
      }
      pred <- calculado$resultado$previsoes
      pred$Data <- as.Date(pred$Data)
      if (anyNA(pred$Data) || anyNA(pred$Localidade) ||
          any(!nzchar(trimws(as.character(pred$Localidade)))) ||
          !is.numeric(pred$Previsao_GRU) || any(!is.finite(pred$Previsao_GRU)) ||
          any(!pred$Data %in% p$datas_teste) || anyDuplicated(pred[c("Localidade", "Data")])) {
        stop("Previsoes da origem nao correspondem ao horizonte solicitado.")
      }
      por_localidade <- split(pred$Data, pred$Localidade)
      if (any(vapply(por_localidade, length, integer(1L)) != p$meses_teste)) {
        stop("Previsoes incompletas em uma ou mais localidades da origem.")
      }
      # Compor no PAI recoloca o Real/cobertura da base observada e completa
      # o calendario. O worker devolveu apenas datas e Previsao_GRU do teste.
      csv <- produtor$compor_csv_gru(dados, pred, job$config)
      if (!is.data.frame(csv) || !nrow(csv) ||
          !all(c("Localidade", "Data", "Real", "Previsao_GRU") %in% names(csv)) ||
          anyNA(csv$Data) || any(format(csv$Data, "%d") != "01") ||
          !is.numeric(csv$Previsao_GRU) || !any(is.finite(csv$Previsao_GRU)) ||
          any(!is.na(csv$Previsao_GRU) & !is.finite(csv$Previsao_GRU)) ||
          anyDuplicated(csv[c("Localidade", "Data")]) ||
          any(csv$Data < p$inicio_treino | csv$Data > p$fim_teste) ||
          any(is.finite(csv$Previsao_GRU) & csv$Data < p$inicio_teste)) {
        stop("CSV composto nao corresponde ao contrato da origem.")
      }
      previstos_csv <- csv[is.finite(csv$Previsao_GRU), c("Localidade", "Data", "Previsao_GRU"), drop = FALSE]
      idx <- match(paste(pred$Localidade, pred$Data), paste(previstos_csv$Localidade, previstos_csv$Data))
      if (nrow(previstos_csv) != nrow(pred) || anyNA(idx) ||
          any(previstos_csv$Previsao_GRU[idx] != pred$Previsao_GRU)) {
        stop("Composicao do CSV alterou ou descartou previsoes da origem.")
      }
      produtor$gravar_csv_gru(csv, saida)
      if (!file.exists(saida)) stop("A gravacao nao produziu a saida solicitada: ", saida)
      nlocais <- length(unique(csv$Localidade))
      message("Origem ", ano, " concluida: ", saida, "; PID ", calculado$pid,
        "; ", length(calculado$resultado$localidades_treino), " identidades no treino global; ",
        nlocais, " localidades exportadas; ", round(calculado$segundos, 1L), " s de calculo.")
      data.frame(Ano_teste = ano, Inicio_treino = p$inicio_treino,
        Fim_treino = p$fim_treino, Inicio_teste = p$inicio_teste, Fim_teste = p$fim_teste,
        Meses_teste = p$meses_teste, Origem = "Novo ajuste GRU global causal",
        Localidades = nlocais, PID = calculado$pid, Arquivo = saida,
        Segundos = calculado$segundos, stringsAsFactors = FALSE)
    }, error = function(e) {
      falhas <<- c(falhas, paste0(ano, ": ", conditionMessage(e)))
      message("Origem ", ano, " nao concluida: ", conditionMessage(e),
        ". CSV anterior nao representa esta execucao: ", saida)
      NULL
    })
    if (!is.null(resultado)) resultados[[as.character(ano)]] <- resultado
  }
  # 5.1. Resumo apenas no console. Nunca contar CSV antigo de uma origem
  # fracassada como resultado novo; o arquivo anterior e preservado.
  if (!length(resultados)) stop("Nenhuma origem GRU concluida; consulte o console. CSVs anteriores preservados.")
  if (length(falhas)) warning(paste(falhas, collapse = "\n"), call. = FALSE)
  resumo <- do.call(rbind, resultados)
  rownames(resumo) <- NULL
  print(resumo, row.names = FALSE)
  invisible(resumo)
}

# ---- 6. ENTRADA LOCAL OU IMPORTACAO SOMENTE DAS FUNCOES ------------------------
if (!isTRUE(getOption("frota.validacao_gru.apenas_funcoes", FALSE))) executar_validacao_gru()
