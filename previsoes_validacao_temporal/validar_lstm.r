# VALIDACAO-LSTM-1-PSOCK-6-CAUSAL
# Executar somente no R local, na raiz do projeto Windows/VS Code:
# source("previsoes_validacao_temporal/validar_lstm.r", encoding = "UTF-8")
# Copiar tambem config_validacao_temporal.r e pre_compilar_LSTM.r revisao3.
# Configuracao comum de anos/localidades/processos: bloco inicial do auxiliar.
# Hiperparametros/preparacao pertencem ao produtor. Este controlador define
# recortes, coordena um cluster reutilizado e grava um CSV por origem no PAI.
# Cinco janelas em sequencia; seis localidades calculadas simultaneamente.
# Teste continuo ate dezembro2025, sem Real de teste nos ajustes/recursao.
# Nao exportar perdas/gradientes ou metricas de erro nesta validacao. Os
# historicos e previsoes de producao permanecem intactos; erros no frontend.
# A origem2025 tambem e recalculada: reuso do CSV legado nao e autorizado
# nesta revisao, pois os valores de Real nao demonstram o protocolo do ajuste.
# Blocos numerados seguem o template SARIMAX/Prophet. Nao executar na nuvem.

# ---- 1. CONFIGURACAO: REUTILIZAR O AUXILIAR COMPARTILHADO ----------------------
config_validacao_lstm <- local({
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  comum <- new.env(parent = baseenv())
  source(caminho, local = comum, encoding = "UTF-8")
  if (!is.function(comum$configurar_validacao_temporal)) stop("Atualize o auxiliar de configuracao: ", caminho)
  comum$configurar_validacao_temporal("LSTM")
})

# ---- 2. COMPATIBILIDADE: CONFERIR ANTES DE SOURCE/PYTHON/TREINAMENTO -----------
vll_carregar_produtor <- function(caminho) {
  # O LSTM legado executava imediatamente e nao respeitava apenas_funcoes.
  # Parse/AST precedem source para recusar essa copia antes de qualquer efeito.
  expressoes <- as.list(parse(file = caminho, encoding = "UTF-8", keep.source = FALSE))
  atribuicoes <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
    is.symbol(x[[2L]]), expressoes)
  definicoes <- stats::setNames(lapply(atribuicoes, function(x) x[[3L]]),
    vapply(atribuicoes, function(x) as.character(x[[2L]]), character(1L)))
  revisao <- "LSTM-LOCAL-3-CAUSAL-VALIDACAO"
  obrigatorias <- c("ler_mensal_lstm", "lstm_validar_mensal", "lstm_localidades",
    "lstm_periodos", "resolver_python_lstm", "criar_cluster_lstm", "executar_lstm_local")
  funcoes <- vapply(obrigatorias, function(n) {
    x <- definicoes[[n]]
    is.call(x) && identical(x[[1L]], as.name("function"))
  }, logical(1L))
  entrada <- quote(if (!isTRUE(getOption("frota.lstm.apenas_funcoes", FALSE))) executar_lstm_local())
  possui_guarda <- any(vapply(expressoes, function(x) identical(x, entrada), logical(1L)))
  config_ast <- definicoes[["config_lstm_local"]]
  flag <- is.call(config_ast) && identical(config_ast[[1L]], as.name("list")) &&
    "salvar_historicos" %in% names(as.list(config_ast))
  if (!identical(definicoes[["lstm_versao"]], revisao) || !all(funcoes) || !possui_guarda || !flag) {
    stop("Substitua ", caminho, " pela revisao ", revisao,
      "; produtor sem interface causal de validacao/controle de historicos. ",
      "Nenhum script do produtor foi executado.", call. = FALSE)
  }
  parametros <- names(definicoes[["executar_lstm_local"]][[2L]])
  if (!all(c("config", "dados_mensais", "cluster") %in% parametros)) {
    stop("Pre-compilador LSTM sem API config/dados_mensais/cluster; nenhum ajuste iniciado.")
  }
  opcoes <- options(frota.lstm.apenas_funcoes = TRUE)
  on.exit(options(opcoes), add = TRUE)
  produtor <- new.env(parent = globalenv())
  source(caminho, local = produtor, encoding = "UTF-8")
  if (!identical(produtor$lstm_versao, revisao) || !is.list(produtor$config_lstm_local) ||
      !all(vapply(obrigatorias, function(n) is.function(produtor[[n]]), logical(1L)))) {
    stop("Pre-compilador LSTM incompativel apos importacao; nenhum ajuste iniciado.")
  }
  produtor
}

# ---- 3. PAI: COORDENAR AS CINCO JANELAS, UM CLUSTER POR EXECUCAO ---------------
executar_validacao_lstm <- function(config = config_validacao_lstm) {
  inicio_total <- proc.time()[["elapsed"]]
  cluster <- NULL
  opcoes <- options(OutDec = ".")
  on.exit({
    if (!is.null(cluster)) {
      message("Encerrando os processos LSTM...")
      tryCatch(parallel::stopCluster(cluster), error = function(e) message("Falha ao encerrar cluster: ", conditionMessage(e)))
    }
    options(opcoes)
    segundos <- proc.time()[["elapsed"]] - inicio_total
    n <- floor(segundos)
    message("TEMPO TOTAL DA VALIDACAO LSTM: ",
      sprintf("%02.0f:%02.0f:%02.0f", n %/% 3600, (n %% 3600) %/% 60, n %% 60),
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)

  # 3.1. Validar configuracao e definir recortes com as funcoes compartilhadas.
  if (!file.exists(config$script_recortes)) stop("Arquivo necessario: ", config$script_recortes)
  recortes <- new.env(parent = baseenv())
  source(config$script_recortes, local = recortes, encoding = "UTF-8")
  recortes$validar_arquivos_validacao(config)
  periodos <- recortes$definir_rodadas_validacao(config)
  anos <- as.integer(names(periodos))
  fim <- as.integer(config$ano_fim_teste)
  if (!is.logical(config$reutilizar_2025) || length(config$reutilizar_2025) != 1L ||
      is.na(config$reutilizar_2025)) stop("reutilizar_2025 deve ser TRUE ou FALSE.")
  if (config$reutilizar_2025) stop("Mantenha reutilizar_2025=FALSE para LSTM: esta revisao recalcula ",
    "a origem2025 pelo protocolo causal; o CSV antigo nao comprova esse protocolo.")
  if (!is.logical(config$paralelo) || length(config$paralelo) != 1L || is.na(config$paralelo)) {
    stop("paralelo deve ser TRUE ou FALSE.")
  }
  if (!is.numeric(config$processos) || length(config$processos) != 1L ||
      !is.finite(config$processos) || config$processos < 1 ||
      config$processos > .Machine$integer.max || config$processos != floor(config$processos)) {
    stop("processos deve ser um inteiro positivo.")
  }
  produtor <- vll_carregar_produtor(config$script_pre_compilacao)
  message("Produtor: ", produtor$lstm_versao, "; validacao sem exportar historicos e sem reuso do CSV antigo.")

  # 3.2. Ler/agregar uma vez cobrindo todo o periodo observado configurado.
  # O leitor LSTM usa DATAS e meses_teste, nao ano_inicio/ano_fim do Prophet.
  # Definir a origem de leitura mais antiga e teste ate o ultimo dezembro.
  leitura <- utils::modifyList(produtor$config_lstm_local, list(
    entrada = config$entrada, inicio_treino = config$inicio_treino,
    fim_treino = periodos[[1L]]$fim_treino,
    meses_teste = periodos[[1L]]$meses_teste, passos_futuro = 0L,
    salvar_historicos = FALSE, script_pre_compilacao = config$script_pre_compilacao), keep.null = TRUE)
  # Se houver Python explicito no auxiliar, usa-lo; caso contrario preservar
  # a selecao do produtor. Resolver antes de criar os workers emprestados.
  if ("python" %in% names(config)) leitura$python <- config$python
  leitura$python <- produtor$resolver_python_lstm(leitura)
  dados <- produtor$lstm_validar_mensal(produtor$ler_mensal_lstm(leitura))
  dados <- dados[dados$Data >= config$inicio_treino &
    dados$Data <= as.Date(sprintf("%04d-12-01", fim)), , drop = FALSE]
  conhecidos <- dados$Data[is.finite(dados$Real)]
  if (!length(conhecidos) || max(conhecidos) < as.Date(sprintf("%04d-12-01", fim))) {
    stop("A entrada nao possui observacoes ate dezembro de ano_fim_teste. Confira o recorte.")
  }
  # Filtrar depois da agregacao protege a soma estadual observada.
  localidades <- produtor$lstm_localidades(dados, config$localidades)

  # 3.3. Conferir todos os destinos antes de iniciar workers/ajustes/gravacoes.
  saidas <- stats::setNames(vapply(anos, function(ano) file.path(config$pasta_saida,
    recortes$nome_arquivo_validacao("LSTM", ano, fim)), character(1L)), as.character(anos))
  canonico <- function(x) {
    x <- path.expand(x)
    if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", x)) x <- file.path(getwd(), x)
    tolower(normalizePath(x, winslash = "/", mustWork = FALSE))
  }
  protegidos <- unique(c(config$entrada, config$script_recortes, config$script_pre_compilacao,
    config$referencia_2025, unlist(produtor$config_lstm_local[c("saida", "historico_perdas", "historico_gradientes")], use.names = FALSE)))
  destinos <- vapply(saidas, canonico, character(1L))
  if (anyDuplicated(destinos) || any(destinos %in% vapply(protegidos, canonico, character(1L)))) {
    stop("Saida da validacao nao pode substituir entrada, producao, historicos, scripts ou outra rodada.")
  }
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    message("Rodada ", ano, ": treino ", p$inicio_treino, " a ", p$fim_treino,
      "; teste ", p$inicio_teste, " a ", p$fim_teste, " (", p$meses_teste, " meses sem reajuste).")
  }

  # ---- INICIO DO CICLO DE VIDA DO CLUSTER ----------------------------------
  # 3.4. Criar uma unica vez. Os workers recebem o mesmo Python do produtor.
  processos <- if (config$paralelo) min(as.integer(config$processos), length(localidades)) else 1L
  if (processos > 1L) cluster <- produtor$criar_cluster_lstm(processos,
    config$script_pre_compilacao, raiz = getwd(), python = leitura$python)
  message("Validacao LSTM: ", length(localidades), " localidades; ",
    if (is.null(cluster)) 1L else length(cluster), " processo(s); janelas em sequencia; historicos de producao preservados.")

  # 3.5. JANELAS EM SEQUENCIA. Paralelismo por localidade dentro do produtor.
  resultados <- list(); falhas <- character()
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    saida <- saidas[[as.character(ano)]]
    inicio_rodada <- proc.time()[["elapsed"]]
    resultado <- tryCatch({
      corte <- recortes$separar_treino_teste(dados, p)
      configuracao <- utils::modifyList(leitura, list(
        saida = saida, inicio_treino = p$inicio_treino, fim_treino = p$fim_treino,
        meses_teste = p$meses_teste, passos_futuro = 0L, localidades = localidades,
        paralelo = config$paralelo, processos = as.integer(config$processos),
        salvar_historicos = FALSE), keep.null = TRUE)
      gerado <- produtor$executar_lstm_local(configuracao,
        dados_mensais = rbind(corte$treino, corte$teste), cluster = cluster)
      if (!file.exists(saida)) stop("O pre-compilador nao gravou a saida solicitada: ", saida)
      segundos <- proc.time()[["elapsed"]] - inicio_rodada
      nlocais <- length(unique(gerado$previsoes$Localidade))
      message("Rodada ", ano, " concluida: ", saida, "; ", nlocais,
        "/", length(localidades), " localidades; ", round(segundos, 1L), " s.")
      data.frame(Ano_teste = ano, Inicio_treino = p$inicio_treino,
        Fim_treino = p$fim_treino, Inicio_teste = p$inicio_teste,
        Fim_teste = p$fim_teste, Meses_teste = p$meses_teste,
        Origem = "Novo ajuste do pre-compilador LSTM causal", Localidades = nlocais,
        Arquivo = saida, Segundos = segundos, stringsAsFactors = FALSE)
    }, error = function(e) {
      falhas <<- c(falhas, paste0(ano, ": ", conditionMessage(e)))
      message("Rodada ", ano, " nao concluida: ", conditionMessage(e))
      NULL
    })
    if (!is.null(resultado)) resultados[[as.character(ano)]] <- resultado
  }
  # 3.6. Resumo no console. on.exit encerra o cluster antes do tempo total.
  if (!length(resultados)) stop("Nenhuma rodada concluida. Consulte o console.")
  if (length(falhas)) warning(paste(falhas, collapse = "\n"), call. = FALSE)
  resumo <- do.call(rbind, resultados); rownames(resumo) <- NULL
  print(resumo, row.names = FALSE)
  invisible(resumo)
}
# ---- FIM DO CICLO DE VIDA: LIMPEZA NO ON.EXIT DO CONTROLADOR ------------------

# ---- 4. ENTRADA LOCAL OU IMPORTACAO SOMENTE DAS FUNCOES ------------------------
if (!isTRUE(getOption("frota.validacao_lstm.apenas_funcoes", FALSE))) executar_validacao_lstm()
