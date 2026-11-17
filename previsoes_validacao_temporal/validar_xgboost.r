# VALIDACAO-XGBOOST-3-PSOCK-6-LOCALIDADES
# Executar no R local, com a raiz do projeto como diretorio:
# source("previsoes_validacao_temporal/validar_xgboost.r", encoding = "UTF-8")
# Editar anos/localidades/destinos no bloco inicial de config_validacao_temporal.r.
# O controlador consulta essa configuracao e passa os recortes ao pre-compilador.
# Cada rodada gera somente seu CSV mensal; erros serao avaliados em runtime.
# Um unico ajuste por localidade e origem, sem incorporar Real durante o teste.
# Janelas em sequencia; ate seis localidades simultaneas por janela.
# Copiar tambem config_validacao_temporal.r e o produtor XGBOOST atualizado.
# Hiperparametros ficam no produtor; este arquivo apenas controla a validacao.
# Revisao de leitura (16/11/2026): blocos numerados; comportamento preservado.
# Este arquivo roda no PAI. O codigo dos WORKERS esta no pre-compilador.
# Fluxo: conferir dados -> conferir reuso -> criar cluster -> janelas -> encerrar.

# ---- 1. CONFIGURACAO: IMPORTAR O BLOCO CENTRALIZADO ----------------------------
config_validacao_xgboost <- local({
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  comum <- new.env(parent = baseenv())
  source(caminho, local = comum, encoding = "UTF-8")
  if (!exists("configurar_validacao_temporal", envir = comum, inherits = FALSE)) {
    stop("Atualize ", caminho, " junto com o controlador: configuracao generica ausente.")
  }
  comum$configurar_validacao_temporal("XGBOOST")
})

# ---- 2. PAI: CONFERIR A REFERENCIA ANUAL DE 2025 -------------------------------
# Somente le e prepara a copia. Nao treina modelos nem inicia workers.
preparar_reuso_xgboost_2025 <- function(caminho, dados, localidades, periodos, produtor) {
  if (!file.exists(caminho)) {
    stop("CSV de 2025 nao encontrado: ", caminho,
      ". Disponibilize a referencia existente ou use reutilizar_2025=FALSE para novo ajuste separado.")
  }
  referencia <- utils::read.csv(caminho, check.names = FALSE,
    fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  obrigatorias <- c("Localidade", "Data", "Real", "Previsao_XGBOOST")
  if (!all(obrigatorias %in% names(referencia))) stop("CSV de 2025 com esquema incompativel.")
  if (anyNA(referencia$Localidade)) stop("Localidade ausente no CSV de 2025.")
  referencia$Data <- as.Date(referencia$Data)
  for (nome in c("Real", "Previsao_XGBOOST")) {
    original <- as.character(referencia[[nome]])
    valor <- suppressWarnings(as.numeric(original))
    if (any(!is.na(original) & is.na(valor)) || any(!is.na(valor) & !is.finite(valor))) {
      stop("Valor invalido em ", nome, " no CSV de 2025.")
    }
    referencia[[nome]] <- valor
  }
  for (nome in c("AIC", "BIC")) {
    if (!nome %in% names(referencia)) referencia[[nome]] <- NA_real_
  }
  if (anyNA(referencia$Data) || any(format(referencia$Data, "%d") != "01")) {
    stop("Datas invalidas no CSV de 2025.")
  }
  chaves <- produtor$xgboost_chave_localidade(referencia$Localidade)
  resultados <- list()
  for (localidade in localidades) {
    parte <- referencia[chaves == produtor$xgboost_chave_localidade(localidade), , drop = FALSE]
    if (!nrow(parte)) {
      message(localidade, ": sem referencia de 2025; ignorada nesta rodada.")
      next
    }
    if (anyDuplicated(parte$Data)) stop("CSV de 2025 com datas duplicadas: ", localidade)
    if (any(parte$Data < periodos$inicio_treino & is.finite(parte$Real))) {
      stop("A referencia de 2025 de ", localidade,
        " usa treino anterior ao inicio escolhido. Mantenha o mesmo inicio ou desative o reuso.")
    }
    if (any(parte$Data < periodos$inicio_teste & is.finite(parte$Previsao_XGBOOST))) {
      stop("A referencia de 2025 nao corresponde ao recorte solicitado para ", localidade, ".")
    }
    esperado <- produtor$xgboost_serie_local(dados, localidade, periodos)
    idx <- match(esperado$Data, parte$Data)
    real_ref <- parte$Real[idx]
    conhecidos <- is.finite(esperado$Real) & is.finite(real_ref)
    if (any(is.finite(esperado$Real) != is.finite(real_ref)) ||
        any(esperado$Real[conhecidos] != real_ref[conhecidos])) {
      stop("Real do CSV de 2025 difere do CSV de entrada em ", localidade,
        ". Confira as versoes ou desative o reuso para novo ajuste separado.")
    }
    idx_teste <- match(periodos$datas_teste, parte$Data)
    if (anyNA(idx_teste) || any(!is.finite(parte$Previsao_XGBOOST[idx_teste]))) {
      message(localidade, ": referencia de 2025 sem todos os meses previstos; ignorada.")
      next
    }
    # Copia para a rodada somente historia/teste solicitados; a referencia
    # original, inclusive suas projecoes 2026--2031, nunca e sobrescrita.
    copia <- esperado
    copia$Real <- real_ref
    copia$Previsao_XGBOOST <- parte$Previsao_XGBOOST[idx]
    copia$AIC <- parte$AIC[idx]
    copia$BIC <- parte$BIC[idx]
    copia$Modelo <- "XGBOOST"
    copia$Particao <- ifelse(copia$Data < periodos$inicio_teste, "treino", "teste")
    resultados[[localidade]] <- copia
  }
  if (!length(resultados)) stop("Nenhuma referencia de 2025 compativel com as localidades escolhidas.")
  resultado <- do.call(rbind, resultados)
  rownames(resultado) <- NULL
  resultado
}

# ---- 3. PAI: COORDENAR TODA A VALIDACAO ---------------------------------------
executar_validacao_xgboost <- function(config = config_validacao_xgboost) {
  # 3.0. Registrar primeiro a limpeza e o TEMPO TOTAL. Este cronometro inclui
  # leitura, verificacao do reuso, criacao do cluster, rodadas, CSVs e encerramento.
  inicio_total <- proc.time()[["elapsed"]]
  cluster <- NULL
  opcoes <- NULL
  on.exit({
    if (!is.null(cluster)) {
      message("Encerrando os processos XGBOOST...")
      tryCatch(parallel::stopCluster(cluster), error = function(e) {
        message("Falha ao encerrar o cluster: ", conditionMessage(e))
      })
    }
    if (!is.null(opcoes)) options(opcoes)
    segundos <- proc.time()[["elapsed"]] - inicio_total
    inteiros <- floor(segundos)
    duracao <- sprintf("%02.0f:%02.0f:%02.0f", inteiros %/% 3600,
      (inteiros %% 3600) %/% 60, inteiros %% 60)
    message("TEMPO TOTAL DA VALIDACAO XGBOOST: ", duracao,
      " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  # 3.1. Conferir auxiliar, recortes e opcoes de execucao.
  if (!file.exists(config$script_recortes)) stop("Arquivo necessario: ", config$script_recortes)
  recortes <- new.env(parent = baseenv())
  source(config$script_recortes, local = recortes, encoding = "UTF-8")
  recortes$validar_arquivos_validacao(config)
  periodos <- recortes$definir_rodadas_validacao(config)
  anos <- as.integer(names(periodos))
  fim <- as.integer(config$ano_fim_teste)
  if (!is.logical(config$reutilizar_2025) || length(config$reutilizar_2025) != 1L ||
      is.na(config$reutilizar_2025)) stop("reutilizar_2025 deve ser TRUE ou FALSE.")
  if (!is.logical(config$paralelo) || length(config$paralelo) != 1L ||
      is.na(config$paralelo)) stop("paralelo deve ser TRUE ou FALSE.")
  if (!is.numeric(config$processos) || length(config$processos) != 1L ||
      !is.finite(config$processos) || config$processos < 1 || config$processos > .Machine$integer.max ||
      config$processos != floor(config$processos)) stop("processos deve ser um inteiro positivo.")
  # 3.2. Importar funcoes do produtor sem disparar sua previsao de producao.
  produtor <- new.env(parent = globalenv())
  opcoes <- options(frota.xgboost.apenas_funcoes = TRUE, OutDec = ".")
  source(config$script_pre_compilacao, local = produtor, encoding = "UTF-8")
  options(opcoes)
  options(OutDec = ".")
  if (!is.function(produtor$criar_cluster_xgboost) ||
      !is.function(produtor$executar_xgboost_local) ||
      !all(c("dados_mensais", "cluster") %in%
        names(formals(produtor$executar_xgboost_local)))) {
    stop("Atualize ", config$script_pre_compilacao,
      " junto com o controlador: produtor sem suporte ao paralelismo por localidade.")
  }
  # 3.3. Ler a base uma vez e definir todas as localidades da execucao.
  configuracao_base <- utils::modifyList(produtor$config_xgboost_local, list(
    entrada = config$entrada,
    ano_inicio = as.integer(format(config$inicio_treino, "%Y")), ano_fim = fim))
  # Agregar todos os municipios antes de filtrar localidades: a soma estadual
  # observada nao pode depender dos municipios escolhidos para processar.
  dados <- produtor$ler_mensal_xgboost(configuracao_base)
  obrigatorias <- c("Localidade", "Data", "Real", "Municipios_com_registro",
    "Municipios_com_total_conhecido", "Municipios_esperados")
  if (!is.data.frame(dados) || !all(obrigatorias %in% names(dados))) {
    stop("Pre-compilador nao retornou series mensais com cobertura.")
  }
  dados$Data <- as.Date(dados$Data)
  if (anyNA(dados$Data) || anyNA(dados$Localidade) ||
      any(!nzchar(as.character(dados$Localidade))) ||
      any(format(dados$Data, "%d") != "01") || !is.numeric(dados$Real) ||
      any(!is.na(dados$Real) & !is.finite(dados$Real)) ||
      anyDuplicated(dados[c("Localidade", "Data")])) stop("Series mensais invalidas.")
  ultimo_real <- dados$Data[is.finite(dados$Real)]
  fim_mensal <- as.Date(sprintf("%04d-12-01", fim))
  if (!length(ultimo_real) || max(ultimo_real) < fim_mensal) {
    stop("A entrada nao possui observacoes ate dezembro de ano_fim_teste. Confira o recorte.")
  }
  localidades <- produtor$xgboost_localidades(dados, config$localidades)
  # 3.4. Conferir todos os destinos antes de iniciar qualquer ajuste ou gravacao.
  saidas <- stats::setNames(vapply(anos, function(ano) {
    file.path(config$pasta_saida, recortes$nome_arquivo_validacao("XGBOOST", ano, fim))
  }, character(1L)), as.character(anos))
  canonico <- function(x) tolower(normalizePath(x, winslash = "/", mustWork = FALSE))
  protegidos <- unique(c(config$entrada, config$referencia_2025,
    produtor$config_xgboost_local$saida))
  if (anyDuplicated(vapply(saidas, canonico, character(1L))) ||
      any(vapply(saidas, canonico, character(1L)) %in%
        vapply(protegidos, canonico, character(1L)))) {
    stop("Saida da validacao nao pode substituir a entrada, a producao ou outra rodada.")
  }
  # 3.5. Exibir o calendario das janelas configuradas (padrao: cinco).
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    message("Rodada ", ano, ": treino ", p$inicio_treino, " a ", p$fim_treino,
      "; teste ", p$inicio_teste, " a ", p$fim_teste,
      " (", p$meses_teste, " meses sem reajuste).")
  }

  # 3.6. Preparar o reuso anual antes de iniciar o cluster.
  # Reuso opcional: somente o teste 2025 de 12 meses tem a origem da referencia.
  # As quatro janelas padrao 2021--2025 a 2024--2025 exigem novos ajustes.
  existente <- if (config$reutilizar_2025 && 2025L %in% anos && fim == 2025L) {
    preparar_reuso_xgboost_2025(config$referencia_2025, dados, localidades,
      periodos[["2025"]], produtor)
  } else NULL
  # ---- INICIO DO CICLO DE VIDA DO CLUSTER ----------------------------------
  # 3.7. Criar o cluster uma unica vez, somente se houver ajustes novos.
  # Um unico cluster compartilhado entre as janelas. Somente o processo pai
  # grava os CSVs; os workers calculam localidades e devolvem seus resultados.
  processos <- if (config$paralelo) min(as.integer(config$processos), length(localidades)) else 1L
  novas <- anos[!(anos == 2025L & !is.null(existente))]
  if (length(novas) && processos > 1L) {
    cluster <- produtor$criar_cluster_xgboost(processos,
      config$script_pre_compilacao, raiz = getwd())
    # 3.8. O on.exit do bloco 3.0 ja possui este cluster: vai encerra-lo ao
    # sair, inclusive por erro/interrupcao, antes de imprimir o tempo total.
  }
  message("Validacao XGBOOST: ", length(localidades), " localidades; ",
    if (is.null(cluster)) 1L else length(cluster), " processo(s) de calculo; janelas em sequencia.")
  # 3.9. Executar JANELAS EM SEQUENCIA, reutilizando o mesmo cluster.
  # O paralelismo entre LOCALIDADES ocorre no pre-compilador, com fila dinamica.
  resultados <- list()
  falhas <- character()
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    saida <- saidas[[as.character(ano)]]
    inicio_rodada <- proc.time()[["elapsed"]]
    resultado <- tryCatch({
      # 3.9.a. Janela anual reaproveitada: copiar sem iniciar novos ajustes.
      if (ano == 2025L && !is.null(existente)) {
        produtor$gravar_csv_xgboost(existente, saida)
        origem <- "Referencia de 2025 reaproveitada; sem novo ajuste"
      } else {
        # 3.9.b. Nova projecao: recortar e passar a rodada ao pre-compilador.
        # O controlador faz o recorte e entrega somente treino+teste da rodada.
        # Dados posteriores ao fim de teste configurado nao chegam ao produtor.
        corte <- recortes$separar_treino_teste(dados, p)
        configuracao <- utils::modifyList(configuracao_base, list(
          saida = saida, inicio_treino = p$inicio_treino, fim_treino = p$fim_treino,
          ano_teste = ano, n_teste = p$meses_teste, meses_teste = p$meses_teste,
          passos_futuro = 0L, localidades = localidades))
        produtor$executar_xgboost_local(configuracao,
          dados_mensais = rbind(corte$treino, corte$teste), cluster = cluster)
        origem <- "Novo ajuste do pre-compilador XGBOOST"
      }
      if (!file.exists(saida)) stop("O pre-compilador nao gravou a saida solicitada: ", saida)
      segundos <- proc.time()[["elapsed"]] - inicio_rodada
      message("Rodada ", ano, " concluida: ", saida, " (", origem,
        "; ", round(segundos, 1L), " s).")
      data.frame(Ano_teste = ano, Inicio_treino = p$inicio_treino,
        Fim_treino = p$fim_treino, Inicio_teste = p$inicio_teste,
        Fim_teste = p$fim_teste, Meses_teste = p$meses_teste, Origem = origem,
        Arquivo = saida, Segundos = segundos,
        stringsAsFactors = FALSE)
    }, error = function(e) {
      # 3.9.c. Registrar falha da janela e permitir o processamento das demais.
      falhas <<- c(falhas, paste0(ano, ": ", conditionMessage(e)))
      message("Rodada ", ano, " nao concluida: ", conditionMessage(e))
      NULL
    })
    if (!is.null(resultado)) resultados[[as.character(ano)]] <- resultado
  }
  # 3.10. Apresentar resumo somente no console. Ao retornar ou falhar, o
  # on.exit de 3.0 encerra o cluster, restaura opcoes e imprime o TEMPO TOTAL.
  if (!length(resultados)) stop("Nenhuma rodada concluida. Consulte o console.")
  if (length(falhas)) warning(paste(falhas, collapse = "\n"), call. = FALSE)
  resumo <- do.call(rbind, resultados)
  rownames(resumo) <- NULL
  print(resumo, row.names = FALSE)
  invisible(resumo)
}
# ---- FIM DO CICLO DE VIDA: LIMPEZA REGISTRADA NO CONTROLADOR -------------------

# ---- 4. ENTRADA: EXECUTAR VALIDACAO OU CARREGAR SOMENTE AS FUNCOES --------------
if (!isTRUE(getOption("frota.validacao_xgboost.apenas_funcoes", FALSE))) {
  executar_validacao_xgboost()
}
