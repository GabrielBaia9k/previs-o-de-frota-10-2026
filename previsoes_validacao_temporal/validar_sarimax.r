  # VALIDACAO-SARIMAX-3-CONFIG-COMUM
# Executar no R local, com a raiz do projeto como diretorio:
# source("previsoes_validacao_temporal/validar_sarimax.r", encoding = "UTF-8")
# Editar anos/localidades/destinos no bloco inicial de config_validacao_temporal.r.
# O controlador consulta essa configuracao e passa os recortes ao pre-compilador.
# Cada rodada gera somente seu CSV mensal; erros serao avaliados em runtime.
# Um unico ajuste por origem, sem incorporar Real durante o teste ate 2025.

config_validacao_sarimax <- local({
  caminho <- "previsoes_validacao_temporal/config_validacao_temporal.r"
  if (!file.exists(caminho)) stop("Arquivo necessario: ", caminho)
  comum <- new.env(parent = baseenv())
  source(caminho, local = comum, encoding = "UTF-8")
  if (!exists("configurar_validacao_temporal", envir = comum, inherits = FALSE)) {
    stop("Atualize ", caminho, " junto com o controlador: configuracao generica ausente.")
  }
  comum$configurar_validacao_temporal("SARIMAX")
})

preparar_reuso_sarimax_2025 <- function(caminho, dados, localidades, periodos, produtor) {
  if (!file.exists(caminho)) {
    stop("CSV de 2025 nao encontrado: ", caminho,
      ". Disponibilize a referencia existente ou use reutilizar_2025=FALSE para novo ajuste separado.")
  }
  referencia <- utils::read.csv(caminho, check.names = FALSE,
    fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  obrigatorias <- c("Localidade", "Data", "Real", "Previsao_SARIMAX")
  if (!all(obrigatorias %in% names(referencia))) stop("CSV de 2025 com esquema incompativel.")
  if (anyNA(referencia$Localidade)) stop("Localidade ausente no CSV de 2025.")
  referencia$Data <- as.Date(referencia$Data)
  for (nome in c("Real", "Previsao_SARIMAX")) {
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
  if (anyNA(referencia$Data)) stop("Datas invalidas no CSV de 2025.")
  chaves <- produtor$sarimax_chave_localidade(referencia$Localidade)
  resultados <- list()
  for (localidade in localidades) {
    parte <- referencia[chaves == produtor$sarimax_chave_localidade(localidade), , drop = FALSE]
    if (!nrow(parte)) {
      message(localidade, ": sem referencia de 2025; ignorada nesta rodada.")
      next
    }
    if (anyDuplicated(parte$Data)) stop("CSV de 2025 com datas duplicadas: ", localidade)
    if (any(parte$Data < periodos$inicio_treino & is.finite(parte$Real))) {
      stop("A referencia de 2025 de ", localidade,
        " usa treino anterior ao inicio escolhido. Mantenha o mesmo inicio ou desative o reuso.")
    }
    if (any(parte$Data < periodos$inicio_teste & is.finite(parte$Previsao_SARIMAX))) {
      stop("A referencia de 2025 nao corresponde ao recorte solicitado para ", localidade, ".")
    }
    esperado <- produtor$sarimax_serie_local(dados, localidade, periodos)
    idx <- match(esperado$Data, parte$Data)
    real_ref <- parte$Real[idx]
    conhecidos <- is.finite(esperado$Real) & is.finite(real_ref)
    if (any(is.finite(esperado$Real) != is.finite(real_ref)) ||
        any(esperado$Real[conhecidos] != real_ref[conhecidos])) {
      stop("Real do CSV de 2025 difere do CSV de entrada em ", localidade,
        ". Confira as versoes ou desative o reuso para novo ajuste separado.")
    }
    idx_teste <- match(periodos$datas_teste, parte$Data)
    if (anyNA(idx_teste) || any(!is.finite(parte$Previsao_SARIMAX[idx_teste]))) {
      message(localidade, ": referencia de 2025 sem todos os meses previstos; ignorada.")
      next
    }
    # Copia para a rodada somente historia/teste solicitados; a referencia
    # original, inclusive suas projecoes 2026--2031, nunca e sobrescrita.
    resultados[[localidade]] <- data.frame(Localidade = localidade,
      Data = esperado$Data, Real = real_ref,
      Previsao_SARIMAX = parte$Previsao_SARIMAX[idx],
      AIC = parte$AIC[idx], BIC = parte$BIC[idx], Modelo = "SARIMAX",
      stringsAsFactors = FALSE)
  }
  if (!length(resultados)) stop("Nenhuma referencia de 2025 compativel com as localidades escolhidas.")
  resultado <- do.call(rbind, resultados)
  rownames(resultado) <- NULL
  resultado
}

executar_validacao_sarimax <- function(config = config_validacao_sarimax) {
  if (!file.exists(config$script_recortes)) stop("Arquivo necessario: ", config$script_recortes)
  recortes <- new.env(parent = baseenv())
  source(config$script_recortes, local = recortes, encoding = "UTF-8")
  recortes$validar_arquivos_validacao(config)
  periodos <- recortes$definir_rodadas_validacao(config)
  anos <- as.integer(names(periodos))
  fim <- as.integer(config$ano_fim_teste)
  if (!is.logical(config$reutilizar_2025) || length(config$reutilizar_2025) != 1L ||
      is.na(config$reutilizar_2025)) stop("reutilizar_2025 deve ser TRUE ou FALSE.")
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop('Instale no R local: install.packages("dplyr", repos = "http://cran.rstudio.com/")')
  }
  produtor <- new.env(parent = globalenv())
  opcoes <- options(frota.sarimax.apenas_funcoes = TRUE)
  on.exit(options(opcoes), add = TRUE)
  source(config$script_pre_compilacao, local = produtor, encoding = "UTF-8")
  options(opcoes)
  dados <- produtor$ler_dados_sarimax(config$entrada)
  ultimo_real <- dados$Data[is.finite(dados$QTD_VEICULO)]
  fim_mensal <- as.Date(sprintf("%04d-12-01", as.integer(fim)))
  if (!length(ultimo_real) || max(ultimo_real) < fim_mensal) {
    stop("A entrada nao possui observacoes ate dezembro de ano_fim_teste. Confira o recorte.")
  }
  localidades <- produtor$sarimax_localidades(dados, config$localidades)
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    message("Rodada ", ano, ": treino ", p$inicio_treino, " a ", p$fim_treino,
      "; teste ", p$inicio_teste, " a ", p$fim_teste,
      " (", p$meses_teste, " meses sem reajuste).")
  }

  # Reuso opcional: somente o teste 2025 de 12 meses tem a origem da referencia.
  # As quatro janelas padrao 2021--2025 a 2024--2025 exigem novos ajustes.
  existente <- if (config$reutilizar_2025 && 2025L %in% anos && fim == 2025L) {
    preparar_reuso_sarimax_2025(config$referencia_2025, dados, localidades,
      periodos[["2025"]], produtor)
  } else NULL
  resultados <- list()
  falhas <- character()
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    nome <- recortes$nome_arquivo_validacao("SARIMAX", ano, fim)
    saida <- file.path(config$pasta_saida, nome)
    if (tolower(normalizePath(saida, winslash = "/", mustWork = FALSE)) ==
        tolower(normalizePath(config$referencia_2025, winslash = "/", mustWork = FALSE))) {
      stop("Saida da validacao nao pode substituir o CSV de producao de 2025.")
    }
    resultado <- tryCatch({
      if (ano == 2025L && !is.null(existente)) {
        produtor$gravar_csv_sarimax(existente, saida)
        origem <- "Referencia de 2025 reaproveitada; sem novo ajuste"
      } else {
        # O controlador faz o recorte e entrega somente treino+teste da rodada.
        # Dados posteriores ao fim de teste configurado nao chegam ao produtor.
        corte <- recortes$separar_treino_teste(dados, p)
        configuracao <- list(entrada = config$entrada, saida = saida,
          inicio_treino = p$inicio_treino, fim_treino = p$fim_treino,
          meses_teste = p$meses_teste, passos_futuro = 0L,
          localidades = localidades)
        produtor$executar_sarimax_local(configuracao,
          dados_basicos = rbind(corte$treino, corte$teste))
        origem <- "Novo ajuste do pre-compilador SARIMAX"
      }
      message("Rodada ", ano, " concluida: ", saida, " (", origem, ").")
      data.frame(Ano_teste = ano, Inicio_treino = p$inicio_treino,
        Fim_treino = p$fim_treino, Inicio_teste = p$inicio_teste,
        Fim_teste = p$fim_teste, Meses_teste = p$meses_teste, Origem = origem, Arquivo = saida,
        stringsAsFactors = FALSE)
    }, error = function(e) {
      falhas <<- c(falhas, paste0(ano, ": ", conditionMessage(e)))
      message("Rodada ", ano, " nao concluida: ", conditionMessage(e))
      NULL
    })
    if (!is.null(resultado)) resultados[[as.character(ano)]] <- resultado
  }
  if (!length(resultados)) stop("Nenhuma rodada concluida. Consulte o console.")
  if (length(falhas)) warning(paste(falhas, collapse = "\n"), call. = FALSE)
  resumo <- do.call(rbind, resultados)
  rownames(resumo) <- NULL
  print(resumo, row.names = FALSE)
  invisible(resumo)
}

if (!isTRUE(getOption("frota.validacao_sarimax.apenas_funcoes", FALSE))) {
  executar_validacao_sarimax()
}
