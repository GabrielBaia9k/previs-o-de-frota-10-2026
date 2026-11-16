# VALIDACAO-XGBOOST-2-CONFIG-COMUM
# Executar no R local, com a raiz do projeto como diretorio de trabalho:
# source("previsoes_validacao_temporal/validar_xgboost.r", encoding = "UTF-8")
# O controlador define os recortes e chama o pre-compilador; nao modela os dados.
# Editar anos/localidades/destinos no bloco inicial de config_validacao_temporal.r.
# Dependencias: config_validacao_temporal.r e pre_compilar_xgboost.r adaptado.
# A revisao publicada XGB-CRAN-LOCAL-ESTADO-4-DIF1 ainda exige essa adaptacao.
# Contrato necessario: executar_xgboost_local(config, dados_mensais = NULL),
# usando dados_mensais ja agregados, localidades selecionadas e os recortes
# recebidos. Deve separar o treino antes de diferenciar/montar lags, aceitar
# n_teste de varios anos e passos_futuro=0, e respeitar config$saida.
# Nao executar o pre-compilador automaticamente ao carregar suas funcoes.

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

vtx_carregar_produtor <- function(caminho) {
  # A opcao ja existe no produtor publicado e impede o ajuste de producao
  # durante source(). Restaurar a opcao anterior inclusive em caso de erro.
  opcoes <- options(frota.xgboost.apenas_funcoes = TRUE)
  on.exit(options(opcoes), add = TRUE)
  produtor <- new.env(parent = globalenv())
  source(caminho, local = produtor, encoding = "UTF-8")
  objetos <- c("config_xgboost_local", "ler_mensal_xgboost", "executar_xgboost_local")
  faltantes <- objetos[!vapply(objetos, exists, logical(1L),
    envir = produtor, inherits = FALSE)]
  if (length(faltantes)) stop("Pre-compilador sem: ", paste(faltantes, collapse = ", "))
  if (!is.list(produtor$config_xgboost_local) ||
      !is.function(produtor$ler_mensal_xgboost) ||
      !is.function(produtor$executar_xgboost_local)) stop("Pre-compilador XGBoost incompativel.")
  if (!all(c("config", "dados_mensais") %in%
      names(formals(produtor$executar_xgboost_local)))) {
    stop("O pre-compilador XGBoost ainda nao foi adaptado para validacao temporal. ",
      "Atualize ", caminho, " para executar_xgboost_local(config, dados_mensais = NULL) ",
      "e para receber os recortes, localidades e passos_futuro=0. Nenhum ajuste foi iniciado.")
  }
  produtor
}

vtx_selecionar_localidades <- function(disponiveis, selecionadas) {
  chave <- function(x) {
    x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC",
      toupper(trimws(as.character(x))))
    x <- trimws(gsub("[^A-Z0-9]+", " ", x))
    x[x %in% c("PARA", "PARA GERAL")] <- "PARA GERAL"
    x
  }
  disponiveis <- unique(as.character(disponiveis))
  if (is.null(selecionadas)) return(disponiveis)
  if (!length(selecionadas) || anyNA(selecionadas)) stop("Selecione ao menos uma localidade.")
  idx <- match(chave(selecionadas), chave(disponiveis))
  if (anyNA(idx)) stop("Localidades nao encontradas: ", paste(selecionadas[is.na(idx)], collapse = ", "))
  unique(disponiveis[idx])
}

executar_validacao_xgboost <- function(config = config_validacao_xgboost) {
  if (!file.exists(config$script_recortes)) stop("Arquivo necessario: ", config$script_recortes)
  recortes <- new.env(parent = baseenv())
  source(config$script_recortes, local = recortes, encoding = "UTF-8")
  recortes$validar_arquivos_validacao(config)
  periodos <- recortes$definir_rodadas_validacao(config)
  anos <- as.integer(names(periodos))
  fim <- as.integer(config$ano_fim_teste)

  # Hiperparametros, lags e diferenciacao pertencem ao pre-compilador.
  # Ler/agregar uma vez, sem filtrar municipios antes da soma estadual.
  produtor <- vtx_carregar_produtor(config$script_pre_compilacao)
  opcoes <- options(OutDec = ".")
  on.exit(options(opcoes), add = TRUE)
  configuracao_base <- utils::modifyList(produtor$config_xgboost_local, list(
    entrada = config$entrada,
    ano_inicio = as.integer(format(config$inicio_treino, "%Y")),
    ano_fim = as.integer(fim)
  ))
  mensal <- produtor$ler_mensal_xgboost(configuracao_base)
  if (!is.data.frame(mensal) || !all(c("Localidade", "Data", "Real") %in% names(mensal))) {
    stop("ler_mensal_xgboost deve retornar Localidade, Data e Real.")
  }
  mensal$Data <- as.Date(mensal$Data)
  if (anyNA(mensal$Data) || anyNA(mensal$Localidade) ||
      any(!nzchar(as.character(mensal$Localidade))) || !is.numeric(mensal$Real) ||
      any(format(mensal$Data, "%d") != "01") ||
      any(!is.na(mensal$Real) & !is.finite(mensal$Real)) ||
      anyDuplicated(mensal[c("Localidade", "Data")])) stop("Series mensais invalidas no pre-compilador.")
  fim_mensal <- as.Date(sprintf("%04d-12-01", as.integer(fim)))
  mensal <- mensal[mensal$Data >= config$inicio_treino & mensal$Data <= fim_mensal, , drop = FALSE]
  conhecidos <- mensal$Data[is.finite(mensal$Real)]
  if (!length(conhecidos) || max(conhecidos) < fim_mensal) {
    stop("A entrada nao possui valores reais conhecidos ate dezembro de ano_fim_teste.")
  }
  localidades <- vtx_selecionar_localidades(mensal$Localidade, config$localidades)
  mensal <- mensal[mensal$Localidade %in% localidades, , drop = FALSE]
  protegidos <- unique(c(config$entrada, produtor$config_xgboost_local$saida,
    "Previsoes_output/previsoes_xgboost_pre_comp.csv"))
  canonico <- function(x) tolower(normalizePath(x, winslash = "/", mustWork = FALSE))
  resultados <- list()
  falhas <- character()
  for (ano in anos) {
    p <- periodos[[as.character(ano)]]
    nome <- recortes$nome_arquivo_validacao("XGBOOST", ano, fim)
    saida <- file.path(config$pasta_saida, nome)
    if (canonico(saida) %in% vapply(protegidos, canonico, character(1L))) {
      stop("Saida da validacao nao pode substituir a entrada ou o CSV de producao.")
    }
    message("Rodada ", ano, "--", as.integer(fim), ": treino ", p$inicio_treino,
      " a ", p$fim_treino, "; ", p$meses_teste, " meses previstos sem reajuste.")
    resultado <- tryCatch({
      corte <- recortes$separar_treino_teste(mensal, p)
      configuracao <- utils::modifyList(configuracao_base, list(
        saida = saida, inicio_treino = p$inicio_treino, fim_treino = p$fim_treino,
        ano_teste = as.integer(ano), n_teste = p$meses_teste,
        meses_teste = p$meses_teste, passos_futuro = 0L, localidades = localidades
      ))
      produtor$executar_xgboost_local(config = configuracao,
        dados_mensais = rbind(corte$treino, corte$teste))
      if (!file.exists(saida)) stop("O pre-compilador nao gravou a saida solicitada: ", saida)
      message("Rodada concluida: ", saida)
      data.frame(Ano_teste = ano, Inicio_treino = p$inicio_treino,
        Fim_treino = p$fim_treino, Inicio_teste = p$inicio_teste,
        Fim_teste = p$fim_teste, Meses_teste = p$meses_teste,
        Arquivo = saida, stringsAsFactors = FALSE)
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
  print(resumo, row.names = FALSE) # Resumo no console; nao exporta metricas/relatorios.
  invisible(resumo)
}

if (!isTRUE(getOption("frota.validacao_xgboost.apenas_funcoes", FALSE))) {
  executar_validacao_xgboost()
}
