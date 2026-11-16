# source("previsoes_pre_compilacao/pre_compilar_sarimax.r")

# SARIMAX-LOCAL-2-RECORTES
# Base: GitHub publicacao-codigo, 4135192d317b5e1d8d56ae8e6bc4fd6f017a5b22.
# Recebe recorte, localidades, horizonte e saida do controlador temporal.
# Executar apenas na maquina local, a partir da raiz do projeto.
# Dependencia do projeto: previsoes_validacao_temporal/config_validacao_temporal.r.
# Pacotes locais, se necessarios:
# install.packages(c("dplyr", "forecast"), repos = "http://cran.rstudio.com/")
# Source direto conserva o padrao publicado: treino 2015--2024, teste 2025,
# futuro 2026--2031 e CSV de producao. O controlador carrega apenas as funcoes.

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

executar_sarimax_local <- function(config = config_sarimax_local, dados_basicos = NULL) {
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
  on.exit(options(opcoes), add = TRUE)
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
  futuro <- if (config$passos_futuro > 0L) {
    seq(as.Date(format(periodos$fim_teste + 1L, "%Y-%m-01")), by = "month",
      length.out = as.integer(config$passos_futuro))
  } else as.Date(character())
  resultados <- list()
  message("SARIMAX: treino ", periodos$inicio_treino, " a ", periodos$fim_treino,
    "; teste ", periodos$inicio_teste, " a ", periodos$fim_teste, ".")

  for (localidade in localidades) {
    resultado <- tryCatch({
      historico <- sarimax_serie_local(dados_basicos, localidade, periodos)
      treino <- historico[historico$Data <= periodos$fim_treino, , drop = FALSE]
      if (sum(is.finite(treino$Real)) < config$minimo_treino) {
        stop("menos de ", config$minimo_treino, " meses conhecidos no treino.")
      }
      # NA internos permanecem na serie regular; o pacote trata observacoes
      # ausentes na estimacao. Nao compactar datas nem preencher com zeros.
      primeira <- which(is.finite(treino$Real))[1L]
      treino <- treino[seq.int(primeira, nrow(treino)), , drop = FALSE]
      serie <- stats::ts(treino$Real,
        start = c(as.integer(format(treino$Data[1L], "%Y")),
          as.integer(format(treino$Data[1L], "%m"))), frequency = 12)
      datas_previsao <- c(periodos$datas_teste, futuro)
      dummy <- function(datas) as.numeric(datas >= as.Date("2013-01-01") &
        datas <= as.Date("2017-12-31"))
      ajuste <- ajustar_prever_sarimax(serie, dummy(treino$Data),
        dummy(datas_previsao), length(datas_previsao))
      saida <- data.frame(Localidade = localidade,
        Data = c(historico$Data, futuro),
        Real = c(historico$Real, rep(NA_real_, length(futuro))),
        Previsao_SARIMAX = NA_real_, AIC = ajuste$AIC, BIC = ajuste$BIC,
        Modelo = "SARIMAX", stringsAsFactors = FALSE)
      saida$Previsao_SARIMAX[match(datas_previsao, saida$Data)] <- ajuste$previsao
      message(localidade, ": concluido; ", nrow(treino), " meses no calendario de treino.")
      saida
    }, error = function(e) {
      message(localidade, ": ignorado; ", conditionMessage(e))
      NULL
    })
    if (!is.null(resultado)) resultados[[localidade]] <- resultado
  }
  if (!length(resultados)) stop("Nenhuma localidade processada; CSV anterior preservado.")
  saida <- do.call(rbind, resultados)
  rownames(saida) <- NULL
  gravar_csv_sarimax(saida, config$saida)
  message("CSV SARIMAX: ", config$saida)
  invisible(saida)
}

if (!isTRUE(getOption("frota.sarimax.apenas_funcoes", FALSE))) {
  executar_sarimax_local()
}
