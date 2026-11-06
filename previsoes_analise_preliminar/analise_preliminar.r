# ACF/PACF dos registros mensais de veiculos -- esquema AP-ACF-PACF-1.
# Executar na raiz do projeto:
# source("previsoes_analise_preliminar/analise_preliminar.r", encoding = "UTF-8")
# Ou, dentro desta pasta: Rscript --vanilla analise_preliminar.r
# Dependencias: somente os pacotes distribuidos com o R, sem instalacoes.
#
# ENTRADA: data/veiculos_por_mes_municipio.csv.
# UNICA SAIDA: previsoes_analise_preliminar/analise_preliminar_output.csv.
# Nao gera graficos, modelos, previsoes, relatorios auxiliares ou arquivos de erros.
# Reexecutar substitui o CSV, sem acumular linhas de execucoes anteriores.
#
# CONTRATO PARA A FUTURA ABA DO SHINY
# Uma linha por Localidade + Representacao + Lag, com ACF e PACF lado a lado.
# Representacao: nivel ou diferenca_1. Lag esta em meses e inicia em 1 (nao ha lag0).
# N: numero de observacoes da representacao no bloco, diferente de Meses_bloco.
# Inicio/Fim_bloco: bloco original escolhido; Inicio/Fim_amostra: datas efetivas
# da representacao (a primeira diferenca inicia no segundo mes do bloco).
# Status: ok, bloco_insuficiente, serie_constante ou erro_calculo.
# Falhas geram uma linha por representacao, com Lag/coeficientes/faixas NA.
# Cobertura_min/max: municipios com total conhecido por mes no periodo analisado;
# para um municipio esses numeros sao 0/1; no estado, comparam-se a Municipios_esperados.
# Limite_inferior/superior: referencias pontuais aproximadas +/-1.96/sqrt(N)
# sob ruido branco; NA em linhas sem calculo. Nao sao intervalos de previsao,
# testes conjuntos nem evidencia de qual modelo ou lag e melhor.
# Versao_esquema e MD5_entrada permitem identificar contrato e versao da base.
# Valores numericos usam ponto decimal; texto em UTF-8; campos ausentes ficam vazios.

config_analise_preliminar <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "previsoes_analise_preliminar/analise_preliminar_output.csv",
  inicio = as.Date("2016-01-01"), fim = as.Date("2024-12-01"),
  lag_max = 24L, minimo_bloco = 24L
)

# 1. LOCALIZAR A RAIZ, sem alterar getwd() ou depender do explorador anterior.
ap_raiz_projeto <- function(entrada) {
  candidatos <- c(getwd(), dirname(getwd()))
  for (p in unique(candidatos)) {
    if (file.exists(file.path(p, entrada))) return(normalizePath(p, winslash = "/"))
  }
  stop("Entrada nao encontrada. Execute a partir da raiz do projeto ou desta pasta.")
}

# 2. VALIDAR A BASE E SEPARAR O TREINAMENTO ANTES DOS DIAGNOSTICOS.
# ANO/MES indicam fluxo mensal de registros, conforme esclarecimento do usuario.
# Somar todos os tipos presentes, como no produtor atual; nao filtrar automoveis.
ap_ler_base <- function(arquivo) {
  x <- utils::read.csv(arquivo, colClasses = "character", check.names = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", "NaN", "NULL"))

  obrigatorias <- c("LOCAL", "ANO", "MES", "TIPO_VEICULO", "QTD_VEICULO")

  if (!all(obrigatorias %in% names(x))) stop("Colunas obrigatorias ausentes na entrada.")

  for (nome in c("LOCAL", "TIPO_VEICULO")) {
    x[[nome]] <- trimws(x[[nome]])
    if (anyNA(x[[nome]]) || any(!nzchar(x[[nome]]))) stop("Identificador ausente: ", nome)
  }

  for (nome in c("ANO", "MES", "QTD_VEICULO")) {
    texto <- trimws(x[[nome]])
    ausente <- is.na(texto) | toupper(texto) %in% c("", "NA", "NAN", "NULL")
    valor <- suppressWarnings(as.numeric(texto))
    valor[ausente] <- NA_real_
    if (any(!ausente & !is.finite(valor))) stop("Numero invalido na coluna ", nome)
    x[[nome]] <- valor
  }

  if (anyNA(x$ANO) || anyNA(x$MES) || any(x$ANO != floor(x$ANO)) ||
      any(x$ANO < 1 | x$ANO > 9999) || any(x$MES != floor(x$MES)) ||
      any(x$MES < 1 | x$MES > 12)) stop("Ano ou mes invalido.")
      
  if (any(x$QTD_VEICULO < 0 | x$QTD_VEICULO != floor(x$QTD_VEICULO), na.rm = TRUE)) {
    stop("Quantidade negativa ou nao inteira.")
  }
  if (anyDuplicated(x[c("LOCAL", "ANO", "MES", "TIPO_VEICULO")])) {
    stop("Chave duplicada LOCAL/ANO/MES/TIPO_VEICULO; investigar antes de somar.")
  }
  chave_local <- chartr("ÁÀÂÃÉÊÍÓÔÕÚÇ", "AAAAEEIOOOUC", toupper(x$LOCAL))
  if (any(chave_local %in% c("PARA", "PARA (GERAL)"))) {
    stop("Total estadual na entrada: o estado deve ser formado apenas dos municipios.")
  }
  x$Data <- as.Date(sprintf("%04d-%02d-01", x$ANO, x$MES))
  x
}

# 3. CALENDARIO REGULAR E SOMA ESTADUAL OBSERVADA.
# Um mes sem linha permanece NA; zeros explicitos sao preservados. Nao expandir
# categorias ausentes como zero. NA explicito em um grupo torna o total NA.
# O estado soma apenas municipios com linha, sem imputar os demais com zero.
ap_series_mensais <- function(base, config) {
  municipios <- sort(unique(base$LOCAL)) # apenas identificadores do arquivo integral
  x <- base[base$Data >= config$inicio & base$Data <= config$fim, ]
  if (!nrow(x)) 
    stop("Sem registros no periodo configurado.")

  mensal <- stats::aggregate(x$QTD_VEICULO, x[c("LOCAL", "Data")],
    function(v) if (anyNA(v)) NA_real_ else sum(v))

  names(mensal)[3L] <- "Registros"

  datas <- seq(config$inicio, config$fim, by = "month")

  valores <- matrix(NA_real_, length(datas), length(municipios), dimnames = list(as.character(datas), municipios))

  presente <- matrix(FALSE, nrow(valores), ncol(valores))

  indices <- cbind(match(mensal$Data, datas), match(mensal$LOCAL, municipios))

  valores[indices] <- mensal$Registros; presente[indices] <- TRUE

  estado <- vapply(seq_along(datas), function(i) {
    v <- valores[i, presente[i, ]]
    if (!length(v) || anyNA(v)) NA_real_ else sum(v)
  }, numeric(1))

  series <- c(list(estado), lapply(seq_along(municipios), function(j) valores[, j]))
  names(series) <- c("PARÁ (Geral)", municipios)

  cobertura <- c(list(rowSums(is.finite(valores))),
    lapply(seq_along(municipios), function(j) as.integer(is.finite(valores[, j]))))
  names(cobertura) <- names(series)
  list(series = series, datas = datas, cobertura = cobertura, n_municipios = length(municipios))
}

# 4. BLOCO CONTINUO PARA ACF/PACF, sem na.omit sobre a serie completa.
# Selecionar o maior bloco conhecido, com desempate pelo mais recente. Nivel
# e primeira diferenca usam esse mesmo bloco; nao saltam sobre meses ausentes.
ap_maior_bloco <- function(y) {
  r <- rle(is.finite(y)); finais <- cumsum(r$lengths)
  candidatos <- which(r$values)
  if (!length(candidatos)) return(integer())
  candidatos <- candidatos[r$lengths[candidatos] == max(r$lengths[candidatos])]
  j <- tail(candidatos, 1L)
  seq.int(finais[j] - r$lengths[j] + 1L, finais[j])
}

# ACF/PACF sao estatisticas descritivas, sem ajuste de modelo de previsao.
# O limite de lags e o mesmo nas duas representacoes. Blocos curtos podem nao
# alcancar o lag12; o tamanho minimo e operacional, nao garantia de confiabilidade.
ap_analisar_localidade <- function(nome, y, datas, cobertura, esperados, config, md5) {

  tipo <- if (identical(nome, "PARÁ (Geral)")) "estado" else "municipio"
  bloco <- ap_maior_bloco(y)
  n_bloco <- length(bloco)
  lag_max <- if (n_bloco >= config$minimo_bloco) min(config$lag_max, floor((n_bloco - 1L) / 4L)) else NA_integer_
  ultimo_bloco <- if (is.finite(tail(y, 1L))) tail(rle(is.finite(y))$lengths, 1L) else 0L
  aviso <- character()

  if (anyNA(y)) 
     aviso <- c(aviso, "serie_com_lacunas")

  if (tipo == "estado" && any(cobertura < esperados)) 
    aviso <- c(aviso, "soma_estadual_observada_com_cobertura_parcial")

  if (n_bloco && tail(bloco, 1L) < length(y)) 
    aviso <- c(aviso, "bloco_nao_termina_no_fim_da_analise")

  if (is.finite(lag_max) && lag_max < 12L) 
    aviso <- c(aviso, "diagnostico_nao_alcanca_lag_12")

  valores <- list(nivel = y[bloco], diferenca_1 = diff(y[bloco]))
  saidas <- list()
  for (representacao in names(valores)) {
    v <- valores[[representacao]]; n <- length(v)
    status <- "ok"; detalhe <- character(); calculo <- NULL
    if (n_bloco < config$minimo_bloco) {
      status <- "bloco_insuficiente"
    } else if (!is.finite(stats::var(v)) || stats::var(v) == 0) {
      status <- "serie_constante"
    } else {
      calculo <- tryCatch({
        a <- as.numeric(stats::acf(v, lag.max = lag_max, plot = FALSE, na.action = na.fail)$acf)[-1L]
        p <- as.numeric(stats::pacf(v, lag.max = lag_max, plot = FALSE, na.action = na.fail)$acf)
        if (length(a) != lag_max || length(p) != lag_max || any(!is.finite(c(a, p)))) {
          stop("Correlacoes nao finitas ou dimensao inesperada.")
        }
        list(acf = a, pacf = p)
      }, error = function(e) { detalhe <<- conditionMessage(e); NULL })
      if (is.null(calculo)) status <- "erro_calculo"
    }
    amostra <- if (representacao == "nivel") bloco else if (n_bloco > 1L) bloco[-1L] else integer()
    linhas <- if (status == "ok") lag_max else 1L
    limite <- if (status == "ok") 1.96 / sqrt(n) else NA_real_

    saidas[[representacao]] <- data.frame(
      Versao_esquema = "AP-ACF-PACF-1", Arquivo_entrada = config$entrada, MD5_entrada = md5,
      Localidade = nome, Tipo_localidade = tipo,
      Representacao = representacao, Inicio_analise = as.character(config$inicio), Fim_analise = as.character(config$fim),
      Inicio_bloco = if (n_bloco) as.character(datas[head(bloco, 1L)]) else NA_character_,
      Fim_bloco = if (n_bloco) as.character(datas[tail(bloco, 1L)]) else NA_character_,
      Inicio_amostra = if (length(amostra)) as.character(datas[head(amostra, 1L)]) else NA_character_,
      Fim_amostra = if (length(amostra)) as.character(datas[tail(amostra, 1L)]) else NA_character_,
      Meses_calendario = length(y), Meses_conhecidos = sum(is.finite(y)), Meses_ausentes = sum(is.na(y)),
      Meses_bloco = n_bloco, Meses_bloco_final = ultimo_bloco, N = n, Lag_max = lag_max,
      Lag = if (status == "ok") seq_len(linhas) else NA_integer_,
      ACF = if (status == "ok") calculo$acf else NA_real_,
      PACF = if (status == "ok") calculo$pacf else NA_real_,
      Limite_inferior = -limite, Limite_superior = limite,
      Cobertura_min = min(cobertura), Cobertura_max = max(cobertura), Municipios_esperados = esperados,
      Cobertura_completa = all(cobertura == esperados), Status = status,
      Observacao = paste(c(aviso, detalhe), collapse = "; "), stringsAsFactors = FALSE
    )
  }
  do.call(rbind, saidas)
}

# 5. GERAR UM UNICO CSV COMPLETO ANTES DE SUBSTITUIR A SAIDA.
# O temporario e apagado ao sair. file.copy(overwrite=TRUE) permite reexecucao
# no Windows; em caso de bloqueio (p.ex. Excel aberto), informar erro no console.
# Nao se promete substituicao atomica entre sistemas operacionais.

executar_analise_preliminar <- function(config = config_analise_preliminar) {
  op <- options(OutDec = "."); on.exit(options(op), add = TRUE)
  if (length(config$inicio) != 1L || length(config$fim) != 1L ||
      anyNA(c(config$inicio, config$fim)) || config$inicio > config$fim ||
      any(format(c(config$inicio, config$fim), "%d") != "01")) stop("Periodo mensal invalido.")

  if (length(config$lag_max) != 1L || !is.finite(config$lag_max) || config$lag_max < 1L ||
      config$lag_max != floor(config$lag_max) || length(config$minimo_bloco) != 1L ||
      !is.finite(config$minimo_bloco) || config$minimo_bloco < 8L ||
      config$minimo_bloco != floor(config$minimo_bloco)) stop("Limites de lags/bloco invalidos.")

  raiz <- ap_raiz_projeto(config$entrada)
  arquivo <- file.path(raiz, config$entrada); saida <- file.path(raiz, config$saida)
  if (identical(normalizePath(arquivo, mustWork = TRUE), normalizePath(saida, mustWork = FALSE))) {
    stop("Entrada e saida devem ser arquivos distintos.")
  }

  base <- ap_ler_base(arquivo); obj <- ap_series_mensais(base, config)

  md5 <- unname(tools::md5sum(arquivo))

  resultado <- do.call(rbind, lapply(seq_along(obj$series), function(i) {
    ap_analisar_localidade(names(obj$series)[i], obj$series[[i]], obj$datas, obj$cobertura[[i]],
      if (i == 1L) obj$n_municipios else 1L, config, md5)
  }))

  rownames(resultado) <- NULL
  dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile("analise_preliminar_", tmpdir = dirname(saida), fileext = ".tmp")
  on.exit(unlink(temporario), add = TRUE)
  utils::write.csv(resultado, temporario, row.names = FALSE, na = "", fileEncoding = "UTF-8")

  if (!file.copy(temporario, saida, overwrite = TRUE)) {
    stop("Nao foi possivel substituir o CSV. Feche o arquivo em outros programas e tente novamente.")
  }

  message("ANALISE_PRELIMINAR_CONCLUIDA: ", normalizePath(saida, winslash = "/"),
    " (", nrow(resultado), " linhas; ", length(unique(resultado$Localidade)), " localidades)")

  invisible(resultado)
}

if (!isTRUE(getOption("frota.analise_preliminar.apenas_funcoes", FALSE))) executar_analise_preliminar()
