# source("previsoes_analise_preliminar/analise_preliminar.r", encoding = "UTF-8")
# ACF/PACF dos registros mensais de veiculos -- AP-ACF-PACF-GRAFICO-2.
# Executar no console R, com a raiz do projeto como diretorio de trabalho:
# source("previsoes_analise_preliminar/analise_preliminar.r", encoding = "UTF-8")
# Ou, dentro desta pasta: source("analise_preliminar.r", encoding = "UTF-8")
# Dependencias: somente os pacotes distribuidos com o R, sem instalacoes.
#
# ENTRADA: data/veiculos_por_mes_municipio.csv.
# SAIDA: um painel 2x2 no dispositivo grafico da sessao R (Plots/janela grafica).
# Nao exporta CSV, PNG ou PDF, nao usa Shiny e nao ajusta modelos de previsao.
# Usar source() em sessao interativa, em vez de Rscript: nao criar Rplots.pdf.
# O CSV produzido pela versao anterior nao e lido, alterado ou regenerado.
#
# LEITURA DO PAINEL
# Primeira linha: ACF e PACF do nivel; segunda: ACF e PACF da primeira diferenca.
# Lags em meses a partir de 1; N e datas efetivas aparecem em cada painel.
# Referencias pontuais aproximadas +/-1.96/sqrt(N) sob ruido branco.
# Nao sao intervalos de previsao, testes conjuntos ou selecao automatica de lags.
# Blocos insuficientes e series constantes aparecem como avisos no painel.
# Para outro municipio, editar localidade abaixo com o nome presente em LOCAL.
# Ou, apos source():
# config_analise_preliminar$localidade <- "BELEM"
# executar_analise_preliminar()

config_analise_preliminar <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  localidade = "PARÁ (Geral)",
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
  if (!nrow(x)) stop("Sem registros no periodo configurado.")
  mensal <- stats::aggregate(x$QTD_VEICULO, x[c("LOCAL", "Data")],
    function(v) if (anyNA(v)) NA_real_ else sum(v))
  names(mensal)[3L] <- "Registros"
  datas <- seq(config$inicio, config$fim, by = "month")
  valores <- matrix(NA_real_, length(datas), length(municipios),
    dimnames = list(as.character(datas), municipios))
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
ap_analisar_localidade <- function(nome, y, datas, cobertura, esperados, config) {
  tipo <- if (identical(nome, "PARÁ (Geral)")) "estado" else "municipio"
  bloco <- ap_maior_bloco(y)
  n_bloco <- length(bloco)
  lag_max <- if (n_bloco >= config$minimo_bloco) min(config$lag_max, floor((n_bloco - 1L) / 4L)) else NA_integer_
  aviso <- character()
  if (anyNA(y)) aviso <- c(aviso, "Serie com lacunas: usado o maior bloco continuo conhecido.")
  if (tipo == "estado" && any(cobertura < esperados)) {
    aviso <- c(aviso, paste0("Soma estadual observada: cobertura parcial (",
      min(cobertura), " a ", max(cobertura), "/", esperados, " municipios no periodo)."))
  }
  if (n_bloco && tail(bloco, 1L) < length(y)) {
    aviso <- c(aviso, "O bloco selecionado termina antes do fim da analise.")
  }
  if (is.finite(lag_max) && lag_max < 12L) aviso <- c(aviso, "O diagnostico nao alcanca o lag 12.")
  valores <- list(nivel = y[bloco], diferenca_1 = diff(y[bloco]))
  saidas <- list()
  for (representacao in names(valores)) {
    v <- valores[[representacao]]; n <- length(v)
    status <- "ok"; detalhe <- character(); calculo <- NULL
    if (n_bloco < config$minimo_bloco) {
      status <- "bloco_insuficiente"
      detalhe <- paste0("Bloco insuficiente: ", n_bloco, " meses.\nMinimo: ", config$minimo_bloco, " meses.")
    } else if (!is.finite(stats::var(v)) || stats::var(v) == 0) {
      status <- "serie_constante"
      detalhe <- "Serie constante: correlacoes indefinidas."
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
    saidas[[representacao]] <- list(
      status = status, mensagem = detalhe, n = n, datas = datas[amostra],
      lags = if (status == "ok") seq_len(lag_max) else integer(),
      acf = if (status == "ok") calculo$acf else numeric(),
      pacf = if (status == "ok") calculo$pacf else numeric(),
      limite = if (status == "ok") 1.96 / sqrt(n) else NA_real_
    )
  }
  list(localidade = nome, periodo = c(config$inicio, config$fim),
    datas_bloco = datas[bloco], avisos = aviso, representacoes = saidas)
}

# 5. EXIBIR O PAINEL DIRETAMENTE NO R, sem exportar arquivos.
# Usar vetores e eixo manual para que as defasagens estejam em meses, nao anos.
# Restaurar os parametros graficos, mantendo aberto o dispositivo do usuario.
ap_plotar_diagnostico <- function(resultado) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  graphics::par(mfrow = c(2, 2), mar = c(4.2, 4.2, 3.4, 1.2),
    oma = c(3.6, 0, 3.8, 0), cex.main = 0.95)
  rotulos <- c(nivel = "Nivel", diferenca_1 = "Primeira diferenca")
  for (representacao in names(resultado$representacoes)) {
    r <- resultado$representacoes[[representacao]]
    periodo <- if (length(r$datas)) {
      paste(format(range(r$datas), "%m/%Y"), collapse = " a ")
    } else "sem amostra conhecida"
    for (medida in c("acf", "pacf")) {
      titulo <- paste(toupper(medida), "-", rotulos[[representacao]])
      if (r$status != "ok") {
        graphics::plot.new()
        graphics::title(main = titulo)
        graphics::text(0.5, 0.5, paste(strwrap(r$mensagem, width = 38), collapse = "\n"), cex = 0.9)
      } else {
        cor <- if (medida == "acf") "#176B8E" else "#9C3D64"
        graphics::plot(r$lags, r[[medida]], type = "h", lwd = 2, col = cor,
          xlim = c(0.5, max(r$lags) + 0.5), ylim = c(-1, 1), xaxt = "n",
          xlab = "Defasagem (meses)", ylab = toupper(medida), main = titulo)
        marcas <- sort(unique(c(1L, r$lags[r$lags %% 6L == 0L], max(r$lags))))
        graphics::axis(1, at = marcas)
        graphics::abline(h = 0, col = "#555555")
        graphics::abline(h = c(-r$limite, r$limite), col = "#666666", lty = 2)
      }
      graphics::mtext(paste0("N = ", r$n, " | ", periodo), side = 3, line = 0.25, cex = 0.7)
    }
  }
  graphics::mtext(paste("ACF e PACF -", resultado$localidade), outer = TRUE,
    side = 3, line = 2.1, font = 2, cex = 1.05)
  graphics::mtext(paste("Janela de analise:", paste(format(resultado$periodo, "%m/%Y"), collapse = " a ")),
    outer = TRUE, side = 3, line = 0.8, cex = 0.8)
  graphics::mtext("Tracejado: referencia pontual +/-1,96/sqrt(N) sob ruido branco.",
    outer = TRUE, side = 1, line = 0.9, cex = 0.7)
  if (length(resultado$avisos)) {
    graphics::mtext(resultado$avisos[1L], outer = TRUE, side = 1, line = 2.1, cex = 0.65)
  }
  invisible(resultado)
}

# 6. EXECUTAR APENAS A ANALISE DESCRITIVA DA LOCALIDADE CONFIGURADA.
executar_analise_preliminar <- function(config = config_analise_preliminar) {
  op <- options(OutDec = "."); on.exit(options(op), add = TRUE)
  if (!interactive()) {
    stop("Execute com source() no console de uma sessao interativa do R para exibir o grafico, sem gravar arquivos.")
  }
  if (length(config$inicio) != 1L || length(config$fim) != 1L ||
      anyNA(c(config$inicio, config$fim)) || config$inicio > config$fim ||
      any(format(c(config$inicio, config$fim), "%d") != "01")) stop("Periodo mensal invalido.")
  if (length(config$lag_max) != 1L || !is.finite(config$lag_max) || config$lag_max < 1L ||
      config$lag_max != floor(config$lag_max) || length(config$minimo_bloco) != 1L ||
      !is.finite(config$minimo_bloco) || config$minimo_bloco < 8L ||
      config$minimo_bloco != floor(config$minimo_bloco)) stop("Limites de lags/bloco invalidos.")
  if (!is.character(config$localidade) || length(config$localidade) != 1L ||
      is.na(config$localidade) || !nzchar(config$localidade)) stop("Configure uma unica localidade valida.")
  raiz <- ap_raiz_projeto(config$entrada)
  base <- ap_ler_base(file.path(raiz, config$entrada)); obj <- ap_series_mensais(base, config)
  i <- match(config$localidade, names(obj$series))
  if (is.na(i)) {
    stop("Localidade nao encontrada: ", config$localidade,
      ". Use o nome presente em LOCAL ou PARÁ (Geral).")
  }
  resultado <- ap_analisar_localidade(names(obj$series)[i], obj$series[[i]], obj$datas, obj$cobertura[[i]],
    if (i == 1L) obj$n_municipios else 1L, config)
  if (length(resultado$avisos)) message(paste(resultado$avisos, collapse = "\n"))
  ap_plotar_diagnostico(resultado)
  message("ANALISE_PRELIMINAR_CONCLUIDA: grafico exibido para ", resultado$localidade)
  invisible(resultado)
}

if (!isTRUE(getOption("frota.analise_preliminar.apenas_funcoes", FALSE))) executar_analise_preliminar()