# Meta Prophet: registros mensais por municipio e PARA (Geral).
# Revisao: PROPHET-CRAN-LOCAL-ESTADO-2-DF; CSV mensal, erros em runtime.
# Base consultada: GitHub publicacao-codigo, 91e519d5521336790484a77be963039a24c05b65.
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
#
# Regra operacional: minimo 24 meses conhecidos, abrangendo os 12 meses do ano.
# Isso nao e requisito do pacote nem garantia estatistica de dados suficientes.
# A sazonalidade anual de ordem 3 e um candidato inicial, nao um valor otimizado.
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
# Operacao: ajustar tendencia linear por trechos e sazonalidade anual aditiva.
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

executar_prophet_local <- function(config = config_prophet_local) {
  # global.R pode usar OutDec=",". Isolar ajuste/escrita e restaurar a sessao.
  anteriores <- options(OutDec = "."); on.exit(options(anteriores), add = TRUE)
  inteiros <- c(config$ano_inicio, config$ano_teste, config$ano_fim,
    config$n_teste, config$passos_futuro, config$minimo_treino, config$seed)
  stopifnot(length(inteiros) == 7L, all(is.finite(inteiros)),
    all(inteiros == floor(inteiros)), config$ano_inicio >= 1L,
    config$ano_inicio < config$ano_teste, config$ano_fim == config$ano_teste,
    config$ano_fim <= 9999L, config$n_teste == 12L,
    config$passos_futuro > 0L, config$minimo_treino >= 24L, config$seed >= 0L)
  if (!is.character(config$entrada) || length(config$entrada) != 1L ||
      is.na(config$entrada) || !nzchar(config$entrada) ||
      !is.character(config$saida) || length(config$saida) != 1L ||
      is.na(config$saida) || !nzchar(config$saida)) stop("Caminhos invalidos.")
  if (identical(normalizePath(config$entrada, winslash = "/", mustWork = FALSE),
      normalizePath(config$saida, winslash = "/", mustWork = FALSE))) {
    stop("Entrada e saida devem ser arquivos distintos.")
  }
  if (!requireNamespace("prophet", quietly = TRUE)) {
    stop('Instale no seu R local: install.packages("prophet", repos = "http://cran.rstudio.com/")')
  }
  mensal <- ler_mensal_prophet(config)
  corte <- as.Date(sprintf("%d-01-01", config$ano_teste))
  datas_teste <- seq(corte, by = "month", length.out = config$n_teste)
  datas_futuras <- seq(as.Date(sprintf("%d-01-01", config$ano_teste + 1L)),
    by = "month", length.out = config$passos_futuro)
  datas_prever <- c(datas_teste, datas_futuras)
  resultados <- list()
  for (nome in unique(mensal$Localidade)) {
    serie <- mensal[mensal$Localidade == nome, ]
    resultado <- tryCatch({
      # ======================================================================
      # ETAPA 04 — SEPARACAO TEMPORAL
      # Entrada: calendario completo com Real original.
      # Operacao: separar 2016-2024 do teste; Real/2025 so participa da exportacao.
      # Saida: historico de treinamento e mascara de teste.
      # ======================================================================
      periodo_treino <- serie$Data < corte
      teste <- !periodo_treino
      historico <- serie[periodo_treino, ]
      conhecidos <- is.finite(historico$Real)

      # ======================================================================
      # ETAPA 05 — VIABILIDADE, COBERTURA E RECENCIA
      # Regra operacional: >=24 valores conhecidos e os 12 meses representados.
      # Nao exigir dezembro/2024 conhecido; nao imputar nem trocar de algoritmo.
      # ======================================================================
      meses <- unique(as.integer(format(historico$Data[conhecidos], "%m")))
      if (sum(conhecidos) < config$minimo_treino || length(meses) < 12L) {
        stop("historico insuficiente: ", sum(conhecidos),
          " meses conhecidos e ", length(meses), "/12 meses do ano representados.")
      }
      ultimo <- max(historico$Data[conhecidos])
      if (any(!conhecidos)) {
        message(nome, ": ", sum(!conhecidos), " meses de treino ausentes; sem imputacao.")
      }
      message(nome, ": treino com ", sum(conhecidos), " observacoes; ultimo mes conhecido ", ultimo, ".")
      if (ultimo < tail(historico$Data, 1L)) {
        message(nome, ": historico termina antes do corte; horizonte permanece fixo em ", corte, ".")
      }
      if (identical(nome, "PARÁ (Geral)") &&
          any(historico$Municipios_com_total_conhecido < historico$Municipios_esperados)) {
        message(nome, ": soma estadual observada com cobertura municipal parcial.")
      }

      # ======================================================================
      # ETAPA 06 — TABELA DS/Y PARA O PROPHET
      # Entrada: somente treino. Saida: datas reais e quantidades conhecidas.
      # Regra: retirar NA do ajuste sem comprimir as datas ou alterar Real.
      # ======================================================================
      treino <- data.frame(ds = historico$Data[conhecidos], y = historico$Real[conhecidos])
      modelo <- ajustar_prophet(treino, config)
      pred <- prever_mensal_prophet(modelo, datas_prever)
      if (length(pred$previsao) != length(datas_prever) || any(!is.finite(pred$previsao))) {
        stop("Previsoes nao correspondem ao horizonte solicitado.")
      }
      if (pred$negativas_corrigidas > 0L) {
        message(nome, ": ", pred$negativas_corrigidas, " previsoes negativas limitadas a zero na saida.")
      }

      # ======================================================================
      # ETAPA 09 — CONSOLIDACAO NA ESCALA ORIGINAL
      # Entrada: yhat e Real original. Saida: esquema mensal usado pelo painel.
      # Regra: previsao NA no treino, Real NA no futuro; nao calcular metricas.
      # ======================================================================
      stopifnot(sum(teste) == config$n_teste, identical(serie$Data[teste], datas_teste))
      previstos <- rep(NA_real_, nrow(serie))
      previstos[teste] <- head(pred$previsao, config$n_teste)
      data.frame(Localidade = nome, Data = c(serie$Data, datas_futuras),
        Real = c(serie$Real, rep(NA_real_, config$passos_futuro)),
        Previsao_PROPHET = c(previstos, tail(pred$previsao, config$passos_futuro)),
        Modelo = "PROPHET", AIC = NA_real_, BIC = NA_real_,
        Particao = c(ifelse(teste, "teste", "treino"), rep("futuro", config$passos_futuro)),
        Municipios_com_registro = c(serie$Municipios_com_registro, rep(NA, config$passos_futuro)),
        Municipios_com_total_conhecido = c(serie$Municipios_com_total_conhecido,
          rep(NA, config$passos_futuro)), Municipios_esperados = serie$Municipios_esperados[1L])
    }, error = function(e) e)
    if (inherits(resultado, "error")) {
      message(nome, ": ignorado; ", conditionMessage(resultado))
      next
    }
    resultados[[nome]] <- resultado
    message(nome, ": previsoes mensais geradas.")
  }

  # ==========================================================================
  # ETAPA 10 — EXPORTACAO DO UNICO CSV
  # Regra: nenhuma avaliacao offline, grafico, modelo RDS ou relatorio auxiliar.
  # Se nenhuma localidade for processada, preservar a saida anterior.
  # ==========================================================================
  if (!length(resultados)) stop("Nenhuma localidade processada; consulte o console.")
  previsoes <- do.call(rbind, resultados)
  rownames(previsoes) <- NULL
  if (anyDuplicated(previsoes[c("Localidade", "Data")])) stop("Previsoes com datas duplicadas.")
  dir.create(dirname(config$saida), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(previsoes, config$saida, row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
  message("PROPHET_LOCAL_CONCLUIDO: ", length(resultados), " localidades; ", nrow(previsoes), " linhas.")
  invisible(list(previsoes = previsoes))
}

# Source/Rscript executam o produtor no Windows. Os testes locais usam a opcao
# abaixo para carregar funcoes sem processar o CSV real. Nao executar na nuvem.
if (!isTRUE(getOption("frota.prophet.apenas_funcoes", FALSE))) executar_prophet_local()
