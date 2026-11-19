# ==============================================================================
# pre_compilar_GRU_global.r | GRU-GLOBAL-1-CAUSAL-VALIDACAO
# Base: GitHub publicacao-codigo,31e3667bc0f291db3c4ae0e2ff811d4712aa67f1.
# Executar SOMENTE no Windows/VS Code, na raiz do projeto:
# source("Previsoes_pre_compilacao/pre_compilar_GRU_global.r", encoding="UTF-8")
# Dependencias locais: keras3,tensorflow,reticulate. Instalar separadamente:
# install.packages(c("keras3","tensorflow","reticulate"), repos="http://cran.rstudio.com/")
# Nao instala ambientes Python/pacotes e nao utiliza GPU nesta revisao.
#
# Uma unica rede global por origem, com identidade one-hot da localidade.
# T=a+b*t+c*t^2; S(m)=media(y/T no mes); z=(y/(T*S)-mu)/sigma.
# T,S,mu,sigma e pesos usam somente treino interno. Validacao interna e
# diagnostico de um passo; teste externo e recursivo, sem Real do teste.
# Nunca iniciar nova sequencia em2026 com observacoes de2025.
#
# Producao: um worker fresco com threads_treino=6; uma unica rede.
# Validacao: controlador distribui origens inteiras em ate6workers, cadaum
# com uma thread; nao existe pool interno nem rede por municipio.
# Somente o pai le a entrada e grava CSVs; objetos Python nao cruzam processos.
# ==============================================================================

# ---- 1. CONFIGURACAO E CONTRATO DE IMPORTACAO SEGURA --------------------------
gru_versao <- "GRU-GLOBAL-1-CAUSAL-VALIDACAO"
config_gru_global <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_gru_global_pre_comp.csv",
  historico_perdas = "data/historico_perdas_gru.csv",
  salvar_historicos = TRUE,
  script_pre_compilacao = "Previsoes_pre_compilacao/pre_compilar_GRU_global.r",
  inicio_treino = as.Date("2016-01-01"), fim_treino = as.Date("2024-12-31"),
  meses_teste = 12L, passos_futuro = 72L,
  lookback = 24L, minimo_treino = 24L, fracao_validacao = 0.1,
  seed = 42L, localidades = NULL, python = NULL, threads_treino = 6L,
  parametros = list(unidades = 32L, taxa_aprendizado = 0.01,
    epocas = 100L, tamanho_lote = 32L, verbose = 0L)
)

gru_inteiro <- function(x, minimo = 0L) {
  is.numeric(x) && length(x) == 1L && is.finite(x) &&
    x >= minimo && x <= .Machine$integer.max && x == floor(x)
}

gru_periodos <- function(config) {
  inicio <- config$inicio_treino
  fim <- config$fim_treino
  meses <- config$meses_teste
  if (!inherits(inicio, "Date") || length(inicio) != 1L || is.na(inicio) ||
      !inherits(fim, "Date") || length(fim) != 1L || is.na(fim) || inicio > fim ||
      format(inicio, "%d") != "01" || !gru_inteiro(meses, 1L)) {
    stop("Informe inicio/fim de treino como Date e meses_teste inteiro positivo.")
  }
  proximo <- seq(as.Date(format(fim, "%Y-%m-01")), by = "month", length.out = 2L)[2L]
  if (fim != proximo - 1L) stop("fim_treino deve ser o ultimo dia do mes.")
  limites <- seq(proximo, by = "month", length.out = as.integer(meses) + 1L)
  list(inicio_treino = inicio, fim_treino = fim,
    inicio_teste = limites[1L], fim_teste = tail(limites, 1L) - 1L,
    meses_teste = as.integer(meses), datas_teste = head(limites, -1L),
    meses_treino = length(seq(inicio, as.Date(format(fim, "%Y-%m-01")), by = "month")))
}

gru_chave_localidade <- function(x) {
  x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC",
    toupper(trimws(as.character(x))))
  x <- trimws(gsub("[^A-Z0-9]+", " ", x))
  x[x %in% c("PARA", "PARA GERAL")] <- "PARA GERAL"
  x
}

gru_localidades <- function(mensal, selecionadas = NULL) {
  disponiveis <- unique(as.character(mensal$Localidade))
  if (!length(disponiveis)) stop("Nenhuma localidade disponivel.")
  if (is.null(selecionadas)) return(disponiveis)
  if (!length(selecionadas) || anyNA(selecionadas)) stop("Selecione ao menos uma localidade.")
  idx <- match(gru_chave_localidade(selecionadas), gru_chave_localidade(disponiveis))
  if (anyNA(idx)) stop("Localidades nao encontradas: ", paste(selecionadas[is.na(idx)], collapse = ", "))
  unique(disponiveis[idx])
}

# ---- 2. LEITURA: UMA VEZ NO PAI, ANTES DO FILTRO DE LOCALIDADES ----------------
# Completar o calendario nao imputa quantidade: mes ausente permanece NA.
# Quantidade desconhecida em um grupo torna seu total desconhecido. O estado
# soma os registros observados, com cobertura informada, antes de completar.
ler_mensal_gru <- function(config) {
  p <- gru_periodos(config)
  if (!file.exists(config$entrada) || dir.exists(config$entrada)) stop("Entrada GRU nao encontrada: ", config$entrada)
  bruto <- utils::read.csv(config$entrada, colClasses = "character", check.names = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", "NaN", "NULL"))
  campos <- c("LOCAL", "ANO", "MES", "TIPO_VEICULO", "QTD_VEICULO")
  if (!all(campos %in% names(bruto))) stop("CSV sem colunas: ", paste(setdiff(campos, names(bruto)), collapse = ", "))
  local <- trimws(bruto$LOCAL)
  tipo <- trimws(bruto$TIPO_VEICULO)
  ano <- suppressWarnings(as.numeric(bruto$ANO))
  mes <- suppressWarnings(as.numeric(bruto$MES))
  texto <- trimws(bruto$QTD_VEICULO)
  ausente <- is.na(texto) | toupper(texto) %in% c("", "NA", "NAN", "NULL")
  qtd <- suppressWarnings(as.numeric(texto)); qtd[ausente] <- NA_real_
  ruim <- is.na(local) | !nzchar(local) | is.na(tipo) | !nzchar(tipo) |
    !is.finite(ano) | ano != floor(ano) | ano < 1 | ano > 9999 |
    !is.finite(mes) | mes != floor(mes) | mes < 1 | mes > 12 |
    (!ausente & (!is.finite(qtd) | qtd < 0 | qtd != floor(qtd)))
  if (any(ruim)) stop("Dados invalidos nas linhas: ", paste(head(which(ruim) + 1L, 10L), collapse = ", "))
  datas <- as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes)))
  manter <- datas >= p$inicio_treino & datas <= p$fim_teste &
    gru_chave_localidade(local) != "PARA GERAL"
  if (!any(manter)) stop("Sem registros municipais no periodo configurado.")
  chave <- data.frame(Localidade = gru_chave_localidade(local[manter]),
    Data = datas[manter], Tipo = tipo[manter])
  if (anyDuplicated(chave)) stop("CSV com LOCAL/ANO/MES/TIPO_VEICULO duplicados; confira a extracao.")
  base <- data.frame(Localidade = local[manter], Data = datas[manter], Real = qtd[manter])
  somar <- function(x) if (anyNA(x)) NA_real_ else sum(x)
  mensal <- stats::aggregate(base$Real, by = base[c("Localidade", "Data")], FUN = somar)
  names(mensal)[3L] <- "Real"
  calendario <- seq(p$inicio_treino, tail(p$datas_teste, 1L), by = "month")
  municipios <- sort(unique(mensal$Localidade))
  completar <- function(nome, parte, esperados = 1L, registros = rep(1L, nrow(parte)),
      conhecidos = as.integer(is.finite(parte$Real))) {
    idx <- match(calendario, parte$Data)
    nr <- registros[idx]; nr[is.na(idx)] <- 0L
    nc <- conhecidos[idx]; nc[is.na(idx)] <- 0L
    data.frame(Localidade = nome, Data = calendario, Real = parte$Real[idx],
      Municipios_com_registro = nr, Municipios_com_total_conhecido = nc,
      Municipios_esperados = esperados)
  }
  resultados <- lapply(municipios, function(nome) completar(nome, mensal[mensal$Localidade == nome, ]))
  estado <- stats::aggregate(mensal$Real, by = mensal["Data"], FUN = somar)
  names(estado)[2L] <- "Real"
  registros <- stats::aggregate(rep(1L, nrow(mensal)), by = mensal["Data"], FUN = sum)
  conhecidos <- stats::aggregate(as.integer(is.finite(mensal$Real)), by = mensal["Data"], FUN = sum)
  estado <- completar("PARÁ (Geral)", estado, length(municipios),
    registros$x[match(estado$Data, registros$Data)], conhecidos$x[match(estado$Data, conhecidos$Data)])
  do.call(rbind, c(list(estado), resultados))
}


# ---- 3. INTERPRETADOR: RESOLVER SEM INICIALIZAR TF NO PAI ----------------------
gru_validar_python <- function(python) {
  if (!is.character(python) || length(python) != 1L || is.na(python) || !nzchar(trimws(python))) {
    stop("Informe config$python com o caminho de um interpretador Python local existente com TensorFlow/Keras.")
  }
  python <- path.expand(trimws(python))
  if (!file.exists(python) || dir.exists(python)) {
    stop("Interpretador Python nao encontrado: ", python,
      ". Defina config$python com o executavel do ambiente local ja utilizado; nenhum ambiente sera criado.")
  }
  normalizePath(python, winslash = "/", mustWork = TRUE)
}

resolver_python_gru <- function(config) {
  # Descobrir apenas um interpretador EXISTENTE. py_discover_config consulta
  # candidatos sem carregar o interpretador na sessao R e sem gerenciar venv.
  anterior <- Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA_character_)
  on.exit({
    if (is.na(anterior)) Sys.unsetenv("RETICULATE_USE_MANAGED_VENV") else
      Sys.setenv(RETICULATE_USE_MANAGED_VENV = anterior)
  }, add = TRUE)
  Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")
  python <- config[["python"]]
  if (!is.null(python)) return(gru_validar_python(python))
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Pacote reticulate ausente no R local; informe o ambiente Python existente em config$python.")
  }
  if (reticulate::py_available(initialize = FALSE)) {
    return(gru_validar_python(reticulate::py_config()$python))
  }
  for (nome in c("RETICULATE_PYTHON", "KERAS_PYTHON")) {
    definido <- Sys.getenv(nome, unset = "")
    if (nzchar(definido)) return(gru_validar_python(definido))
  }
  descoberto <- tryCatch(reticulate::py_discover_config(required_module = "tensorflow"),
    error = function(e) NULL)
  if (is.null(descoberto) || is.null(descoberto$python)) {
    stop("Nenhum Python local existente com TensorFlow foi identificado. Defina config$python ",
      "com o executavel do ambiente ja utilizado; nenhum ambiente ou pacote sera criado automaticamente.")
  }
  gru_validar_python(descoberto$python)
}


# ---- 4. ESCRITA ATOMICA: APENAS O PAI CHAMA ESTA FUNCAO -----------------------
gravar_csv_gru <- function(dados, saida) {
  dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile("gru_novo_", tmpdir = dirname(saida), fileext = ".csv")
  anterior <- NULL
  on.exit({
    unlink(temporario)
    if (!is.null(anterior) && file.exists(anterior) && !file.exists(saida)) file.rename(anterior, saida)
  }, add = TRUE)
  utils::write.csv(dados, temporario, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  conferido <- utils::read.csv(temporario, colClasses = "character", check.names = FALSE,
    fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  if (nrow(conferido) != nrow(dados) || !identical(names(conferido), names(dados))) stop("CSV GRU temporario inconsistente; destino preservado.")
  if (file.exists(saida)) {
    anterior <- tempfile("gru_anterior_", tmpdir = dirname(saida), fileext = ".csv")
    if (!file.rename(saida, anterior)) stop("Nao foi possivel preservar a saida anterior: ", saida)
  }
  if (!file.rename(temporario, saida)) stop("Nao foi possivel concluir a gravacao: ", saida)
  if (!is.null(anterior)) unlink(anterior)
  invisible(saida)
}

# ---- 5. VALIDACAO DE CONFIGURACAO, CALENDARIO E DADOS R -----------------------
gru_validar_config <- function(config) {
  p <- gru_periodos(config)
  if (!gru_inteiro(config$lookback, 1L) || !gru_inteiro(config$minimo_treino, 1L) ||
      !gru_inteiro(config$seed, 0L) || !gru_inteiro(config$passos_futuro, 0L) ||
      !gru_inteiro(config$threads_treino, 1L) || !is.numeric(config$fracao_validacao) ||
      length(config$fracao_validacao) != 1L || !is.finite(config$fracao_validacao) ||
      config$fracao_validacao <= 0 || config$fracao_validacao >= 1 ||
      !is.logical(config$salvar_historicos) || length(config$salvar_historicos) != 1L || is.na(config$salvar_historicos)) {
    stop("Configuracao GRU invalida: conferir horizontes, lookback, minimo, fracao, semente e threads.")
  }
  a <- config$parametros
  if (!is.list(a) || !gru_inteiro(a$unidades, 1L) || !gru_inteiro(a$epocas, 1L) ||
      !gru_inteiro(a$tamanho_lote, 1L) || !gru_inteiro(a$verbose, 0L) || a$verbose > 2L ||
      !is.numeric(a$taxa_aprendizado) || length(a$taxa_aprendizado) != 1L ||
      !is.finite(a$taxa_aprendizado) || a$taxa_aprendizado <= 0) stop("Hiperparametros GRU invalidos.")
  for (campo in c("entrada", "saida", "historico_perdas", "script_pre_compilacao")) {
    valor <- config[[campo]]
    if (!is.character(valor) || length(valor) != 1L || is.na(valor) || !nzchar(valor)) stop("Caminho invalido: ", campo)
  }
  invisible(p)
}

gru_validar_mensal <- function(mensal, cobertura = FALSE) {
  colunas <- c("Localidade", "Data", "Real")
  if (cobertura) colunas <- c(colunas, "Municipios_com_registro", "Municipios_com_total_conhecido", "Municipios_esperados")
  if (!is.data.frame(mensal) || !all(colunas %in% names(mensal))) stop("Series GRU sem colunas: ", paste(colunas, collapse = ", "))
  mensal$Data <- as.Date(mensal$Data); mensal$Localidade <- as.character(mensal$Localidade)
  if (anyNA(mensal$Data) || any(format(mensal$Data, "%d") != "01") ||
      anyNA(mensal$Localidade) || any(!nzchar(trimws(mensal$Localidade))) ||
      !is.numeric(mensal$Real) || any(!is.na(mensal$Real) & (!is.finite(mensal$Real) | mensal$Real < 0)) ||
      anyDuplicated(data.frame(Localidade = gru_chave_localidade(mensal$Localidade), Data = mensal$Data))) {
    stop("Series GRU com localidades, datas, quantidades ou chaves invalidas.")
  }
  if (cobertura) for (campo in colunas[-(1:3)]) {
    x <- mensal[[campo]]
    if (!is.numeric(x) || any(!is.na(x) & (!is.finite(x) | x < 0 | x != floor(x)))) stop("Cobertura GRU invalida em ", campo)
  }
  mensal
}

gru_log <- function(config, fase, ..., localidade = NULL) {
  texto <- trimws(gsub("[\r\n]+", " | ", paste0(..., collapse = "")))
  cat(paste0("[PID ", Sys.getpid(), " | origem ", format(config$fim_treino + 1L, "%Y-%m"),
    " | ", fase, if (!is.null(localidade)) paste0(" | ", localidade), "] ", texto, "\n"))
  flush(stdout())
  invisible(NULL)
}

# ---- 6. PREPARACAO CAUSAL DE UMA SERIE, ANTES DO VOCABULARIO GLOBAL ----------
# fim_ajuste e uma POSICAO do calendario comum, nunca uma contagem apos excluir NA.
# Falta de contexto final nao elimina as janelas historicas desta identidade.
preparar_localidade_gru <- function(serie, calendario, fim_ajuste, config) {
  idx <- match(calendario, serie$Data)
  y <- serie$Real[idx]
  n <- length(calendario); l <- as.integer(config$lookback)
  t_index <- seq_len(n)
  conhecidos <- which(t_index <= fim_ajuste & is.finite(y))
  if (length(conhecidos) < 3L) stop("menos de tres valores no prefixo para tendencia quadratica.")
  dados <- data.frame(y = y[conhecidos], t_index = t_index[conhecidos])
  tendencia_modelo <- stats::lm(y ~ t_index + I(t_index^2), data = dados)
  coeficientes <- stats::coef(tendencia_modelo)
  if (tendencia_modelo$rank < 3L || any(!is.finite(coeficientes))) stop("tendencia quadratica nao identificada.")
  tendencia <- as.numeric(coeficientes[1L] + coeficientes[2L] * t_index + coeficientes[3L] * t_index^2)
  if (any(!is.finite(tendencia[conhecidos])) || any(abs(tendencia[conhecidos]) < 1e-10)) {
    stop("tendencia invalida/divisor nulo no prefixo de ajuste.")
  }
  meses <- as.integer(format(calendario, "%m"))
  razao <- y / tendencia
  perfil <- vapply(1:12, function(mes) {
    pos <- conhecidos[meses[conhecidos] == mes]
    if (length(pos)) mean(razao[pos]) else NA_real_
  }, numeric(1L))
  names(perfil) <- as.character(1:12)
  if (any(!is.finite(perfil)) || any(abs(perfil) < 1e-10)) stop("perfil sazonal incompleto ou divisor sazonal nulo.")
  divisores <- tendencia * perfil[as.character(meses)]
  ruido <- y / divisores
  media <- mean(ruido[conhecidos]); desvio <- stats::sd(ruido[conhecidos])
  if (!is.finite(media) || !is.finite(desvio) || desvio < 1e-10) {
    stop("residuo sem variacao valida; nao aplicar ruido, imputacao ou escala artificial.")
  }
  z <- as.numeric((ruido - media) / desvio)
  z[!is.finite(divisores) | abs(divisores) < 1e-10 | !is.finite(z)] <- NA_real_
  candidatos <- seq.int(l + 1L, n)
  validos <- candidatos[vapply(candidatos, function(i) all(is.finite(z[seq.int(i - l, i)])), logical(1L))]
  treino <- validos[validos <= fim_ajuste]
  validacao <- validos[validos > fim_ajuste]
  if (length(treino) < config$minimo_treino) stop(length(treino), " janelas de ajuste; minimo ", config$minimo_treino, ".")
  list(residual_norm = z, datas_historico = calendario,
    indices_treino = treino, indices_validacao = validacao,
    contexto_final_disponivel = length(z) >= l && all(is.finite(tail(z, l))),
    preparacao = list(coef_tendencia = coeficientes, perfil_sazonal = perfil,
      media_res = media, desvio_res = desvio, fim_ajuste = calendario[fim_ajuste],
      tamanho_historico = n))
}

# ---- 7. POPULACAO E TENSORES GLOBAIS [AMOSTRA,TEMPO,ATRIBUTO] -----------------
# N teorico=meses externos-lookback; ultimos N-floor((1-fracao)*N) alvos
# definem um unico corte interno para TODOS os municipios e para o estado.
preparar_treino_global_gru <- function(treino_mensal, config) {
  gru_validar_config(config)
  treino_mensal <- gru_validar_mensal(treino_mensal)
  if (any(treino_mensal$Data < config$inicio_treino | treino_mensal$Data > config$fim_treino)) {
    stop("O calculo global recebe SOMENTE o treino externo; quantidades do teste sao proibidas.")
  }
  calendario <- seq(config$inicio_treino, as.Date(format(config$fim_treino, "%Y-%m-01")), by = "month")
  n <- length(calendario); l <- as.integer(config$lookback)
  N <- n - l
  if (N < 2L) stop("Calendario externo insuficiente para lookback e validacao interna.")
  n_ajuste <- as.integer(floor((1 - config$fracao_validacao) * N))
  n_validacao <- N - n_ajuste
  if (n_ajuste < config$minimo_treino || n_validacao < 1L) stop("Calendario sem janelas teoricas suficientes.")
  fim_ajuste <- l + n_ajuste
  # So nomes/valores do treino participam da elegibilidade; localidades de
  # saida NAO entram neste bloco e nao mudam a rede nem os indicadores.
  disponiveis <- unique(treino_mensal$Localidade)
  disponiveis <- disponiveis[order(gru_chave_localidade(disponiveis))]
  metadados <- list(); avisos <- character()
  for (localidade in disponiveis) {
    serie <- treino_mensal[treino_mensal$Localidade == localidade, c("Data", "Real"), drop = FALSE]
    meta <- tryCatch(preparar_localidade_gru(serie, calendario, fim_ajuste, config), error = function(e) {
      aviso <- paste0(localidade, ": excluida do treino global; ", conditionMessage(e))
      avisos <<- c(avisos, aviso); gru_log(config, "preparacao", aviso)
      NULL
    })
    if (!is.null(meta)) {
      meta$localidade <- localidade
      metadados[[localidade]] <- meta
      if (!length(meta$indices_validacao)) {
        aviso <- paste0(localidade, ": contribui ao ajuste, sem janelas validas na validacao interna.")
        avisos <- c(avisos, aviso); gru_log(config, "preparacao", aviso)
      }
    }
  }
  if (!length(metadados)) stop("Nenhuma identidade elegivel para o treino global.")
  K <- length(metadados); features <- 13L + K
  nt <- sum(vapply(metadados, function(m) length(m$indices_treino), integer(1L)))
  nv <- sum(vapply(metadados, function(m) length(m$indices_validacao), integer(1L)))
  if (!nv) stop("Conjunto global sem amostras de validacao interna.")
  estimativa <- (as.double(nt + nv) * l * features + nt + nv) * 8 / 1024^2
  gru_log(config, "tensores", K, " identidades; treino[", nt, ",", l, ",", features,
    "]; validacao[", nv, ",", l, ",", features, "]; arrays R ~", round(estimativa, 1L),
    " MiB, sem incluir copias TensorFlow. Reduza processos se necessario.")
  gru_log(config, "populacao", paste(names(metadados), collapse = ", "))
  x_treino <- array(NA_real_, dim = c(nt, l, features)); y_treino <- numeric(nt)
  x_validacao <- array(NA_real_, dim = c(nv, l, features)); y_validacao <- numeric(nv)
  it <- 0L; iv <- 0L
  for (k in seq_along(metadados)) {
    meta <- metadados[[k]]; meta$local_index <- k
    identidade <- matrix(0, nrow = l, ncol = K); identidade[, k] <- 1
    meses <- as.integer(format(meta$datas_historico, "%m"))
    for (i in meta$indices_treino) {
      janela <- seq.int(i - l, i - 1L); it <- it + 1L
      x_treino[it, , ] <- cbind(meta$residual_norm[janela], diag(12L)[meses[janela], , drop = FALSE], identidade)
      y_treino[it] <- meta$residual_norm[i]
    }
    for (i in meta$indices_validacao) {
      janela <- seq.int(i - l, i - 1L); iv <- iv + 1L
      x_validacao[iv, , ] <- cbind(meta$residual_norm[janela], diag(12L)[meses[janela], , drop = FALSE], identidade)
      y_validacao[iv] <- meta$residual_norm[i]
    }
    metadados[[k]] <- meta
  }
  if (any(!is.finite(x_treino)) || any(!is.finite(y_treino)) ||
      any(!is.finite(x_validacao)) || any(!is.finite(y_validacao))) stop("Tensores GRU nao finitos.")
  list(x_treino = x_treino, y_treino = y_treino, x_validacao = x_validacao, y_validacao = y_validacao,
    metadados = metadados, localidades_treino = names(metadados), n_features = features,
    corte_interno = list(fim_ajuste = calendario[fim_ajuste], inicio_validacao = calendario[fim_ajuste + 1L],
      n_amostras_teoricas = N, n_validacao_teoricas = n_validacao), avisos = avisos)
}

# ---- 8. BACKEND CPU: SOMENTE EM PROCESSO DEDICADO E ANTES DE TF/KERAS ---------
inicializar_backend_gru <- function(config) {
  threads <- config$threads_treino
  if (!gru_inteiro(threads, 1L)) stop("threads_treino deve ser inteiro positivo.")
  # O pai deve propagar o caminho concreto; nunca permitir que um worker
  # escolha outro Python ou crie um ambiente gerenciado silenciosamente.
  python <- gru_validar_python(config[["python"]])
  Sys.setenv(KERAS_BACKEND = "tensorflow", CUDA_VISIBLE_DEVICES = "-1", TF_CPP_MIN_LOG_LEVEL = "2",
    RETICULATE_USE_MANAGED_VENV = "no", RETICULATE_PYTHON = python, KERAS_PYTHON = python,
    TF_ENABLE_ONEDNN_OPTS = "0", TF_NUM_INTRAOP_THREADS = as.character(threads),
    TF_NUM_INTEROP_THREADS = "1", OMP_NUM_THREADS = as.character(threads),
    OPENBLAS_NUM_THREADS = as.character(threads), MKL_NUM_THREADS = as.character(threads))
  for (p in c("reticulate", "tensorflow")) if (!requireNamespace(p, quietly = TRUE)) stop("Pacote ausente no R local: ", p)
  reticulate::use_python(python, required = TRUE)
  tf <- tensorflow::tf
  tryCatch({
    intra <- as.integer(tf$config$threading$get_intra_op_parallelism_threads())
    inter <- as.integer(tf$config$threading$get_inter_op_parallelism_threads())
    if (intra != threads) tf$config$threading$set_intra_op_parallelism_threads(as.integer(threads))
    if (inter != 1L) tf$config$threading$set_inter_op_parallelism_threads(1L)
    if (length(tf$config$get_visible_devices("GPU"))) tf$config$set_visible_devices(list(), "GPU")
  }, error = function(e) stop("TensorFlow ja inicializado com recursos incompativeis: ",
    conditionMessage(e), ". Use um processo R fresco; limites TF nao sao livremente reversiveis."))
  if (!requireNamespace("keras3", quietly = TRUE)) stop("Pacote ausente no R local: keras3")
  invisible(NULL)
}

gru_seed_origem <- function(config) {
  valor <- as.double(config$seed)
  for (codigo in utf8ToInt(format(config$fim_treino, "%Y-%m-%d"))) valor <- (valor * 131 + codigo) %% 2147483647
  as.integer(valor)
}

inicializar_semente_gru <- function(config) {
  seed <- gru_seed_origem(config)
  set.seed(seed)
  keras3::set_random_seed(seed)
  seed
}

limpar_sessao_gru <- function() {
  if ("keras3" %in% loadedNamespaces()) try(keras3::clear_session(), silent = TRUE)
  invisible(gc(verbose = FALSE))
}

ajustar_gru <- function(preparado, config) {
  a <- config$parametros
  modelo <- keras3::keras_model_sequential(input_shape = c(as.integer(config$lookback), preparado$n_features))
  modelo <- keras3::layer_gru(modelo, units = as.integer(a$unidades))
  modelo <- keras3::layer_dense(modelo, units = 1L)
  keras3::compile(modelo, loss = "mse", optimizer = keras3::optimizer_adam(learning_rate = a$taxa_aprendizado))
  history <- keras3::fit(modelo, x = preparado$x_treino, y = matrix(preparado$y_treino, ncol = 1L),
    epochs = as.integer(a$epocas), batch_size = as.integer(a$tamanho_lote),
    validation_data = list(preparado$x_validacao, matrix(preparado$y_validacao, ncol = 1L)),
    shuffle = FALSE, verbose = as.integer(a$verbose))
  loss <- as.numeric(history$metrics[["loss"]]); val_loss <- as.numeric(history$metrics[["val_loss"]])
  if (!length(loss) || length(loss) != length(val_loss)) stop("Historico GRU sem perdas de treino/validacao correspondentes.")
  list(modelo = modelo, perdas = data.frame(Modelo = "GRU", Localidade = "Global",
    Epoca = seq_along(loss), Loss = loss, Val_Loss = val_loss))
}

# ---- 9. RECURSAO CONTINUA POR LOCALIDADE DA MESMA REDE GLOBAL ----------------
prever_recursivo_gru <- function(modelo, meta, datas, n_features, n_localidades, config,
    preditor = function(modelo, x) as.numeric(keras3::predict_on_batch(modelo, x = x))) {
  l <- as.integer(config$lookback)
  esperadas <- seq(tail(meta$datas_historico, 1L), by = "month", length.out = length(datas) + 1L)[-1L]
  if (!inherits(datas, "Date") || anyNA(datas) || !identical(as.numeric(datas), as.numeric(esperadas))) {
    stop("Recursao deve iniciar no mes seguinte ao corte externo e seguir sem interrupcoes.")
  }
  if (length(meta$residual_norm) < l || any(!is.finite(tail(meta$residual_norm, l)))) stop("Contexto final incompleto; sem imputacao.")
  if (n_features != 13L + n_localidades || meta$local_index < 1L || meta$local_index > n_localidades) stop("Identidade/dimensoes GRU inconsistentes.")
  id <- numeric(n_localidades); id[meta$local_index] <- 1
  id_matriz <- matrix(rep(id, each = l), nrow = l)
  meses <- as.integer(format(tail(meta$datas_historico, l), "%m"))
  sequencia <- cbind(tail(meta$residual_norm, l), diag(12L)[meses, , drop = FALSE], id_matriz)
  preparo <- meta$preparacao; t_index <- length(meta$datas_historico) + seq_along(datas)
  co <- preparo$coef_tendencia
  tendencia <- as.numeric(co[1L] + co[2L] * t_index + co[3L] * t_index^2)
  fatores <- as.numeric(preparo$perfil_sazonal[as.character(as.integer(format(datas, "%m")))])
  if (any(!is.finite(tendencia)) || any(abs(tendencia) < 1e-10) ||
      any(!is.finite(fatores)) || any(abs(fatores) < 1e-10)) stop("Tendencia/sazonalidade futura nao permite a reconstrucao.")
  previstos <- numeric(length(datas))
  for (i in seq_along(datas)) {
    x <- array(sequencia, dim = c(1L, l, n_features))
    z <- preditor(modelo, x)
    if (length(z) != 1L || !is.finite(z)) stop("Saida GRU nao finita no horizonte ", i, ".")
    previstos[i] <- tendencia[i] * fatores[i] * (preparo$media_res + preparo$desvio_res * z)
    # Somente z PREVISTO e indicadores conhecidos; nenhum Real do teste.
    nova <- c(z, diag(12L)[as.integer(format(datas[i], "%m")), ], id)
    sequencia <- rbind(sequencia[-1L, , drop = FALSE], nova)
  }
  if (any(!is.finite(previstos))) stop("Previsao GRU reconstruida nao finita.")
  previstos # Sem log, truncamento ou coeficiente de crescimento imposto.
}

# ---- 10. CALCULO DE UMA ORIGEM: SEM LEITURA/GRAVACAO E SEM CLUSTER ANINHADO ----
calcular_origem_gru <- function(config, treino_mensal) {
  inicio <- proc.time()[["elapsed"]]
  config <- utils::modifyList(config_gru_global, config, keep.null = TRUE)
  gru_validar_config(config)
  treino_mensal <- gru_validar_mensal(treino_mensal)
  # Conferir antes de carregar backend; rejeitar a passagem acidental do teste.
  if (any(treino_mensal$Data < config$inicio_treino | treino_mensal$Data > config$fim_treino)) stop("Calculo GRU recebeu valores fora do treino externo.")
  opcoes <- options(OutDec = ".")
  on.exit(options(opcoes), add = TRUE)
  preparado <- NULL; ajuste <- NULL; previsoes <- NULL
  on.exit({
    ajuste <- NULL; preparado <- NULL
    limpar_sessao_gru()
  }, add = TRUE)
  preparado <- preparar_treino_global_gru(treino_mensal[c("Localidade", "Data", "Real")], config)
  selecionadas <- gru_localidades(treino_mensal, config$localidades)
  estado_solicitado <- "PARA GERAL" %in% gru_chave_localidade(selecionadas)
  elegiveis <- gru_chave_localidade(preparado$localidades_treino)
  saidas <- selecionadas[gru_chave_localidade(selecionadas) %in% elegiveis]
  avisos <- preparado$avisos
  indisponiveis <- setdiff(gru_chave_localidade(selecionadas), elegiveis)
  if (length(indisponiveis)) avisos <- c(avisos, paste0("Sem identidade treinada para saida: ", paste(indisponiveis, collapse = ", "), "."))
  if (estado_solicitado && !"PARA GERAL" %in% elegiveis) stop("Para (Geral) solicitado sem identidade elegivel; origem interrompida.")
  if (!length(saidas)) stop("Nenhuma identidade solicitada pode emitir previsao.")
  inicializar_backend_gru(config)
  limpar_sessao_gru()
  seed <- inicializar_semente_gru(config)
  gru_log(config, "ajuste", "uma rede global; seed ", seed, "; corte interno ", preparado$corte_interno$fim_ajuste)
  inicio_fit <- proc.time()[["elapsed"]]
  ajuste <- ajustar_gru(preparado, config)
  # A inferencia usa apenas metadados; liberar os grandes arrays globais
  # antes da recursao reduz a sobreposicao de memoria R/TensorFlow.
  preparado$x_treino <- preparado$y_treino <- NULL
  preparado$x_validacao <- preparado$y_validacao <- NULL
  invisible(gc(verbose = FALSE))
  segundos_fit <- proc.time()[["elapsed"]] - inicio_fit
  gru_log(config, "ajuste", "concluido em ", round(segundos_fit, 2L), " s")
  p <- gru_periodos(config)
  datas <- seq(p$inicio_teste, by = "month", length.out = p$meses_teste + as.integer(config$passos_futuro))
  resultados <- list()
  for (localidade in saidas) {
    meta <- preparado$metadados[[match(gru_chave_localidade(localidade), elegiveis)]]
    if (!meta$contexto_final_disponivel) {
      aviso <- paste0(localidade, ": contexto final de ", config$lookback, " meses incompleto; historico manteve sua contribuicao ao ajuste.")
      avisos <- c(avisos, aviso); gru_log(config, "previsao", aviso)
      if (gru_chave_localidade(localidade) == "PARA GERAL" && estado_solicitado) stop(aviso)
      next
    }
    gru_log(config, "previsao", length(datas), " passos recursivos continuos", localidade = localidade)
    valores <- tryCatch(prever_recursivo_gru(ajuste$modelo, meta, datas,
      preparado$n_features, length(elegiveis), config), error = function(e) {
      if (gru_chave_localidade(localidade) == "PARA GERAL" && estado_solicitado) stop(e)
      aviso <- paste0(localidade, ": previsao ignorada; ", conditionMessage(e))
      avisos <<- c(avisos, aviso); gru_log(config, "previsao", aviso)
      NULL
    })
    if (!is.null(valores)) resultados[[localidade]] <- data.frame(Localidade = localidade,
      Data = datas, Previsao_GRU = valores)
  }
  if (!length(resultados)) stop("Nenhuma previsao GRU disponivel nesta origem.")
  previsoes <- do.call(rbind, resultados); rownames(previsoes) <- NULL
  perdas <- ajuste$perdas
  total <- unname(proc.time()[["elapsed"]] - inicio)
  gru_log(config, "origem", "concluida; ", length(resultados), " localidades previstas; ", round(total, 2L), " s")
  # Somente dados R simples: nenhuma rede/tensor/callback cruza o PSOCK.
  list(previsoes = previsoes, perdas = perdas, avisos = avisos,
    localidades_treino = preparado$localidades_treino,
    tempos = list(total = total, ajuste = unname(segundos_fit)), seed = seed,
    corte_interno = preparado$corte_interno)
}

# ---- 11. PAI: JUNTAR REAL/CALENDARIO/COBERTURA SEM ALTERAR A PREVISAO ---------
compor_csv_gru <- function(dados_mensais, previsoes, config) {
  dados_mensais <- gru_validar_mensal(dados_mensais, cobertura = TRUE)
  p <- gru_periodos(config)
  if (!is.data.frame(previsoes) || !all(c("Localidade", "Data", "Previsao_GRU") %in% names(previsoes)) || !nrow(previsoes)) stop("Previsoes GRU sem esquema mensal.")
  previsoes$Data <- as.Date(previsoes$Data)
  if (anyNA(previsoes$Data) || any(!is.finite(previsoes$Previsao_GRU)) ||
      anyDuplicated(data.frame(Localidade = gru_chave_localidade(previsoes$Localidade), Data = previsoes$Data))) stop("Previsoes GRU invalidas/duplicadas.")
  horizonte <- seq(p$inicio_teste, by = "month", length.out = p$meses_teste + as.integer(config$passos_futuro))
  calendario <- seq(p$inicio_treino, tail(horizonte, 1L), by = "month")
  resultados <- lapply(unique(previsoes$Localidade), function(localidade) {
    parte <- dados_mensais[gru_chave_localidade(dados_mensais$Localidade) == gru_chave_localidade(localidade), , drop = FALSE]
    if (!nrow(parte)) stop("Localidade prevista ausente na base mensal: ", localidade)
    prev <- previsoes[previsoes$Localidade == localidade, , drop = FALSE]
    prev <- prev[order(prev$Data), , drop = FALSE]
    if (!identical(as.numeric(prev$Data), as.numeric(horizonte))) stop("Horizonte incompleto na previsao de ", localidade)
    idx <- match(calendario, parte$Data)
    real <- parte$Real[idx]; futuro <- calendario > p$fim_teste
    real[futuro] <- NA_real_
    cobertura <- lapply(c("Municipios_com_registro", "Municipios_com_total_conhecido", "Municipios_esperados"), function(campo) {
      x <- parte[[campo]][idx]; x[futuro] <- NA_real_; x
    })
    data.frame(Localidade = localidade, Data = calendario, Real = real,
      Previsao_GRU = prev$Previsao_GRU[match(calendario, prev$Data)], AIC = NA_real_, BIC = NA_real_, Modelo = "GRU",
      Particao = ifelse(calendario < p$inicio_teste, "treino", ifelse(futuro, "futuro", "teste")),
      Municipios_com_registro = cobertura[[1L]], Municipios_com_total_conhecido = cobertura[[2L]], Municipios_esperados = cobertura[[3L]])
  })
  resultado <- do.call(rbind, resultados); rownames(resultado) <- NULL
  resultado
}

# ---- 12. PSOCK: UM BACKEND POR PROCESSO, SEM SERIALIZAR OBJETOS PYTHON --------
# Conferir o contrato antes de importar o arquivo no processo novo. O legado
# executava IO/treino no topo e nao pode ser carregado apenas com uma opcao.
gru_conferir_script <- function(caminho) {
  if (!file.exists(caminho) || dir.exists(caminho)) stop("Produtor GRU nao encontrado: ", caminho)
  expressoes <- as.list(parse(file = caminho, encoding = "UTF-8", keep.source = FALSE))
  atribuicoes <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) && is.symbol(x[[2L]]), expressoes)
  definicoes <- stats::setNames(lapply(atribuicoes, function(x) x[[3L]]),
    vapply(atribuicoes, function(x) as.character(x[[2L]]), character(1L)))
  guarda <- quote(if (!isTRUE(getOption("frota.gru.apenas_funcoes", FALSE))) executar_gru_global())
  obrigatorias <- c("calcular_origem_gru", "inicializar_backend_gru", "executar_gru_global")
  possui_funcoes <- all(vapply(obrigatorias, function(n) {
    x <- definicoes[[n]]; is.call(x) && identical(x[[1L]], as.name("function"))
  }, logical(1L)))
  possui_guarda <- any(vapply(expressoes, function(x) identical(x, guarda), logical(1L)))
  if (!identical(definicoes[["gru_versao"]], "GRU-GLOBAL-1-CAUSAL-VALIDACAO") || !possui_funcoes || !possui_guarda) {
    stop("Atualize o produtor GRU causal junto com o controlador; legado sem guarda foi rejeitado antes de source().")
  }
  invisible(TRUE)
}

criar_cluster_gru <- function(processos, script_pre_compilacao, raiz = getwd(), python = NULL, threads = 1L) {
  if (!gru_inteiro(processos, 1L) || !gru_inteiro(threads, 1L)) stop("processos e threads devem ser inteiros positivos.")
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  script <- normalizePath(script_pre_compilacao, winslash = "/", mustWork = TRUE)
  gru_conferir_script(script)
  nomes <- c("KERAS_BACKEND", "KERAS_PYTHON", "RETICULATE_PYTHON", "CUDA_VISIBLE_DEVICES", "TF_CPP_MIN_LOG_LEVEL",
    "RETICULATE_USE_MANAGED_VENV",
    "TF_ENABLE_ONEDNN_OPTS", "TF_NUM_INTRAOP_THREADS", "TF_NUM_INTEROP_THREADS", "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS")
  anteriores <- Sys.getenv(nomes, unset = NA_character_)
  on.exit({
    ausentes <- is.na(anteriores)
    if (any(ausentes)) Sys.unsetenv(nomes[ausentes])
    if (any(!ausentes)) do.call(Sys.setenv, as.list(anteriores[!ausentes]))
  }, add = TRUE)
  novos <- stats::setNames(rep(as.character(threads), length(nomes)), nomes)
  novos[["KERAS_BACKEND"]] <- "tensorflow"; novos[["CUDA_VISIBLE_DEVICES"]] <- "-1"
  novos[["RETICULATE_USE_MANAGED_VENV"]] <- "no"
  novos[["TF_NUM_INTEROP_THREADS"]] <- "1"; novos[["TF_CPP_MIN_LOG_LEVEL"]] <- "2"; novos[["TF_ENABLE_ONEDNN_OPTS"]] <- "0"
  novos <- novos[!names(novos) %in% c("KERAS_PYTHON", "RETICULATE_PYTHON")]
  do.call(Sys.setenv, as.list(novos))
  if (!is.null(python)) Sys.setenv(RETICULATE_PYTHON = python, KERAS_PYTHON = python)
  cluster <- parallel::makePSOCKcluster(as.integer(processos), outfile = "")
  pronto <- FALSE
  on.exit(if (!pronto) try(parallel::stopCluster(cluster), silent = TRUE), add = TRUE)
  inicializar <- function(raiz, script, python, threads, bibliotecas) {
    setwd(raiz); .libPaths(bibliotecas)
    opcoes <- options(frota.gru.apenas_funcoes = TRUE, OutDec = ".")
    on.exit(options(opcoes), add = TRUE)
    produtor <- new.env(parent = globalenv())
    source(script, local = produtor, encoding = "UTF-8")
    produtor$inicializar_backend_gru(list(python = python, threads_treino = threads))
    assign(".frota_gru_produtor", produtor, envir = globalenv())
    invisible(NULL)
  }
  environment(inicializar) <- baseenv()
  invisible(parallel::clusterCall(cluster, inicializar, raiz, script, python, as.integer(threads), .libPaths()))
  pronto <- TRUE
  cluster
}

# ---- 13. PRODUCAO: UM WORKER FRESCO, UMA REDE E ESCRITA SOMENTE NO PAI --------
gru_caminho_canonico <- function(caminho) {
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
  faltantes <- character(); existente <- caminho
  while (!file.exists(existente) && !dir.exists(existente)) {
    pai <- dirname(existente)
    if (identical(pai, existente)) break
    faltantes <- c(basename(existente), faltantes); existente <- pai
  }
  existente <- normalizePath(existente, winslash = "/", mustWork = FALSE)
  if (length(faltantes)) existente <- paste(c(existente, faltantes), collapse = "/")
  tolower(existente)
}

executar_gru_global <- function(config = config_gru_global, dados_mensais = NULL) {
  inicio <- proc.time()[["elapsed"]]; cluster <- NULL
  opcoes <- options(OutDec = ".")
  on.exit({
    if (!is.null(cluster)) tryCatch(parallel::stopCluster(cluster), error = function(e) message(conditionMessage(e)))
    options(opcoes)
    segundos <- proc.time()[["elapsed"]] - inicio; inteiros <- floor(segundos)
    message("TEMPO TOTAL DO PRE-COMPILADOR GRU: ", sprintf("%02.0f:%02.0f:%02.0f",
      inteiros %/% 3600, (inteiros %% 3600) %/% 60, inteiros %% 60), " (", round(segundos, 2L), " s).")
    flush.console()
  }, add = TRUE)
  config <- utils::modifyList(config_gru_global, config, keep.null = TRUE)
  gru_validar_config(config); p <- gru_periodos(config)
  saidas <- vapply(config[c("saida", "historico_perdas")], gru_caminho_canonico, character(1L))
  if (anyDuplicated(saidas) || any(saidas %in% vapply(config[c("entrada", "script_pre_compilacao")], gru_caminho_canonico, character(1L)))) stop("Entrada, scripts e saidas GRU devem ser distintos.")
  config$python <- resolver_python_gru(config)
  mensal <- gru_validar_mensal(if (is.null(dados_mensais)) ler_mensal_gru(config) else dados_mensais, cobertura = TRUE)
  treino <- mensal[mensal$Data >= config$inicio_treino & mensal$Data <= config$fim_treino,
    c("Localidade", "Data", "Real"), drop = FALSE]
  cluster <- criar_cluster_gru(1L, config$script_pre_compilacao, getwd(), config$python, config$threads_treino)
  trabalhar <- function(config, treino) {
    produtor <- get(".frota_gru_produtor", envir = globalenv(), inherits = FALSE)
    produtor$calcular_origem_gru(config, treino)
  }
  environment(trabalhar) <- baseenv()
  resultado <- parallel::clusterCall(cluster, trabalhar, config, treino)[[1L]]
  csv <- compor_csv_gru(mensal, resultado$previsoes, config)
  gravar_csv_gru(csv, config$saida)
  if (isTRUE(config$salvar_historicos)) gravar_csv_gru(resultado$perdas, config$historico_perdas)
  message("GRU_GLOBAL_CONCLUIDO: ", length(unique(csv$Localidade)), " localidades; ", config$saida)
  invisible(list(previsoes = csv, perdas = resultado$perdas, avisos = resultado$avisos,
    localidades_treino = resultado$localidades_treino))
}

# ---- 14. ENTRADA: IMPORTACAO SEM IO/BACKEND OU EXECUCAO LOCAL -----------------
if (!isTRUE(getOption("frota.gru.apenas_funcoes", FALSE))) executar_gru_global()

