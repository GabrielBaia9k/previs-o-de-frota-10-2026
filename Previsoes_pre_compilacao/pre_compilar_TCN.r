# source("Previsoes_pre_compilacao/pre_compilar_TCN.r", encoding = "UTF-8")

# TCN: registros mensais por municipio e PARA (Geral).
# Revisao: TCN-KERAS3-LOCAL-ESTADO-3-VALIDACAO-R; previsoes e perdas por epoca.
# Base consultada: GitHub publicacao-codigo, 91e519d5521336790484a77be963039a24c05b65.
# Executar EXCLUSIVAMENTE no R local, com a raiz do projeto como diretorio:
# source("Previsoes_pre_compilacao/pre_compilar_TCN.r", encoding = "UTF-8")
# Ou: Rscript --vanilla Previsoes_pre_compilacao/pre_compilar_TCN.r
# Pacotes R, caso ausentes, instalar manualmente na maquina local:
# install.packages(c("keras3", "tensorflow", "reticulate"),
#                  repos = "http://cran.rstudio.com/")
# Usar o ambiente Python/TensorFlow que ja executa LSTM/GRU. O produtor nao
# chama instaladores, nao carrega global.R e nao altera o frontend.
#
# PIPELINE: validar -> agregar/calendario -> separar treino -> padronizar
# -> janelas causais -> ajustar TCN -> recursao mensal -> escala original -> CSV.
# Alvo: quantidade de REGISTROS no mes, somando os tipos presentes; nao estoque.
# Representacao inicial: nivel padronizado, sem log/diferencas/tendencia externa.
# Antes de2025, separar ajuste/validacao; media/desvio usam SOMENTE o ajuste.
# Uma rede por municipio e outra para a soma estadual observada, independente.
# Um ajuste anterior a2025 gera84passos continuos (teste2025 + futuro2026-2031).
# As ultimas10%janelas pre2025 validam e NAO atualizam pesos; nao ha refit.
# Real/2025 serve somente para exportacao/avaliacao posterior no Shiny.
# Zero explicito e observado; mes sem linha continua NA. Nao ha imputacao.
# Janelas com ausencias nao treinam; a ultima janela pre2025 precisa ser completa.
# Dois arquivos: CSV mensal e data/historico_perdas_tcn.csv (Loss/Val_Loss).
# Sem backtest movel, MAE/RMSE/MAPE offline, modelos salvos ou gradientes.
# Referencias de arquitetura/API:
# https://arxiv.org/abs/1803.01271
# https://github.com/locuslab/TCN/blob/master/TCN/tcn.py
# https://github.com/rstudio/keras3/blob/main/man/layer_conv_1d.Rd

# =============================================================================
# ETAPA 01 — CONFIGURACAO E VERIFICACOES DO AMBIENTE LOCAL
# Os parametros abaixo sao candidatos iniciais, nao valores otimizados.
# minimo_treino conta JANELAS DE AJUSTE, depois da reserva de validacao.
# =============================================================================
config_tcn_local <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  saida = "Previsoes_output/previsoes_tcn_pre_comp.csv",
  arquivo_historico = "data/historico_perdas_tcn.csv",
  ano_inicio = 2016L, ano_teste = 2025L, ano_fim = 2025L,
  n_teste = 12L, passos_futuro = 72L,
  lookback = 24L, minimo_treino = 24L, validacao_interna = 0.10, seed = 42L,
  parametros = list(
    filtros = 4L, kernel_size = 2L, dilatacoes = c(1L, 2L, 4L, 8L),
    dropout = 0.1, taxa_aprendizado = 0.001,
    epocas = 100L, tamanho_lote = 8L, verbose = 0L
  )
)

validar_config_tcn <- function(config) {
  inteiro <- function(x, minimo = 1L) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x == floor(x) && x >= minimo && x <= .Machine$integer.max
  }
  for (nome in c("ano_inicio", "ano_teste", "ano_fim", "n_teste",
      "passos_futuro", "lookback", "minimo_treino")) {
    if (!inteiro(config[[nome]])) stop("Configuracao inteira invalida: ", nome)
  }
  if (!inteiro(config$seed, 0L)) stop("seed invalida.")
  if (config$ano_inicio >= config$ano_teste || config$ano_fim != config$ano_teste ||
      config$ano_fim + ceiling(config$passos_futuro / 12) > 9999L ||
      config$n_teste != 12L || config$lookback < 2L || config$minimo_treino < 24L) {
    stop("Conferir anos, teste de12meses, lookback>=2 e minimo_treino>=24janelas.")
  }
  if (!is.numeric(config$validacao_interna) || length(config$validacao_interna) != 1L ||
      !is.finite(config$validacao_interna) || config$validacao_interna <= 0 ||
      config$validacao_interna >= 1) stop("validacao_interna deve estar entre0 e1.")
  for (nome in c("entrada", "saida", "arquivo_historico")) {
    x <- config[[nome]]
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop("Caminho invalido: ", nome)
    }
  }
  caminhos <- vapply(config[c("entrada", "saida", "arquivo_historico")],
    normalizePath, character(1L), winslash = "/", mustWork = FALSE)
  if (anyDuplicated(caminhos)) {
    stop("Entrada, previsoes e historico devem ser arquivos diferentes.")
  }
  if (dir.exists(config$saida) || dir.exists(config$arquivo_historico)) {
    stop("Saidas devem ser arquivos CSV, nao diretorios.")
  }
  p <- config$parametros
  for (nome in c("filtros", "kernel_size", "epocas", "tamanho_lote")) {
    if (!inteiro(p[[nome]])) stop("Parametro inteiro invalido: ", nome)
  }
  d <- p$dilatacoes
  if (!is.numeric(d) || !length(d) || any(!is.finite(d)) ||
      any(d < 1 | d != floor(d) | d > .Machine$integer.max)) {
    stop("dilatacoes deve conter inteiros positivos.")
  }
  if (!is.numeric(p$dropout) || length(p$dropout) != 1L ||
      !is.finite(p$dropout) || p$dropout < 0 || p$dropout >= 1 ||
      !is.numeric(p$taxa_aprendizado) || length(p$taxa_aprendizado) != 1L ||
      !is.finite(p$taxa_aprendizado) || p$taxa_aprendizado <= 0 ||
      !inteiro(p$verbose, 0L) || !p$verbose %in% 0:2) {
    stop("Conferir dropout [0,1), taxa_aprendizado>0 e verbose em0:2.")
  }
  campo <- 1 + 2 * (p$kernel_size - 1) * sum(d)
  if (!is.finite(campo) || campo < config$lookback) {
    stop("Campo receptivo menor que lookback; ampliar dilatacoes/kernel ou reduzir a janela.")
  }
  invisible(config)
}

verificar_backend_tcn <- function() {
  pacotes <- c("keras3", "tensorflow", "reticulate")
  ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(ausentes)) {
    stop("Instale no R local: install.packages(c(",
      paste(sprintf('"%s"', ausentes), collapse = ", "),
      '), repos = "http://cran.rstudio.com/")')
  }
  tryCatch({
    if (!reticulate::py_module_available("tensorflow")) {
      stop("Modulo Python tensorflow nao encontrado no ambiente selecionado.")
    }
    backend <- keras3::config_backend()
    if (!identical(backend, "tensorflow")) {
      stop("Backend keras3 atual: ", backend, "; este produtor requer tensorflow.")
    }
    tf <- tensorflow::tf
    invisible(tf$constant(0))
    message("Backend local: R/keras3 ", utils::packageVersion("keras3"),
      "; Python/TensorFlow ", tf$`__version__`, ".")
  }, error = function(e) {
    stop("Backend TCN indisponivel: ", conditionMessage(e),
      " Use o ambiente Python/TensorFlow dos produtores LSTM/GRU. ",
      "Confira reticulate::py_config() e tensorflow::tf_config() no R local.",
      call. = FALSE)
  })
  invisible(TRUE)
}

# =============================================================================
# ETAPA 02 — LEITURA E VALIDACAO DO CSV DE ENTRADA
# Rejeitar datas/quantidades invalidas e chaves duplicadas antes de somar.
# NA e zero tem significados distintos; quantidades negativas nao sao registros.
# =============================================================================
ler_mensal_tcn <- function(config) {
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
  # ETAPA 03 — AGREGACAO E CALENDARIO MENSAL REGULAR
  # Somar tipos por municipio/mes; qualquer NA explicito torna seu total NA.
  # Estado = soma dos municipios observados, com cobertura registrada; nao e
  # estimativa dos municipios ausentes. Nao completar lacunas com zero.
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
  estado <- stats::aggregate(mensal$Real, by = mensal["Data"],
    FUN = function(x) if (anyNA(x)) NA_real_ else sum(x))
  names(estado)[2L] <- "Real"
  registros <- stats::aggregate(rep(1L, nrow(mensal)), by = mensal["Data"], FUN = sum)
  conhecidos <- stats::aggregate(as.integer(is.finite(mensal$Real)),
    by = mensal["Data"], FUN = sum)
  estado <- completar("PARÁ (Geral)", estado, length(municipios),
    registros$x[match(estado$Data, registros$Data)],
    conhecidos$x[match(estado$Data, conhecidos$Data)])
  saida <- do.call(rbind, c(list(estado), resultado))
  rownames(saida) <- NULL
  saida
}

calendario_tcn <- function(datas) {
  mes <- as.integer(format(as.Date(datas), "%m"))
  cbind(mes_seno = sin(2 * pi * (mes - 1L) / 12),
    mes_cosseno = cos(2 * pi * (mes - 1L) / 12))
}

# =============================================================================
# ETAPA 04 — CORTE TEMPORAL INTERNO E ESCALA ESTIMADA SOMENTE NO AJUSTE
# Entrada desta funcao: APENAS o historico anterior ao teste.
# Identificar janelas completas e reservar as ultimas para validacao ANTES
# de calcular a escala. z[t]=(y[t]-media_ajuste)/desvio_ajuste.
# Serie constante no ajuste usa denominador1. Nenhum valor reservado ou2025
# participa da estimacao da escala. Sem log/diferencas/tendencia externa.
# =============================================================================
preparar_treino_tcn <- function(historico, config) {
  datas <- as.Date(historico$Data)
  if (!length(datas) || anyNA(datas) || anyDuplicated(datas) ||
      !identical(datas, seq(datas[1L], by = "month", length.out = length(datas))) ||
      any(format(datas, "%d") != "01")) {
    stop("Treino deve ter calendario mensal regular, ordenado e sem duplicatas.")
  }
  corte <- as.Date(sprintf("%d-01-01", config$ano_teste))
  if (any(datas >= corte)) stop("Treino contem datas do teste/futuro.")
  if (tail(datas, 1L) != seq(corte, by = "-1 month", length.out = 2L)[2L]) {
    stop("Calendario de treino nao termina em dezembro anterior ao teste.")
  }
  y <- historico$Real; n <- length(y); l <- as.integer(config$lookback)
  if (n <= l) stop("Treino menor ou igual ao lookback.")
  if (any(!is.finite(tail(y, l)))) {
    stop("Ultima janela de", l, "meses do treino incompleta; sem imputacao.")
  }
  candidatos <- seq.int(l + 1L, n)
  validas <- vapply(candidatos, function(t) {
    all(is.finite(y[seq.int(t - l, t)]))
  }, logical(1L))
  alvos <- candidatos[validas]
  n_ajuste <- floor(length(alvos) * (1 - config$validacao_interna))
  n_validacao <- length(alvos) - n_ajuste
  if (n_ajuste < config$minimo_treino || n_validacao < 1L) {
    stop("Apenas ", length(alvos), " janelas completas: ", n_ajuste,
      " para ajuste e ", n_validacao, " para validacao; minimo de ajuste = ",
      config$minimo_treino, ".")
  }
  ajuste <- seq_len(n_ajuste)
  validacao <- seq.int(n_ajuste + 1L, length(alvos))
  limite_escala <- alvos[n_ajuste]
  y_ajuste <- y[seq_len(limite_escala)]
  conhecidos <- is.finite(y_ajuste)
  media <- mean(y_ajuste[conhecidos]); desvio <- stats::sd(y_ajuste[conhecidos])
  if (!is.finite(media) || !is.finite(desvio)) stop("Escala do ajuste nao finita.")
  escala_unitaria <- desvio == 0
  if (escala_unitaria) desvio <- 1
  z <- (y - media) / desvio
  atributos <- cbind(quantidade = z, calendario_tcn(datas))

  # ==========================================================================
  # ETAPA 05 — JANELAS SUPERVISIONADAS CAUSAIS E TENSORES
  # Prever t usando [t-lookback,...,t-1], sem o proprio alvo na entrada.
  # Tensor: [amostra,tempo,atributo] = [N,lookback,3]. Preencher indices
  # explicitamente evita misturar amostras/atributos com unlist()/array().
  # NA elimina apenas a janela afetada; o calendario nao e compactado.
  # Validacao de UM PASSO: pode usar observacoes anteriores do proprio bloco
  # validado como contexto. Nao e validacao da recursao de84meses.
  # ==========================================================================
  x <- array(NA_real_, dim = c(length(alvos), l, 3L))
  for (i in seq_along(alvos)) {
    indices <- seq.int(alvos[i] - l, alvos[i] - 1L)
    x[i, , ] <- atributos[indices, , drop = FALSE]
  }
  alvo <- as.numeric(z[alvos])
  if (any(!is.finite(x)) || any(!is.finite(alvo))) stop("Tensor de treino nao finito.")
  list(x = x[ajuste, , , drop = FALSE], y = alvo[ajuste],
    x_validacao = x[validacao, , , drop = FALSE], y_validacao = alvo[validacao],
    datas_alvos = datas[alvos[ajuste]], datas_validacao = datas[alvos[validacao]],
    ultima_entrada = atributos[seq.int(n - l + 1L, n), , drop = FALSE],
    media = media, desvio = desvio, escala_unitaria = escala_unitaria,
    janelas_descartadas = sum(!validas))
}

# =============================================================================
# ETAPA 06 — ARQUITETURA TCN: CONVOLUCOES CAUSAIS, DILATADAS E RESIDUAIS
# Duas Conv1D por bloco; padding causal impede acesso a tempos posteriores.
# Projecao1x1 ajusta canais da conexao residual quando necessario.
# Campo receptivo = 1 + 2*(kernel_size-1)*sum(dilatacoes):31 no padrao.
# A janela fornece24meses; padding interno nao representa valores imputados.
# Usar somente o ultimo passo temporal e uma saida densa linear, sem teto.
# =============================================================================
criar_modelo_tcn <- function(config) {
  p <- config$parametros
  entrada <- keras3::keras_input(shape = c(as.integer(config$lookback), 3L),
    name = "historico_mensal")
  x <- entrada
  canais <- 3L
  for (i in seq_along(p$dilatacoes)) {
    prefixo <- paste0("bloco", i)
    residual <- x
    z <- keras3::layer_conv_1d(x, filters = as.integer(p$filtros),
      kernel_size = as.integer(p$kernel_size), strides = 1L, padding = "causal",
      dilation_rate = as.integer(p$dilatacoes[i]), activation = "relu",
      name = paste0(prefixo, "_conv1"))
    z <- keras3::layer_spatial_dropout_1d(z, rate = p$dropout,
      name = paste0(prefixo, "_drop1"))
    z <- keras3::layer_conv_1d(z, filters = as.integer(p$filtros),
      kernel_size = as.integer(p$kernel_size), strides = 1L, padding = "causal",
      dilation_rate = as.integer(p$dilatacoes[i]), activation = "relu",
      name = paste0(prefixo, "_conv2"))
    z <- keras3::layer_spatial_dropout_1d(z, rate = p$dropout,
      name = paste0(prefixo, "_drop2"))
    if (canais != p$filtros) {
      residual <- keras3::layer_conv_1d(residual, filters = as.integer(p$filtros),
        kernel_size = 1L, padding = "same", name = paste0(prefixo, "_projecao"))
    }
    x <- keras3::layer_add(list(residual, z), name = paste0(prefixo, "_soma"))
    x <- keras3::layer_activation(x, activation = "relu", name = paste0(prefixo, "_relu"))
    canais <- p$filtros
  }
  x <- keras3::layer_cropping_1d(x,
    cropping = list(as.integer(config$lookback - 1L), 0L), name = "ultimo_passo")
  x <- keras3::layer_flatten(x, name = "vetor_final")
  saida <- keras3::layer_dense(x, units = 1L, activation = "linear", name = "quantidade_z")
  keras3::keras_model(inputs = entrada, outputs = saida, name = "tcn_mensal")
}

# =============================================================================
# ETAPA 07 — AJUSTE EFETIVO, UMA REDE INDEPENDENTE POR LOCALIDADE
# Epocas fixas, MSE e Adam; sem embaralhar, usar2025 ou early stopping.
# Validar com o trecho final reservado ANTES da escala; nao atualizar pesos
# nesse trecho. O mesmo ajuste produz o historico e a previsao de84meses.
# Limpar a sessao evita acumular grafos entre municipios. Semente fixa nao
# implica reproducibilidade bit a bit entre versoes/hardware diferentes.
# =============================================================================
ajustar_tcn <- function(treino, config) {
  keras3::clear_session()
  set.seed(config$seed)
  keras3::set_random_seed(as.integer(config$seed))
  modelo <- criar_modelo_tcn(config)
  keras3::compile(modelo, loss = "mse",
    optimizer = keras3::optimizer_adam(learning_rate = config$parametros$taxa_aprendizado))
  # Manter validation_data como LISTA R: keras3 normaliza recursivamente x/y.
  # Uma tuple Python pronta pula essa conversao; o vetor y pode virar lista
  # Python e o adaptador rejeitar os dados. Alvos Nx1 preservam a dimensao de
  # amostras inclusive quando existe apenas uma janela de validacao.
  history <- keras3::fit(modelo, x = treino$x, y = matrix(treino$y, ncol = 1L),
    epochs = as.integer(config$parametros$epocas),
    batch_size = as.integer(config$parametros$tamanho_lote),
    shuffle = FALSE,
    validation_data = list(treino$x_validacao, matrix(treino$y_validacao, ncol = 1L)),
    verbose = as.integer(config$parametros$verbose))
  list(modelo = modelo, metricas = history$metrics)
}

extrair_historico_tcn <- function(metricas, localidade) {
  loss <- as.numeric(metricas[["loss"]])
  if (!length(loss)) stop("Historico Keras sem loss.")
  val_loss <- metricas[["val_loss"]]
  if (is.null(val_loss) || !length(val_loss)) {
    message(localidade, ": val_loss ausente; historico registra NA, sem fabricar curva.")
    val_loss <- rep(NA_real_, length(loss))
  }
  val_loss <- as.numeric(val_loss)
  if (length(val_loss) != length(loss)) stop("loss e val_loss com numeros de epocas diferentes.")
  limpar <- function(x, nome) {
    invalidos <- !is.finite(x) | x < 0
    if (any(invalidos)) {
      message(localidade, ": ", sum(invalidos), " valor(es) invalido(s)/ausente(s) em ", nome, ".")
      x[invalidos] <- NA_real_
    }
    x
  }
  loss <- limpar(loss, "Loss"); val_loss <- limpar(val_loss, "Val_Loss")
  if (!any(is.finite(loss))) stop("Nenhuma perda de treino finita no historico.")
  data.frame(Modelo = "TCN", Localidade = localidade, Epoca = seq_along(loss),
    Loss = loss, Val_Loss = val_loss)
}

# =============================================================================
# ETAPA 08 — RECURSAO CONTINUA DO TESTE E FUTURO, SEM REAL/2025
# A funcao recebe somente ultima janela, escala do treino e datas solicitadas.
# Prever um mes, acrescentar z previsto + calendario e deslocar a janela.
# predict_on_batch e a API para inferencia de um lote pequeno, sem reajuste.
# Negativos sao limitados apenas na saida; nao corrigir o buffer recursivo.
# =============================================================================
prever_mensal_tcn <- function(modelo, treino, datas) {
  datas <- as.Date(datas)
  if (!length(datas) || anyNA(datas) || anyDuplicated(datas) ||
      !identical(datas, seq(datas[1L], by = "month", length.out = length(datas)))) {
    stop("Grade de previsao mensal invalida.")
  }
  sequencia <- treino$ultima_entrada
  l <- nrow(sequencia)
  calendario <- calendario_tcn(datas)
  previsao <- numeric(length(datas))
  for (i in seq_along(datas)) {
    entrada <- array(0, dim = c(1L, l, 3L))
    entrada[1L, , ] <- sequencia
    z <- as.numeric(keras3::predict_on_batch(modelo, x = entrada))
    if (length(z) != 1L || !is.finite(z)) stop("TCN retornou previsao nao finita ou forma inesperada.")
    previsao[i] <- treino$media + treino$desvio * z
    if (!is.finite(previsao[i])) stop("Previsao nao finita ao recuperar a escala original.")
    sequencia <- rbind(sequencia[-1L, , drop = FALSE], c(z, calendario[i, ]))
  }
  list(previsao = pmax(0, previsao), negativas_corrigidas = sum(previsao < 0))
}

# Escrever ambos os CSVs da mesma execucao. Preparar os dois antes de substituir
# os anteriores e tentar restaurar o par se houver erro de permissao/publicacao.
# Temporarios sao removidos ao terminar; nao constituem relatorios auxiliares.
gravar_resultados_tcn <- function(previsoes, historico, config) {
  destinos <- c(config$saida, config$arquivo_historico)
  for (pasta in unique(dirname(destinos))) dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  temporarios <- vapply(destinos, function(p) tempfile(".tcn_novo_", tmpdir = dirname(p)), character(1L))
  backups <- vapply(destinos, function(p) tempfile(".tcn_anterior_", tmpdir = dirname(p)), character(1L))
  movidos <- publicados <- rep(FALSE, 2L); concluido <- FALSE
  on.exit({
    if (!concluido) {
      for (i in seq_along(destinos)) {
        if (publicados[i]) unlink(destinos[i])
      }
      for (i in seq_along(destinos)) {
        if (movidos[i] && !file.rename(backups[i], destinos[i])) {
          warning("Nao foi possivel restaurar ", destinos[i],
            "; copia anterior preservada em ", backups[i], call. = FALSE)
        }
      }
    }
    unlink(temporarios)
    if (concluido) unlink(backups)
  }, add = TRUE)
  dados <- list(previsoes, historico)
  for (i in seq_along(destinos)) {
    utils::write.csv(dados[[i]], temporarios[i], row.names = FALSE, fileEncoding = "UTF-8", na = "NA")
  }
  for (i in seq_along(destinos)) {
    if (file.exists(destinos[i])) {
      if (!file.rename(destinos[i], backups[i])) stop("Nao foi possivel substituir ", destinos[i], ". Feche o arquivo e tente novamente.")
      movidos[i] <- TRUE
    }
  }
  for (i in seq_along(destinos)) {
    if (!file.rename(temporarios[i], destinos[i])) stop("Falha ao gravar ", destinos[i], ".")
    publicados[i] <- TRUE
  }
  concluido <- TRUE
  invisible(NULL)
}

executar_tcn_local <- function(config = config_tcn_local) {
  # global.R pode usar OutDec=",". Isolar execucao/escrita e restaurar ao sair.
  anteriores <- options(OutDec = ".")
  on.exit(options(anteriores), add = TRUE)
  validar_config_tcn(config)
  mensal <- ler_mensal_tcn(config)
  verificar_backend_tcn()
  on.exit(try(keras3::clear_session(), silent = TRUE), add = TRUE)
  corte <- as.Date(sprintf("%d-01-01", config$ano_teste))
  datas_teste <- seq(corte, by = "month", length.out = config$n_teste)
  datas_futuras <- seq(as.Date(sprintf("%d-01-01", config$ano_teste + 1L)),
    by = "month", length.out = config$passos_futuro)
  datas_prever <- c(datas_teste, datas_futuras)
  p <- config$parametros
  message("TCN: lookback=", config$lookback, "; filtros=", p$filtros,
    "; kernel=", p$kernel_size, "; dilatacoes=", paste(p$dilatacoes, collapse = "/"),
    "; dropout=", p$dropout, "; Adam=", p$taxa_aprendizado,
    "; epocas=", p$epocas, "; lote=", p$tamanho_lote,
    "; validacao_interna=", config$validacao_interna, "; seed=", config$seed, ".")
  message("Validacao temporal de um passo, anterior ao teste; sem segundo ajuste.")
  resultados <- historicos <- list(); ignoradas <- 0L
  for (nome in unique(mensal$Localidade)) {
    serie <- mensal[mensal$Localidade == nome, ]
    resultado <- tryCatch({
      # ======================================================================
      # ETAPA 09 — CORTE TEMPORAL E EXECUCAO POR LOCALIDADE
      # Separar antes de padronizar/criar tensores. O treino nao contem2025.
      # Separar tambem a validacao interna antes da escala. O mesmo modelo e a
      # mesma escala atendem os84meses, sem refit ou reinicio em2026.
      # ======================================================================
      periodo_treino <- serie$Data < corte
      historico <- serie[periodo_treino, ]
      treino <- preparar_treino_tcn(historico, config)
      message(nome, ": ", dim(treino$x)[1L], " janelas de ajuste [",
        min(treino$datas_alvos), " a ", max(treino$datas_alvos), "]; ",
        dim(treino$x_validacao)[1L], " de validacao [",
        min(treino$datas_validacao), " a ", max(treino$datas_validacao), "]; ",
        treino$janelas_descartadas, " descartadas por NA.")
      if (treino$escala_unitaria) message(nome, ": serie constante; denominador da escala =1.")
      if (identical(nome, "PARÁ (Geral)") &&
          any(historico$Municipios_com_total_conhecido < historico$Municipios_esperados)) {
        message(nome, ": soma estadual observada com cobertura municipal parcial.")
      }
      ajuste <- ajustar_tcn(treino, config)
      modelo <- ajuste$modelo
      historico_perdas <- extrair_historico_tcn(ajuste$metricas, nome)
      pred <- prever_mensal_tcn(modelo, treino, datas_prever)
      if (length(pred$previsao) != length(datas_prever) || any(!is.finite(pred$previsao))) {
        stop("Previsoes nao correspondem ao horizonte solicitado.")
      }
      if (pred$negativas_corrigidas > 0L) {
        message(nome, ": ", pred$negativas_corrigidas, " previsoes negativas limitadas a zero somente na saida.")
      }

      # ======================================================================
      # ETAPA 10 — CONSOLIDACAO E CSV NA ESCALA ORIGINAL
      # Mesmo contrato dos novos produtores: Real original, previsao NA no
      # treino e Real NA no futuro. AIC/BIC nao se aplicam e ficam NA.
      # Historico de perdas vai para data/, separado das previsoes mensais.
      # MAE/RMSE/MAPE/R2 continuam em runtime; nao salvar pesos/gradientes.
      # ======================================================================
      teste <- !periodo_treino
      stopifnot(sum(teste) == config$n_teste, identical(serie$Data[teste], datas_teste))
      previstos <- rep(NA_real_, nrow(serie))
      previstos[teste] <- head(pred$previsao, config$n_teste)
      previsoes <- data.frame(Localidade = nome, Data = c(serie$Data, datas_futuras),
        Real = c(serie$Real, rep(NA_real_, config$passos_futuro)),
        Previsao_TCN = c(previstos, tail(pred$previsao, config$passos_futuro)),
        Modelo = "TCN", AIC = NA_real_, BIC = NA_real_,
        Particao = c(ifelse(teste, "teste", "treino"), rep("futuro", config$passos_futuro)),
        Municipios_com_registro = c(serie$Municipios_com_registro, rep(NA, config$passos_futuro)),
        Municipios_com_total_conhecido = c(serie$Municipios_com_total_conhecido,
          rep(NA, config$passos_futuro)), Municipios_esperados = serie$Municipios_esperados[1L])
      list(previsoes = previsoes, historico = historico_perdas)
    }, error = function(e) e)
    # Liberar o modelo mesmo em caso de erro; nao persistir pesos no disco.
    if (exists("modelo", inherits = FALSE)) rm(modelo)
    if (exists("ajuste", inherits = FALSE)) rm(ajuste)
    invisible(try(keras3::clear_session(), silent = TRUE))
    if (inherits(resultado, "error")) {
      ignoradas <- ignoradas + 1L
      message(nome, ": ignorado; ", conditionMessage(resultado))
      next
    }
    resultados[[nome]] <- resultado$previsoes
    historicos[[nome]] <- resultado$historico
    message(nome, ": previsoes mensais concluidas.")
  }
  if (!length(resultados)) {
    stop("Nenhuma localidade processada; consulte o console. CSVs anteriores preservados.")
  }
  saida <- do.call(rbind, unname(resultados)); rownames(saida) <- NULL
  if (anyDuplicated(saida[c("Localidade", "Data")])) stop("Saida com datas duplicadas por localidade.")
  perdas <- do.call(rbind, unname(historicos)); rownames(perdas) <- NULL
  if (!nrow(perdas) || anyDuplicated(perdas[c("Localidade", "Epoca")])) {
    stop("Historico de perdas vazio ou com epocas duplicadas; arquivos anteriores preservados.")
  }
  gravar_resultados_tcn(saida, perdas, config)
  message("TCN: ", length(resultados), " localidades processadas; ", ignoradas,
    " ignoradas. Previsoes: ", config$saida, "; perdas: ", config$arquivo_historico, ".")
  invisible(saida)
}

# source()/Rscript executam o produtor local. A opcao permite carregar apenas
# funcoes nos testes locais; nao usar para executar modelos/testes na nuvem.
if (!isTRUE(getOption("frota.tcn.apenas_funcoes", FALSE))) {
  executar_tcn_local()
}
