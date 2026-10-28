# ==============================================================================
# pre_compilar_GRU_global_reorganizado.r
# GRU global com tensor 3D e previsao autoregressiva ate 2031
# Execute na raiz do projeto:
#   source("pre_compilar_GRU_global_reorganizado.r")
# ==============================================================================

Sys.setenv(
  TF_CPP_MIN_LOG_LEVEL = "2",
  TF_ENABLE_ONEDNN_OPTS = "0"
)

library(dplyr)
library(keras3)
library(tensorflow)

# ==============================================================================
# CONFIGURACAO CENTRAL
# ==============================================================================

config <- list(
  # Dados e horizonte
  arquivo_entrada = "data/previsoes_sarimax_pre_comp.parquet",
  ano_inicio = 2016L,
  ano_fim_observado = 2025L,
  n_meses = 12L,
  lookback = 24L,
  n_teste = 12L,
  passos_futuro = 72L,

  # Arquitetura e treinamento
  unidades_gru = 32L,
  unidades_dense = 1L,
  funcao_perda = "mse",
  taxa_aprendizado = 0.01,
  epocas = 100L,
  tamanho_lote = 32L,
  validacao_interna = 0.10,
  semente = 42L,

  # Compatibilidade com o criterio da versao anterior
  minimo_extra_observacoes = 5L,

  # Arquivos produzidos pelo script
  arquivo_historico = "data/historico_perdas_gru.csv",
  arquivo_previsoes = file.path(
  "Previsoes_output", "previsoes_gru_global_pre_comp.csv"
  )
)

# O modelo recebe: 1 residuo + 12 indicadores mensais + 1 indicador por localidade.
config$n_features_base <- 1L + config$n_meses

set.seed(config$semente)
keras3::set_random_seed(config$semente)

cat("Iniciando processo global com rede GRU...\n")
cat(sprintf(
  paste0(
    "Configuracao: lookback=%d, teste=%d, futuro=%d, GRU=%d, Dense=%d, ",
    "loss=%s, Adam=%.4f, epocas=%d, lote=%d, validacao=%.2f, semente=%d\n"
  ),
  config$lookback, config$n_teste, config$passos_futuro,
  config$unidades_gru, config$unidades_dense, config$funcao_perda,
  config$taxa_aprendizado, config$epocas, config$tamanho_lote,
  config$validacao_interna, config$semente
))

if (!file.exists(config$arquivo_entrada)) {
  stop("Arquivo de entrada nao encontrado: ", config$arquivo_entrada)
}

# ==============================================================================
# FUNCOES AUXILIARES
# ==============================================================================

one_hot_meses <- function(meses, n_meses) {
  saida <- matrix(0, nrow = length(meses), ncol = n_meses)
  saida[cbind(seq_along(meses), as.integer(meses))] <- 1
  saida
}

montar_tensor_amostras <- function(blocos, lookback, n_features) {
  if (length(blocos) == 0L) {
    stop("Nenhum bloco de amostras foi fornecido.")
  }

  tamanhos <- vapply(blocos, function(bloco) dim(bloco)[1L], integer(1L))
  total_amostras <- sum(tamanhos)
  tensor <- array(
    NA_real_,
    dim = c(total_amostras, lookback, n_features)
  )

  inicio <- 1L
  for (bloco in blocos) {
    n <- dim(bloco)[1L]
    indices <- seq.int(inicio, length.out = n)
    tensor[indices, , ] <- bloco
    inicio <- inicio + n
  }

  stopifnot(
    dim(tensor)[1L] == total_amostras,
    dim(tensor)[2L] == lookback,
    dim(tensor)[3L] == n_features
  )
  tensor
}

agregar_localidade <- function(dados, local) {
  base <- if (identical(local, "PARÁ (Geral)")) {
    dados
  } else {
    dplyr::filter(dados, LOCAL == local)
  }

  base %>%
    group_by(ANO, MES) %>%
    summarise(
      valor_mes = sum(QTD_VEICULO, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
    ) %>%
    arrange(Data)
}

preparar_localidade <- function(
    dados,
    local,
    local_index,
    n_localidades,
    config
) {
  df_local <- agregar_localidade(dados, local)

  if (nrow(df_local) <= (
    config$lookback + config$n_teste + config$minimo_extra_observacoes
  )) {
    message("Ignorando ", local, ": serie temporal insuficiente.")
    return(NULL)
  }

  # Identidade da localidade: uma codificacao one-hot por serie.
  local_onehot <- matrix(
    0,
    nrow = nrow(df_local),
    ncol = n_localidades
  )
  local_onehot[, local_index] <- 1

  # Tendencia, perfil sazonal e residuo normalizado.
  t_index <- seq_along(df_local$valor_mes)
  modelo_tendencia <- lm(
    df_local$valor_mes ~ t_index + I(t_index^2)
  )
  tendencia_historica <- as.numeric(predict(modelo_tendencia))

  if (any(!is.finite(tendencia_historica)) ||
      any(abs(tendencia_historica) < 1e-10)) {
    stop("Tendencia invalida para a localidade: ", local)
  }

  razao_tendencia <- df_local$valor_mes / tendencia_historica
  perfil_sazonal <- tapply(
    razao_tendencia,
    df_local$MES,
    mean,
    na.rm = TRUE
  )

  if (length(perfil_sazonal) != config$n_meses ||
      any(!is.finite(perfil_sazonal))) {
    stop("Perfil sazonal incompleto para a localidade: ", local)
  }

  sazonalidade <- as.numeric(
    perfil_sazonal[as.character(df_local$MES)]
  )
  ruido_puro <- razao_tendencia / sazonalidade
  media_res <- mean(ruido_puro)
  desvio_res <- sd(ruido_puro)

  if (!is.finite(media_res) || !is.finite(desvio_res) ||
      desvio_res < 1e-10) {
    stop("Residuo sem variacao valida para a localidade: ", local)
  }

  residual_norm <- (ruido_puro - media_res) / desvio_res
  meses_onehot <- one_hot_meses(df_local$MES, config$n_meses)

  n_amostras <- length(residual_norm) - config$lookback
  X <- lapply(seq_len(n_amostras), function(i) {
    indices <- seq.int(i, length.out = config$lookback)
    cbind(
      residual_norm[indices],
      meses_onehot[indices, , drop = FALSE],
      local_onehot[indices, , drop = FALSE]
    )
  })
  Y <- as.numeric(residual_norm[(config$lookback + 1L):nrow(df_local)])

  # Cada X[i] e uma matriz [tempo x variaveis]. A transformacao abaixo
  # preserva os eixos ao formar [amostras x tempo x variaveis].
  X_array <- aperm(
    array(
      unlist(X, use.names = FALSE),
      dim = c(
        config$lookback,
        config$n_features_base + n_localidades,
        length(X)
      )
    ),
    c(3L, 1L, 2L)
  )

  stopifnot(
    all(dim(X_array) == c(
      length(X),
      config$lookback,
      config$n_features_base + n_localidades
    )),
    isTRUE(all.equal(
      unname(X_array[1L, , ]),
      unname(X[[1L]]),
      check.attributes = TRUE
    ))
  )

  n_treino <- length(Y) - config$n_teste
  if (n_treino < 1L) {
    stop("Nao ha janelas suficientes para treino em: ", local)
  }

  list(
    local = local,
    df_local = df_local,
    X_train = X_array[seq_len(n_treino), , , drop = FALSE],
    Y_train = Y[seq_len(n_treino)],
    X_test = X_array[(n_treino + 1L):length(Y), , , drop = FALSE],
    modelo_tendencia = modelo_tendencia,
    tendencia_historica = tendencia_historica,
    perfil_sazonal = perfil_sazonal,
    media_res = media_res,
    desvio_res = desvio_res,
    residual_norm = residual_norm,
    meses_onehot = meses_onehot,
    local_onehot = local_onehot,
    n_treino = n_treino
  )
}

criar_modelo_gru <- function(n_features, config) {
  keras_model_sequential(
    input_shape = c(config$lookback, n_features)
  ) %>%
    layer_gru(units = config$unidades_gru) %>%
    layer_dense(units = config$unidades_dense)
}

reconstruir_previsao <- function(
    residuos_norm,
    meta,
    datas,
    indices_tendencia
) {
  if (length(residuos_norm) != length(datas) ||
      length(datas) != length(indices_tendencia) ||
      any(indices_tendencia < 1L)) {
    stop("Dimensoes invalidas ao reconstruir a previsao de: ", meta$local)
  }

  meses <- as.integer(format(datas, "%m"))
  tendencia <- if (all(indices_tendencia <= nrow(meta$df_local))) {
    meta$tendencia_historica[indices_tendencia]
  } else {
    as.numeric(predict(
      meta$modelo_tendencia,
      newdata = data.frame(t_index = indices_tendencia)
    ))
  }
  sazonalidade <- as.numeric(
    meta$perfil_sazonal[as.character(meses)]
  )
  ruido <- residuos_norm * meta$desvio_res + meta$media_res
  valores <- tendencia * sazonalidade * ruido

  if (any(!is.finite(valores))) {
    stop("Previsao nao finita para: ", meta$local)
  }
  valores
}

prever_futuro <- function(modelo, meta, datas, n_features, config) {
  sequencia <- cbind(
    tail(meta$residual_norm, config$lookback),
    tail(meta$meses_onehot, config$lookback),
    tail(meta$local_onehot, config$lookback)
  )
  previsoes <- numeric(length(datas))
  vetor_local <- meta$local_onehot[1L, ]

  for (i in seq_along(datas)) {
    entrada <- array(
      sequencia,
      dim = c(1L, config$lookback, n_features)
    )
    pred <- as.numeric(predict(modelo, entrada, verbose = 0L))
    previsoes[i] <- pred

    mes <- as.integer(format(datas[i], "%m"))
    nova_linha <- c(
      pred,
      as.numeric(seq_len(config$n_meses) == mes),
      vetor_local
    )
    sequencia <- rbind(
      sequencia[-1L, , drop = FALSE],
      nova_linha
    )
  }
  previsoes
}

# ==============================================================================
# PREPARACAO DO DADO E CONSTRUCAO DO TENSOR GLOBAL
# ==============================================================================

dados_basicos <- arrow::open_dataset(config$arquivo_entrada) %>%
  select(Localidade_Nome, Data, Real) %>%
  filter(!is.na(Localidade_Nome), !is.na(Real)) %>%
  collect() %>%
  mutate(
    Data = as.Date(Data),
    LOCAL = trimws(as.character(Localidade_Nome)),
    ANO = as.integer(format(Data, "%Y")),
    MES = as.integer(format(Data, "%m")),
    QTD_VEICULO = as.numeric(Real)
  ) %>%
  filter(
    ANO >= config$ano_inicio,
    ANO <= config$ano_fim_observado,
    nzchar(LOCAL),
    !toupper(LOCAL) %in% c(
      "PARÁ",
      "PARÁ (GERAL)",
      "PARA",
      "PARA (GERAL)"
    )
  ) %>%
  select(LOCAL, ANO, MES, QTD_VEICULO)

municipios <- unique(dados_basicos$LOCAL)
localidades <- c("PARÁ (Geral)", municipios)
n_localidades <- length(localidades)
n_features <- config$n_features_base + n_localidades
mapa_localidades <- setNames(seq_along(localidades), localidades)

cat(
  "\nEtapa 1: preparando", n_localidades,
  "localidades e construindo o tensor global...\n"
)

metadados <- list()
for (local in localidades) {
  cat("Lendo dados de: ", local, "...\n", sep = "")
  resultado <- tryCatch(
    preparar_localidade(
      dados_basicos,
      local,
      mapa_localidades[[local]],
      n_localidades,
      config
    ),
    error = function(e) {
      if (identical(local, "PARÁ (Geral)")) stop(e)
      message("Ignorando ", local, ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(resultado)) metadados[[local]] <- resultado
}

if (!"PARÁ (Geral)" %in% names(metadados)) {
  stop("A serie do Pará (Geral) nao foi preparada.")
}

X_train_global <- montar_tensor_amostras(
  lapply(metadados, `[[`, "X_train"),
  config$lookback,
  n_features
)
Y_train_global <- unlist(lapply(metadados, `[[`, "Y_train"), use.names = FALSE)

stopifnot(
  dim(X_train_global)[1L] == length(Y_train_global),
  dim(X_train_global)[2L] == config$lookback,
  dim(X_train_global)[3L] == n_features
)

# ==============================================================================
# TREINAMENTO E PERSISTENCIA DAS PERDAS
# ==============================================================================

cat(
  "\nEtapa 2: treinando uma unica GRU global com",
  length(Y_train_global), "amostras...\n"
)

keras3::clear_session()
keras3::set_random_seed(config$semente)
modelo_gru <- criar_modelo_gru(n_features, config)
modelo_gru %>% compile(
  loss = config$funcao_perda,
  optimizer = optimizer_adam(
    learning_rate = config$taxa_aprendizado
  )
)

history_gru <- modelo_gru %>% fit(
  x = X_train_global,
  y = Y_train_global,
  epochs = config$epocas,
  batch_size = config$tamanho_lote,
  validation_split = config$validacao_interna,
  verbose = 0
)

metricas <- history_gru$metrics
if (!"loss" %in% names(metricas)) {
  stop("O historico da GRU nao contem a metrica 'loss'.")
}

loss <- as.numeric(metricas[["loss"]])
val_loss <- if ("val_loss" %in% names(metricas)) {
  as.numeric(metricas[["val_loss"]])
} else {
  rep(NA_real_, length(loss))
}

df_historico <- data.frame(
  Modelo = "GRU",
  Localidade = "Global",
  Epoca = seq_along(loss),
  Loss = loss,
  Val_Loss = val_loss
)

dir.create(dirname(config$arquivo_historico), recursive = TRUE,
           showWarnings = FALSE)
write.csv(df_historico, config$arquivo_historico, row.names = FALSE)
cat("Historico salvo em: ", config$arquivo_historico, "\n", sep = "")

# ==============================================================================
# INFERENCIA: TESTE TEMPORAL E PREVISAO FUTURA CONTINUA
# ==============================================================================

cat("\nEtapa 3: gerando previsoes por localidade...\n")
lista_resultados <- list()

for (local in names(metadados)) {
  cat("Prevendo e reconstruindo para: ", local, "...\n", sep = "")
  meta <- metadados[[local]]
  n_observacoes <- nrow(meta$df_local)

  # Os ultimos n_teste alvos formam o teste temporal.
  previsoes_norm_teste <- as.numeric(predict(
    modelo_gru,
    meta$X_test,
    verbose = 0L
  ))
  indice_teste_inicio <- config$lookback + meta$n_treino + 1L
  indice_teste_fim <- indice_teste_inicio + config$n_teste - 1L
  datas_teste <- meta$df_local$Data[
    indice_teste_inicio:indice_teste_fim
  ]
  valores_teste <- reconstruir_previsao(
    previsoes_norm_teste,
    meta,
    datas_teste,
    indices_tendencia = indice_teste_inicio:indice_teste_fim
  )

  # A mesma rede continua recursivamente de janeiro de 2026 a dezembro de 2031.
  datas_futuras <- seq.Date(
    max(meta$df_local$Data),
    by = "month",
    length.out = config$passos_futuro + 1L
  )[-1L]
  previsoes_norm_futuras <- prever_futuro(
    modelo_gru,
    meta,
    datas_futuras,
    n_features,
    config
  )
  valores_futuros <- reconstruir_previsao(
    previsoes_norm_futuras,
    meta,
    datas_futuras,
    indices_tendencia = seq.int(
      n_observacoes + 1L,
      length.out = length(datas_futuras)
    )
  )

  datas_completas <- c(meta$df_local$Data, datas_futuras)
  resultado <- data.frame(
    Localidade = local,
    Data = datas_completas,
    Real = c(
      as.numeric(meta$df_local$valor_mes),
      rep(NA_real_, length(datas_futuras))
    ),
    Previsao_GRU = rep(NA_real_, length(datas_completas)),
    AIC = NA_real_,
    BIC = NA_real_,
    Modelo = "GRU_Global"
  )

  resultado$Previsao_GRU[
    indice_teste_inicio:indice_teste_fim
  ] <- valores_teste
  resultado$Previsao_GRU[
    (n_observacoes + 1L):nrow(resultado)
  ] <- valores_futuros
  lista_resultados[[local]] <- resultado
}

# ==============================================================================
# CONSOLIDACAO FINAL
# ==============================================================================

df_previsoes <- bind_rows(lista_resultados)
if (anyDuplicated(df_previsoes[c("Localidade", "Data")]) > 0L) {
  stop("Foram encontradas chaves Localidade/Data duplicadas.")
}

dir.create(dirname(config$arquivo_previsoes), recursive = TRUE,
           showWarnings = FALSE)
write.csv(df_previsoes, config$arquivo_previsoes, row.names = FALSE)

cat("Previsoes salvas em: ", config$arquivo_previsoes, "\n", sep = "")
cat("Processo global concluido.\n")
