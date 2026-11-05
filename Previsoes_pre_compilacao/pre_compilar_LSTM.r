# ==============================================================================
# pre_compilar_LSTM_monitorado.r
# Pré-cálculo das previsões LSTM com histórico de perdas e gradientes
# ==============================================================================

library(dplyr)
library(keras3)
library(tensorflow)

cat("Iniciando a pré-compilação das previsões do LSTM...\n")

if (!reticulate::py_available()) {
  cat("Iniciando o ambiente Python do TensorFlow...\n")
}

# ------------------------------------------------------------------------------
# Diagnóstico dos gradientes
# ------------------------------------------------------------------------------
# Esta função mede os gradientes no estado atual do modelo. Ela não chama
# optimizer$apply_gradients(), portanto não altera os pesos nem o treinamento.
medir_gradientes_lstm <- function(modelo, x_treino, y_treino) {

  x_tf <- tf$convert_to_tensor(
    x_treino,
    dtype = tf$float32
  )

  y_tf <- tf$convert_to_tensor(
    y_treino,
    dtype = tf$float32
  )

  y_tf <- tf$reshape(
    y_tf,
    shape = c(-1L, 1L)
  )

  with(tf$GradientTape() %as% fita, {
    previsoes <- modelo(
      x_tf,
      training = TRUE
    )

    perda <- tf$reduce_mean(
      tf$square(y_tf - previsoes)
    )
  })

  gradientes <- fita$gradient(
    perda,
    modelo$trainable_weights
  )

  gradientes <- gradientes[
    !vapply(gradientes, is.null, logical(1))
  ]

  if (length(gradientes) == 0L) {
    return(data.frame(
      Loss_Gradiente = NA_real_,
      Norma_Gradiente = NA_real_,
      Max_Gradiente = NA_real_,
      Gradiente_NaN = NA,
      Gradiente_Inf = NA
    ))
  }

  soma_quadrados <- vapply(
    gradientes,
    function(gradiente) {
      as.numeric(
        tf$reduce_sum(
          tf$square(gradiente)
        )$numpy()
      )
    },
    numeric(1)
  )

  maiores_valores <- vapply(
    gradientes,
    function(gradiente) {
      as.numeric(
        tf$reduce_max(
          tf$abs(gradiente)
        )$numpy()
      )
    },
    numeric(1)
  )

  possui_nan <- any(
    vapply(
      gradientes,
      function(gradiente) {
        isTRUE(as.logical(
          tf$reduce_any(
            tf$math$is_nan(gradiente)
          )$numpy()
        ))
      },
      logical(1)
    )
  )

  possui_inf <- any(
    vapply(
      gradientes,
      function(gradiente) {
        isTRUE(as.logical(
          tf$reduce_any(
            tf$math$is_inf(gradiente)
          )$numpy()
        ))
      },
      logical(1)
    )
  )

  data.frame(
    Loss_Gradiente = as.numeric(perda$numpy()),
    Norma_Gradiente = sqrt(sum(soma_quadrados)),
    Max_Gradiente = max(maiores_valores),
    Gradiente_NaN = possui_nan,
    Gradiente_Inf = possui_inf
  )
}

criar_callback_gradientes_lstm <- function(
    modelo,
    x_treino,
    y_treino
) {

  estado <- new.env(parent = emptyenv())

  estado$dados <- data.frame(
    Epoca = integer(0),
    Loss_Gradiente = numeric(0),
    Norma_Gradiente = numeric(0),
    Max_Gradiente = numeric(0),
    Gradiente_NaN = logical(0),
    Gradiente_Inf = logical(0)
  )

  callback <- keras3::callback_lambda(
    on_epoch_end = function(epoch, logs = NULL) {

      diagnostico <- tryCatch(
        medir_gradientes_lstm(
          modelo = modelo,
          x_treino = x_treino,
          y_treino = y_treino
        ),
        error = function(e) {
          data.frame(
            Loss_Gradiente = NA_real_,
            Norma_Gradiente = NA_real_,
            Max_Gradiente = NA_real_,
            Gradiente_NaN = NA,
            Gradiente_Inf = NA
          )
        }
      )

      diagnostico$Epoca <- as.integer(epoch) + 1L

      diagnostico <- diagnostico[
        ,
        c(
          "Epoca",
          "Loss_Gradiente",
          "Norma_Gradiente",
          "Max_Gradiente",
          "Gradiente_NaN",
          "Gradiente_Inf"
        )
      ]

      estado$dados <- rbind(
        estado$dados,
        diagnostico
      )
    }
  )

  list(
    callback = callback,
    estado = estado
  )
}

# ------------------------------------------------------------------------------
# Leitura da base CSV original utilizada na geração da tabela do artigo
# ------------------------------------------------------------------------------
dados_basicos <- read.csv(
  "data/veiculos_por_mes_municipio.csv"
)

municipios <- unique(dados_basicos$LOCAL)
localidades <- c("PARÁ (Geral)", municipios)

lista_resultados_lstm <- list()
lista_historicos_lstm <- list()
lista_gradientes_lstm <- list()

lookback <- 24
n_teste <- 12
passos_futuro <- 72

set.seed(42)
tensorflow::tf$random$set_seed(42L)

# ------------------------------------------------------------------------------
# Treinamento por localidade
# ------------------------------------------------------------------------------
for (local in localidades) {

  cat("\nProcessando Rede Neural LSTM para:", local, "...")

  if (local == "PARÁ (Geral)") {

    df_local <- dados_basicos %>%
      filter(ANO >= 2016, ANO <= 2025) %>%
      group_by(ANO, MES) %>%
      summarise(
        valor_mes = sum(QTD_VEICULO, na.rm = TRUE),
        .groups = "drop"
      )

  } else {

    df_local <- dados_basicos %>%
      filter(
        LOCAL == local,
        ANO >= 2016,
        ANO <= 2025
      ) %>%
      group_by(ANO, MES) %>%
      summarise(
        valor_mes = sum(QTD_VEICULO, na.rm = TRUE),
        .groups = "drop"
      )
  }

  if (nrow(df_local) <= (lookback + n_teste + 5)) {
    cat(" Ignorado (série temporal insuficiente para o lookback).")
    next
  }

  df_local <- df_local %>%
    mutate(
      Data = as.Date(
        paste(ANO, MES, "01", sep = "-")
      )
    ) %>%
    arrange(Data)

  tryCatch({

    keras3::clear_session()

    # Tendência e normalização
    t_index <- seq_along(df_local$valor_mes)
    valor <- df_local$valor_mes

    modelo_tendencia <- lm(
      valor ~ t_index + I(t_index^2)
    )

    tendencia_historica <- predict(modelo_tendencia)

    sazonalidade_residual <-
      df_local$valor_mes - tendencia_historica

    media_res <- mean(sazonalidade_residual)
    desvio_res <- sd(sazonalidade_residual)

    residual_norm <- (
      sazonalidade_residual - media_res
    ) / desvio_res

    # Variáveis mensais one-hot
    meses_onehot <- matrix(
      0,
      nrow = nrow(df_local),
      ncol = 12
    )

    for (j in seq_len(nrow(df_local))) {
      meses_onehot[j, df_local$MES[j]] <- 1
    }

    # Tensor de entrada
    X <- list()
    Y <- list()

    for (i in 1:(length(residual_norm) - lookback)) {

      matriz_temporaria <- cbind(
        residual_norm[i:(i + lookback - 1)],
        meses_onehot[i:(i + lookback - 1), ]
      )

      X[[i]] <- matriz_temporaria
      Y[[i]] <- residual_norm[i + lookback]
    }

    X <- array(
      unlist(X),
      dim = c(length(X), lookback, 13)
    )

    Y <- array(
      unlist(Y),
      dim = c(length(Y))
    )

    # Separação treino/teste
    n_treino <- length(Y) - n_teste

    X_train <- X[
      1:n_treino,
      ,
      ,
      drop = FALSE
    ]

    Y_train <- Y[1:n_treino]

    X_test <- X[
      (n_treino + 1):length(Y),
      ,
      ,
      drop = FALSE
    ]

    Y_test <- Y[
      (n_treino + 1):length(Y)
    ]

    # Modelo LSTM
    modelo_lstm <- keras_model_sequential(
      input_shape = c(lookback, 13)
    ) %>%
      layer_lstm(units = 32) %>%
      layer_dense(units = 1)

    modelo_lstm %>% compile(
      loss = "mse",
      optimizer = optimizer_adam(
        learning_rate = 0.01
      )
    )

    # validation_split = 0.1 utiliza os últimos 10% dos dados
    # como validação interna. O diagnóstico usa somente os demais 90%.
    fracao_validacao <- 0.1
    n_amostras_treino <- dim(X_train)[1]

    n_validacao_interna <- floor(
      n_amostras_treino * fracao_validacao
    )

    n_treino_gradiente <-
      n_amostras_treino - n_validacao_interna

    if (n_treino_gradiente < 1L) {
      stop("Não há dados suficientes para monitorar o gradiente.")
    }

    X_treino_gradiente <- X_train[
      seq_len(n_treino_gradiente),
      ,
      ,
      drop = FALSE
    ]

    Y_treino_gradiente <- Y_train[
      seq_len(n_treino_gradiente)
    ]

    monitor_gradientes <- criar_callback_gradientes_lstm(
      modelo = modelo_lstm,
      x_treino = X_treino_gradiente,
      y_treino = Y_treino_gradiente
    )

    # Treinamento: parâmetros originais preservados
    invisible(capture.output(
      history <- modelo_lstm %>% fit(
        x = X_train,
        y = Y_train,
        epochs = 100,
        batch_size = 8,
        validation_split = 0.1,
        callbacks = list(
          monitor_gradientes$callback
        ),
        verbose = 0
      )
    ))

    # Histórico original de perdas
    metricas_history <- history$metrics

    loss_history <- as.numeric(
      metricas_history[["loss"]]
    )

    val_loss_history <- if ("val_loss" %in% names(metricas_history)) {
      as.numeric(metricas_history[["val_loss"]])
    } else {
      rep(NA_real_, length(loss_history))
    }

    lista_historicos_lstm[[local]] <- data.frame(
      Modelo = "LSTM",
      Localidade = local,
      Epoca = seq_along(loss_history),
      Loss = loss_history,
      Val_Loss = val_loss_history
    )

    # Histórico de gradientes da localidade
    gradientes_localidade <- monitor_gradientes$estado$dados
    gradientes_localidade$Localidade <- local

    gradientes_localidade <- gradientes_localidade[
      ,
      c(
        "Localidade",
        "Epoca",
        "Loss_Gradiente",
        "Norma_Gradiente",
        "Max_Gradiente",
        "Gradiente_NaN",
        "Gradiente_Inf"
      )
    ]

    lista_gradientes_lstm[[local]] <- gradientes_localidade

    # Previsões do conjunto de teste
    previsoes_norm <- modelo_lstm %>% predict(
      x = X_test,
      verbose = 0
    )

    residuos_teste_previstos <- as.numeric(
      (previsoes_norm * desvio_res) + media_res
    )

    idx_teste_inicio <- lookback + n_treino + 1
    idx_teste_fim <- lookback + n_treino + n_teste

    tendencia_teste <- tendencia_historica[
      idx_teste_inicio:idx_teste_fim
    ]

    valores_teste_previstos <-
      tendencia_teste + residuos_teste_previstos

    # Previsões futuras
    datas_futuras <- seq(
      max(df_local$Data),
      by = "month",
      length.out = passos_futuro + 1
    )[-1]

    meses_futuros <- as.numeric(
      format(datas_futuras, "%m")
    )

    meses_futuros_onehot <- matrix(
      0,
      nrow = passos_futuro,
      ncol = 12
    )

    for (m in seq_len(passos_futuro)) {
      meses_futuros_onehot[
        m,
        meses_futuros[m]
      ] <- 1
    }

    ultima_seq_matriz <- cbind(
      tail(residual_norm, lookback),
      tail(meses_onehot, lookback)
    )

    previsoes_futuras_res_norm <- numeric(
      passos_futuro
    )

    sequencia_atual <- ultima_seq_matriz

    for (i in seq_len(passos_futuro)) {

      X_fut_tensor <- array(
        sequencia_atual,
        dim = c(1, lookback, 13)
      )

      pred <- modelo_lstm %>% predict(
        x = X_fut_tensor,
        verbose = 0
      )

      previsoes_futuras_res_norm[i] <- as.numeric(pred)

      nova_linha <- c(
        as.numeric(pred),
        meses_futuros_onehot[i, ]
      )

      sequencia_atual <- rbind(
        sequencia_atual[-1, ],
        nova_linha
      )
    }

    residuos_futuros <- as.numeric(
      (previsoes_futuras_res_norm * desvio_res) + media_res
    )

    t_futuro <- seq(
      length(df_local$valor_mes) + 1,
      length(df_local$valor_mes) + passos_futuro
    )

    tendencia_futura <- as.numeric(
      predict(
        modelo_tendencia,
        newdata = data.frame(t_index = t_futuro)
      )
    )

    previsoes_futuras_reais <-
      tendencia_futura + residuos_futuros

    # Data frame final
    Datas_Completas <- c(
      df_local$Data,
      datas_futuras
    )

    df_combinado <- data.frame(
      Localidade = local,
      Data = Datas_Completas,
      Real = rep(
        NA_real_,
        length(Datas_Completas)
      ),
      Previsao_LSTM = rep(
        NA_real_,
        length(Datas_Completas)
      ),
      AIC = NA_real_,
      BIC = NA_real_,
      Modelo = "LSTM"
    )

    df_combinado$Real[
      1:length(df_local$valor_mes)
    ] <- as.numeric(df_local$valor_mes)

    df_combinado$Previsao_LSTM[
      idx_teste_inicio:idx_teste_fim
    ] <- valores_teste_previstos

    idx_futuro_inicio <- length(df_local$valor_mes) + 1
    idx_futuro_fim <- length(df_local$valor_mes) + passos_futuro

    df_combinado$Previsao_LSTM[
      idx_futuro_inicio:idx_futuro_fim
    ] <- previsoes_futuras_reais

    lista_resultados_lstm[[local]] <- df_combinado

    cat(" Concluído com Sucesso!")

  }, error = function(e) {

    cat(
      " Erro de Tensor ou Convergência Keras. Omitido.",
      e$message
    )
  })
}

# ------------------------------------------------------------------------------
# Consolidação e gravação
# ------------------------------------------------------------------------------
cat(
  "\n\nEmpilhando resultados dos municípios processados...\n"
)

df_previsoes_lstm_pronto <- bind_rows(
  lista_resultados_lstm
)

caminho_saida <- file.path(
  "Previsoes_output", "previsoes_lstm_pre_comp.csv"
)

dir.create(
  dirname(caminho_saida),
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  df_previsoes_lstm_pronto,
  caminho_saida,
  row.names = FALSE
)

if (length(lista_historicos_lstm) > 0L) {

  df_historico_lstm <- dplyr::bind_rows(
    lista_historicos_lstm
  )

} else {

  df_historico_lstm <- data.frame(
    Modelo = character(0),
    Localidade = character(0),
    Epoca = integer(0),
    Loss = numeric(0),
    Val_Loss = numeric(0)
  )
}

caminho_historico_lstm <- "data/historico_perdas_lstm.csv"

write.csv(
  df_historico_lstm,
  caminho_historico_lstm,
  row.names = FALSE
)

if (length(lista_gradientes_lstm) > 0L) {

  df_historico_gradientes_lstm <- dplyr::bind_rows(
    lista_gradientes_lstm
  )

} else {

  df_historico_gradientes_lstm <- data.frame(
    Localidade = character(0),
    Epoca = integer(0),
    Loss_Gradiente = numeric(0),
    Norma_Gradiente = numeric(0),
    Max_Gradiente = numeric(0),
    Gradiente_NaN = logical(0),
    Gradiente_Inf = logical(0)
  )
}

caminho_gradientes_lstm <-
  "data/historico_gradientes_lstm.csv"

write.csv(
  df_historico_gradientes_lstm,
  caminho_gradientes_lstm,
  row.names = FALSE
)

cat(
  "Histórico de perdas salvo em:",
  caminho_historico_lstm,
  "\n"
)

cat(
  "Histórico de gradientes salvo em:",
  caminho_gradientes_lstm,
  "\n"
)

cat("=====================================================\n")
cat(
  "Processo offline concluído. Previsões salvas em:\n",
  caminho_saida,
  "\n"
)
cat("=====================================================\n")


