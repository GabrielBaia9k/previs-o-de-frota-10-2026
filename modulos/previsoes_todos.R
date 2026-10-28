# ==============================================================================
# previsoes_todos.R | CSV-PREVISOES-OUTPUT-1
# Substitua INTEGRALMENTE modulos/previsoes_todos.R por este arquivo.
#
# Mantem as chamadas existentes:
#   previsoes_todos_ui("previsoes_todos")
#   previsoes_todos_server("previsoes_todos")
#
# Previsoes: seis arquivos CSV em Previsoes_output/ (incluindo SARIMAX).
# Historicos e gradientes: permanecem em data/.
# Os diretorios podem ser configurados nos argumentos do servidor.
#
# Fontes dos graficos de perdas:
#   data/historico_perdas_lstm.csv
#   data/historico_perdas_gru.csv
# Fonte do diagnóstico dos gradientes:
#   data/historico_gradientes_lstm.csv
# Colunas: Modelo, Localidade, Epoca, Loss, Val_Loss
# Loss e Val_Loss sao as metricas retornadas por history$metrics no fit().
# A rede minimiza MSE dos residuos normalizados; nao e erro em veiculos.
# O LSTM e exibido para a localidade selecionada; a GRU e um modelo global.
# Este modulo NAO executa fit(), NAO treina modelos e NAO altera arquivos de dados.
#
# O grafico de perdas usa renderPlot()/plotOutput() e graphics (R nativo).
# Nao existe grafico de erro mensal neste modulo, nem dependencia de echarts4r.
# Previsoes mensais aparecem em tabela e no grafico de 2016 a 2031.
# ==============================================================================

previsoes_todos_versao <- "LSTM-GRU-PERDAS-5-CSV-PREVISOES-OUTPUT"

# --- Leitura e validacao --------------------------------------------------------

pt_normalizar_localidade <- function(x) {
  x <- trimws(as.character(x))
  estado <- toupper(x) %in% c("PARÁ", "PARA", "PARÁ (GERAL)", "PARA (GERAL)")
  x[estado] <- "Pará"
  x
}

pt_chave_localidade <- function(x) {
  # A GRU preserva nomes do CSV original (BELEM); os demais modelos podem
  # escrever Belém. A chave serve apenas para exibir a mesma localidade.
  x <- toupper(trimws(as.character(x)))
  x <- chartr(
    "ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ",
    "AAAAAEEEEIIIIOOOOOUUUUC", x
  )
  trimws(gsub("[^A-Z0-9]+", " ", x))
}

pt_numero <- function(x) {
  # Os CSVs originais usam ponto; tambem aceita decimal com virgula.
  # Nao arredonda, nao normaliza novamente e nao substitui perdas ausentes por zero.
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(sub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

pt_ler_csv <- function(caminho) {
  if (!file.exists(caminho) || dir.exists(caminho)) {
    stop("Arquivo não encontrado: ", caminho, call. = FALSE)
  }
  primeira <- readLines(caminho, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (length(primeira) == 0L || !nzchar(primeira)) {
    stop("O arquivo está vazio: ", caminho, call. = FALSE)
  }
  separador <- if (grepl(";", primeira, fixed = TRUE)) ";" else ","
  dados <- utils::read.table(
    caminho, header = TRUE, sep = separador, quote = "\"",
    colClasses = "character", stringsAsFactors = FALSE,
    check.names = FALSE, comment.char = "", fill = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", "NaN", "NULL")
  )
  names(dados) <- trimws(sub("^\ufeff", "", names(dados)))
  dados
}

pt_historico_vazio <- function() {
  data.frame(
    Modelo = character(), Localidade = character(), Epoca = integer(),
    Loss = numeric(), Val_Loss = numeric(), stringsAsFactors = FALSE
  )
}

pt_ler_historico <- function(caminho, modelo_esperado) {
  # Erros ficam visiveis na interface, sem recorrer a dados de previsoes.
  avisos <- character()
  tryCatch({
    modelo_esperado <- toupper(trimws(as.character(modelo_esperado)))
    dados <- pt_ler_csv(caminho)
    obrigatorias <- c("Modelo", "Localidade", "Epoca", "Loss")
    faltantes <- setdiff(obrigatorias, names(dados))
    if (length(faltantes)) {
      stop(
        "Colunas ausentes no histórico: ",
        paste(faltantes, collapse = ", "),
        ". Use o CSV gravado pelo treinamento do modelo esperado.",
        call. = FALSE
      )
    }
    if (!"Val_Loss" %in% names(dados)) {
      dados$Val_Loss <- rep(NA_character_, nrow(dados))
    }
    dados$Modelo <- toupper(trimws(dados$Modelo))
    if (identical(modelo_esperado, "GRU")) {
      dados$Modelo[dados$Modelo %in% "GRU_GLOBAL"] <- "GRU"
    }
    dados <- dados[dados$Modelo %in% modelo_esperado, , drop = FALSE]
    if (!nrow(dados)) {
      stop(
        "O histórico não contém registros do modelo ",
        modelo_esperado,
        ".",
        call. = FALSE
      )
    }

    dados <- data.frame(
      Modelo = dados$Modelo,
      Localidade = pt_normalizar_localidade(dados$Localidade),
      Epoca = pt_numero(dados$Epoca),
      Loss = pt_numero(dados$Loss),
      Val_Loss = pt_numero(dados$Val_Loss),
      stringsAsFactors = FALSE
    )
    if (identical(modelo_esperado, "GRU")) {
      global <- !is.na(dados$Localidade) & toupper(dados$Localidade) == "GLOBAL"
      if (any(!global)) {
        avisos <- c(
          avisos,
          "GRU: registros fora de Localidade='Global' foram ignorados."
        )
      }
      dados <- dados[global, , drop = FALSE]
      if (!nrow(dados)) {
        stop(
          "O histórico GRU deve identificar Localidade='Global'.",
          call. = FALSE
        )
      }
      dados$Localidade <- "Global"
    }
    chaves_validas <- !is.na(dados$Localidade) & nzchar(dados$Localidade) &
      is.finite(dados$Epoca) & dados$Epoca >= 1 &
      dados$Epoca <= .Machine$integer.max & dados$Epoca == floor(dados$Epoca)
    if (any(!chaves_validas)) {
      avisos <- c(avisos, paste(
        sum(!chaves_validas),
        "registro(s) com localidade ou época inválida foram ignorados."
      ))
    }
    dados <- dados[chaves_validas, , drop = FALSE]
    if (!nrow(dados)) {
      stop("Não há épocas válidas no histórico.", call. = FALSE)
    }
    dados$Epoca <- as.integer(dados$Epoca)
    if (anyDuplicated(dados[c("Localidade", "Epoca")])) {
      stop(paste(
        "Há épocas duplicadas para a mesma localidade.",
        "Use o histórico de uma única execução; não serão calculadas médias entre execuções."
      ), call. = FALSE)
    }
    for (coluna in c("Loss", "Val_Loss")) {
      invalidos <- !is.finite(dados[[coluna]]) | dados[[coluna]] < 0
      if (any(invalidos)) {
        avisos <- c(avisos, paste(
          sum(invalidos), "valor(es) ausente(s) ou inválido(s) em", coluna,
          "serão exibidos como lacunas."
        ))
      }
      dados[[coluna]][invalidos] <- NA_real_
    }
    if (!any(is.finite(dados$Loss))) {
      stop(
        "A coluna Loss não contém perdas de treinamento numéricas válidas.",
        call. = FALSE
      )
    }
    if (!any(is.finite(dados$Val_Loss))) {
      avisos <- c(
        avisos,
        "Val_Loss ausente: somente a perda de treino será desenhada."
      )
    }
    # Nao elimina Loss quando Val_Loss esta ausente (e vice-versa).
    dados <- dados[order(dados$Localidade, dados$Epoca), , drop = FALSE]
    rownames(dados) <- NULL
    list(dados = dados, erro = NULL, avisos = avisos, caminho = caminho)
  }, error = function(e) {
    list(
      dados = pt_historico_vazio(),
      erro = conditionMessage(e),
      avisos = avisos,
      caminho = caminho
    )
  })
}

# --- Histórico de gradientes do LSTM -------------------------------------------

pt_gradientes_vazio <- function() {
  data.frame(
    Localidade = character(),
    Epoca = integer(),
    Loss_Gradiente = numeric(),
    Norma_Gradiente = numeric(),
    Max_Gradiente = numeric(),
    Gradiente_NaN = logical(),
    Gradiente_Inf = logical(),
    stringsAsFactors = FALSE
  )
}

pt_logico_gradiente <- function(x) {
  texto <- tolower(trimws(as.character(x)))
  resultado <- rep(NA, length(texto))
  resultado[texto %in% c("true", "t", "1", "sim", "yes")] <- TRUE
  resultado[texto %in% c("false", "f", "0", "nao", "não", "no")] <- FALSE
  resultado
}

pt_ler_gradientes_lstm <- function(caminho) {
  avisos <- character()

  tryCatch({
    dados <- pt_ler_csv(caminho)

    obrigatorias <- c(
      "Localidade", "Epoca", "Norma_Gradiente",
      "Max_Gradiente", "Gradiente_NaN", "Gradiente_Inf"
    )

    faltantes <- setdiff(obrigatorias, names(dados))

    if (length(faltantes)) {
      stop(
        "Colunas ausentes no histórico de gradientes: ",
        paste(faltantes, collapse = ", "),
        call. = FALSE
      )
    }

    if (!"Loss_Gradiente" %in% names(dados)) {
      dados$Loss_Gradiente <- NA_character_
      avisos <- c(avisos, "Loss_Gradiente não encontrada.")
    }

    dados <- data.frame(
      Localidade = pt_normalizar_localidade(dados$Localidade),
      Epoca = pt_numero(dados$Epoca),
      Loss_Gradiente = pt_numero(dados$Loss_Gradiente),
      Norma_Gradiente = pt_numero(dados$Norma_Gradiente),
      Max_Gradiente = pt_numero(dados$Max_Gradiente),
      Gradiente_NaN = pt_logico_gradiente(dados$Gradiente_NaN),
      Gradiente_Inf = pt_logico_gradiente(dados$Gradiente_Inf),
      stringsAsFactors = FALSE
    )

    validos <- !is.na(dados$Localidade) &
      nzchar(dados$Localidade) &
      is.finite(dados$Epoca) &
      dados$Epoca >= 1 &
      dados$Epoca == floor(dados$Epoca)

    if (any(!validos)) {
      avisos <- c(
        avisos,
        paste(sum(!validos), "registro(s) inválido(s) foram ignorados.")
      )
      dados <- dados[validos, , drop = FALSE]
    }

    if (!nrow(dados)) {
      stop(
        "O histórico de gradientes não possui registros válidos.",
        call. = FALSE
      )
    }

    dados$Epoca <- as.integer(dados$Epoca)

    for (coluna in c(
      "Loss_Gradiente",
      "Norma_Gradiente",
      "Max_Gradiente"
    )) {
      invalidos <- !is.finite(dados[[coluna]]) | dados[[coluna]] < 0
      dados[[coluna]][invalidos] <- NA_real_
    }

    if (!any(is.finite(dados$Norma_Gradiente))) {
      stop(
        "Norma_Gradiente não possui valores numéricos válidos.",
        call. = FALSE
      )
    }

    if (anyDuplicated(dados[c("Localidade", "Epoca")])) {
      stop("Há épocas duplicadas para a mesma localidade.", call. = FALSE)
    }

    dados <- dados[order(dados$Localidade, dados$Epoca), , drop = FALSE]
    rownames(dados) <- NULL

    list(dados = dados, erro = NULL, avisos = avisos, caminho = caminho)
  }, error = function(e) {
    list(
      dados = pt_gradientes_vazio(),
      erro = conditionMessage(e),
      avisos = avisos,
      caminho = caminho
    )
  })
}

# --- Previsoes mensais: tabela e grafico de projecoes ----------------------------

pt_previsoes_vazias <- function() {
  data.frame(
    Modelo = character(), Localidade = character(), Data = as.Date(character()),
    Real = numeric(), Previsao = numeric(), AIC = numeric(), BIC = numeric(),
    stringsAsFactors = FALSE
  )
}

pt_carregar_previsoes <- function(data_dir = here::here("Previsoes_output")) {
  especificacoes <- data.frame(
    Modelo = c("LSTM", "GRU", "ARIMA", "ETS", "SARIMA", "SARIMAX"),
    Arquivo = c(
      "previsoes_lstm_pre_comp.csv", "previsoes_gru_global_pre_comp.csv",
      "previsoes_arima_pre_comp.csv", "previsoes_ets_pre_comp.csv",
      "previsoes_sarima_pre_comp.csv", "previsoes_sarimax_pre_comp.csv"
    ),
    stringsAsFactors = FALSE
  )

  resultados <- list()
  avisos <- character()

  for (i in seq_len(nrow(especificacoes))) {
    modelo <- especificacoes$Modelo[i]
    caminho <- file.path(data_dir, especificacoes$Arquivo[i])

    resultado <- tryCatch({
      if (!file.exists(caminho)) {
        stop("arquivo não encontrado: ", caminho, call. = FALSE)
      }

      # Todos os algoritmos geram CSV com Localidade e Previsao_<MODELO>.
      dados <- pt_ler_csv(caminho)

      if (!nrow(dados)) {
        stop("CSV sem registros: ", caminho, call. = FALSE)
      }

      coluna <- paste0("Previsao_", modelo)

      faltantes <- setdiff(
        c("Localidade", "Data", "Real", coluna),
        names(dados)
      )

      if (length(faltantes)) {
        stop(
          "colunas ausentes: ",
          paste(faltantes, collapse = ", "),
          call. = FALSE
        )
      }

      data.frame(
        Modelo = rep(modelo, nrow(dados)),
        Localidade = pt_normalizar_localidade(dados$Localidade),
        Data = as.Date(dados$Data),
        Real = pt_numero(dados$Real),
        Previsao = pt_numero(dados[[coluna]]),
        AIC = if ("AIC" %in% names(dados)) {
          pt_numero(dados$AIC)
        } else {
          rep(NA_real_, nrow(dados))
        },
        BIC = if ("BIC" %in% names(dados)) {
          pt_numero(dados$BIC)
        } else {
          rep(NA_real_, nrow(dados))
        },
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      avisos <<- c(
        avisos,
        paste0(modelo, ": ", conditionMessage(e))
      )
      NULL
    })

    if (!is.null(resultado)) {
      resultados[[modelo]] <- resultado
    }
  }

  dados <- if (length(resultados)) {
    do.call(rbind, resultados)
  } else {
    pt_previsoes_vazias()
  }

  # Para o seletor e os joins, usa grafia LSTM (ou SARIMAX) quando a
  # localidade da GRU tem a mesma chave sem acentos/caixa.
  rotulos <- dados[
    dados$Modelo %in% c("LSTM", "SARIMAX") &
      !is.na(dados$Localidade) & nzchar(dados$Localidade),
    c("Modelo", "Localidade"),
    drop = FALSE
  ]

  if (nrow(rotulos)) {
    rotulos <- rotulos[
      order(match(rotulos$Modelo, c("LSTM", "SARIMAX"))),
      ,
      drop = FALSE
    ]

    chaves_rotulos <- pt_chave_localidade(rotulos$Localidade)
    rotulos <- rotulos[!duplicated(chaves_rotulos), , drop = FALSE]
    chaves_rotulos <- pt_chave_localidade(rotulos$Localidade)

    idx <- match(pt_chave_localidade(dados$Localidade), chaves_rotulos)
    encontrados <- which(!is.na(idx))

    dados$Localidade[encontrados] <- rotulos$Localidade[idx[encontrados]]
  }

  rownames(dados) <- NULL
  list(dados = dados, avisos = avisos)
}

pt_tabela_previsoes <- function(dados) {
  if (!nrow(dados)) return(data.frame())

  reais <- dados |>
    dplyr::filter(!is.na(Data), !is.na(Real)) |>
    dplyr::arrange(dplyr::desc(Modelo == "GRU")) |>
    dplyr::group_by(Data) |>
    dplyr::summarise(Real = Real[1], .groups = "drop")

  previstas <- dados |>
    dplyr::filter(!is.na(Data)) |>
    dplyr::group_by(Data, Modelo) |>
    dplyr::summarise(
      Previsao = if (all(is.na(Previsao))) {
        NA_real_
      } else {
        Previsao[which(!is.na(Previsao))[1]]
      },
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = Modelo, values_from = Previsao)

  dplyr::full_join(reais, previstas, by = "Data") |>
    dplyr::arrange(Data) |>
    dplyr::rename("Frota Real" = Real)
}

# --- Projeções LSTM × GRU e métricas de erro ------------------------------------

pt_reais_projecao <- function(dados_completos, localidade) {
  reais <- dados_completos[
    (dados_completos$Localidade %in% localidade |
      (identical(localidade, "Pará") &
        dados_completos$Modelo == "GRU" &
        toupper(dados_completos$Localidade) == "GLOBAL")) &
      !is.na(dados_completos$Data) &
      is.finite(dados_completos$Real),
    c("Modelo", "Data", "Real"),
    drop = FALSE
  ]

  if (!nrow(reais)) {
    return(data.frame(
      Data = as.Date(character()),
      Real = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  reais |>
    dplyr::arrange(dplyr::desc(Modelo == "GRU")) |>
    dplyr::group_by(Data) |>
    dplyr::summarise(
      Real = dplyr::first(Real),
      .groups = "drop"
    ) |>
    dplyr::arrange(Data)
}

pt_dados_projecoes_comparacao <- function(dados, localidade) {
  base <- dados[
    dados$Modelo %in% c("LSTM", "GRU") &
      !is.na(dados$Data) &
      is.finite(dados$Previsao),
    ,
    drop = FALSE
  ]

  if (!nrow(base)) {
    return(data.frame(
      Data = as.Date(character()),
      Modelo = character(),
      Previsao = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  locais_modelos <- base[
    base$Localidade %in% localidade,
    ,
    drop = FALSE
  ]

  # Se a GRU estiver gravada como modelo global, a comparação com a
  # série estadual "Pará" usa o registro Global. Não se faz essa
  # substituição para municípios, pois isso misturaria escalas diferentes.
  if (identical(localidade, "Pará")) {
    gru_local <- locais_modelos[
      locais_modelos$Modelo == "GRU",
      ,
      drop = FALSE
    ]

    if (!nrow(gru_local)) {
      gru_global <- base[
        base$Modelo == "GRU" &
          toupper(base$Localidade) == "GLOBAL",
        ,
        drop = FALSE
      ]

      if (nrow(gru_global)) {
        gru_global$Localidade <- "Pará"
        locais_modelos <- rbind(
          locais_modelos[locais_modelos$Modelo != "GRU", , drop = FALSE],
          gru_global
        )
      }
    }
  }

  if (!nrow(locais_modelos)) {
    return(data.frame(
      Data = as.Date(character()),
      Modelo = character(),
      Previsao = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  locais_modelos |>
    dplyr::group_by(Data, Modelo) |>
    dplyr::summarise(
      Previsao = first(Previsao[is.finite(Previsao)]),
      .groups = "drop"
    ) |>
    dplyr::arrange(Data, Modelo)
}

pt_tabela_erros_projecoes <- function(
    projecoes,
    dados_completos,
    localidade
) {
  # Cada modelo e avaliado contra o Real presente no seu proprio arquivo.
  # Misturar o Real do LSTM com a previsao da GRU muda o MAPE calculado.
  reais <- dados_completos[
    (dados_completos$Localidade %in% localidade |
      (identical(localidade, "Pará") &
        dados_completos$Modelo == "GRU" &
        toupper(dados_completos$Localidade) == "GLOBAL")) &
      dados_completos$Modelo %in% c("LSTM", "GRU") &
      !is.na(dados_completos$Data) & is.finite(dados_completos$Real),
    c("Modelo", "Data", "Real"),
    drop = FALSE
  ]

  reais <- reais |>
    dplyr::group_by(Modelo, Data) |>
    dplyr::summarise(Real = dplyr::first(Real), .groups = "drop")

  avaliacao <- projecoes |>
    dplyr::inner_join(reais, by = c("Modelo", "Data")) |>
    dplyr::filter(
      is.finite(Real),
      is.finite(Previsao)
    )

  if (!nrow(avaliacao)) {
    return(data.frame())
  }

  resultados <- lapply(
    split(avaliacao, avaliacao$Modelo),
    function(dados_modelo) {
      erro <- dados_modelo$Previsao - dados_modelo$Real
      indices_mape <- dados_modelo$Real != 0

      mape <- if (any(indices_mape)) {
        mean(
          abs(
            erro[indices_mape] /
              dados_modelo$Real[indices_mape]
          )
        ) * 100
      } else {
        NA_real_
      }

      # Coeficiente de determinacao calculado no mesmo periodo das demais
      # metricas. SST=0 (serie real constante) ou um unico ponto: indefinido.
      sst <- sum((dados_modelo$Real - mean(dados_modelo$Real))^2)
      r2 <- if (nrow(dados_modelo) >= 2L && is.finite(sst) && sst > 0) {
        1 - sum(erro^2) / sst
      } else {
        NA_real_
      }

      data.frame(
        Modelo = dados_modelo$Modelo[1],
        Periodos_validos = nrow(dados_modelo),
        MAPE = mape,
        MAE = mean(abs(erro)),
        RMSE = sqrt(mean(erro^2)),
        R2 = r2,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, resultados)
}

pt_desenhar_projecoes <- function(
    projecoes,
    dados_completos,
    localidade
) {
  reais <- pt_reais_projecao(
    dados_completos,
    localidade
  )

  # Janela visual:
  # Todos os meses de jan/2016 ate dez/2031 (fim exclusivo em jan/2032).
  janela_inicio <- as.Date("2016-01-01")
  janela_fim <- as.Date("2031-12-01")
  janela_fim_exclusivo <- as.Date("2032-01-01")

  reais <- reais[
    !is.na(reais$Data) &
      reais$Data >= janela_inicio &
      reais$Data < janela_fim_exclusivo,
    ,
    drop = FALSE
  ]

  projecoes <- projecoes[
    !is.na(projecoes$Data) &
      projecoes$Data >= janela_inicio &
      projecoes$Data < janela_fim_exclusivo,
    ,
    drop = FALSE
  ]

  datas <- sort(unique(c(
    reais$Data,
    projecoes$Data
  )))

  valores <- c(
    reais$Real,
    projecoes$Previsao
  )

  valores <- valores[is.finite(valores)]

  if (!length(datas) || !length(valores)) {
    stop(
      "Não há projeções ou valores reais para desenhar.",
      call. = FALSE
    )
  }

  limite_y <- max(valores)
  if (limite_y <= 0) limite_y <- 1

  parametros <- graphics::par(
    mar = c(4.5, 6.5, 5.2, 1.2),
    las = 1
  )
  on.exit(graphics::par(parametros), add = TRUE)

  graphics::plot(
    datas,
    rep(NA_real_, length(datas)),
    type = "n",
    xlab = "Data",
    ylab = "",
    ylim = c(0, limite_y * 1.08),
    xlim = as.numeric(c(janela_inicio, janela_fim)),
    xaxt = "n",
    bty = "l"
  )

  graphics::mtext(
    "Quantidade de veículos",
    side = 2,
    line = 4.6,
    las = 0
  )

  marcas <- pretty(datas, n = 10)
  graphics::axis.Date(1, at = marcas, format = "%b/%Y")
  graphics::grid(nx = NA, ny = NULL, col = "#e5e7eb", lty = 1)

  graphics::mtext(
    "Projeções pré-compiladas: LSTM × GRU",
    side = 3,
    line = 3.4,
    font = 2,
    cex = 1.1
  )

  graphics::mtext(
    paste(
      "Localidade:", localidade,
      "| Janela: jan/2016 a dez/2031"
    ),
    side = 3,
    line = 2.2,
    cex = 0.85
  )

  if (nrow(reais)) {
    graphics::lines(
      reais$Data,
      reais$Real,
      col = "#333333",
      lwd = 2
    )

    graphics::points(
      reais$Data,
      reais$Real,
      col = "#333333",
      pch = 16,
      cex = 0.45
    )
  }

  cores <- c(
    LSTM = "#1D44B8",
    GRU = "#C0392B"
  )

  for (modelo in c("LSTM", "GRU")) {
    parte <- projecoes[
      projecoes$Modelo == modelo,
      ,
      drop = FALSE
    ]

    if (!nrow(parte)) next

    parte <- parte[order(parte$Data), , drop = FALSE]

    graphics::lines(
      parte$Data, parte$Previsao,
      col = cores[[modelo]], lwd = 2
    )
    graphics::points(
      parte$Data, parte$Previsao,
      col = cores[[modelo]], pch = 16, cex = 0.45
    )
  }

  if (nrow(reais)) {
    data_corte <- max(reais$Data)

    graphics::abline(
      v = as.numeric(data_corte),
      lty = 3,
      col = "#6B7280"
    )
  }

  graphics::legend(
    "topleft",
    legend = c("Real", "LSTM", "GRU"),
    col = c("#333333", "#1D44B8", "#C0392B"),
    lty = 1,
    lwd = 2,
    pch = 16,
    bty = "n",
    cex = 0.85
  )

  if (nrow(reais)) {
    graphics::mtext(
      "Linha tracejada: último período com valor real disponível",
      side = 1,
      line = 3.0,
      cex = 0.75,
      col = "#4B5563"
    )
  }

  invisible(NULL)
}

# --- Grafico comparativo de perdas (R nativo, sem ECharts) -----------------------

pt_historico_disponivel <- function(historico) {
  is.list(historico) &&
    is.null(historico$erro) &&
    is.data.frame(historico$dados) &&
    nrow(historico$dados) > 0L
}

pt_dados_perdas_comparacao <- function(historico_lstm, historico_gru, localidade) {
  partes <- list()

  # O LSTM possui um histórico separado para cada localidade.
  if (pt_historico_disponivel(historico_lstm)) {
    lstm <- historico_lstm$dados[
      historico_lstm$dados$Localidade %in% localidade,
      ,
      drop = FALSE
    ]

    if (nrow(lstm) > 0L) {
      partes[[length(partes) + 1L]] <- data.frame(
        Epoca = lstm$Epoca,
        Serie = "LSTM_Treino",
        Valor = lstm$Loss,
        stringsAsFactors = FALSE
      )
      partes[[length(partes) + 1L]] <- data.frame(
        Epoca = lstm$Epoca,
        Serie = "LSTM_Validacao",
        Valor = lstm$Val_Loss,
        stringsAsFactors = FALSE
      )
    }
  }

  # A GRU Global possui uma única curva, independentemente da localidade
  # selecionada na tabela.
  if (pt_historico_disponivel(historico_gru)) {
    gru <- historico_gru$dados[
      historico_gru$dados$Localidade %in% "Global",
      ,
      drop = FALSE
    ]

    if (nrow(gru) > 0L) {
      partes[[length(partes) + 1L]] <- data.frame(
        Epoca = gru$Epoca,
        Serie = "GRU_Treino",
        Valor = gru$Loss,
        stringsAsFactors = FALSE
      )
      partes[[length(partes) + 1L]] <- data.frame(
        Epoca = gru$Epoca,
        Serie = "GRU_Validacao",
        Valor = gru$Val_Loss,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(partes)) {
    return(data.frame(
      Epoca = integer(),
      Serie = character(),
      Valor = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  dados <- do.call(rbind, partes)
  dados$Epoca <- as.integer(dados$Epoca)
  dados$Valor <- as.numeric(dados$Valor)
  dados <- dados[
    is.finite(dados$Epoca) & dados$Epoca >= 1,
    ,
    drop = FALSE
  ]
  dados[order(dados$Epoca, dados$Serie), , drop = FALSE]
}

pt_desenhar_perdas <- function(dados, localidade) {
  if (!nrow(dados) || !any(is.finite(dados$Valor))) {
    stop("Não há perdas de treinamento válidas para desenhar.", call. = FALSE)
  }

  dados <- dados[order(dados$Epoca, dados$Serie), , drop = FALSE]
  valores <- dados$Valor[is.finite(dados$Valor)]
  limite_y <- max(valores)
  if (limite_y <= 0) limite_y <- 1

  eixo_x <- dados$Epoca[is.finite(dados$Epoca)]
  limite_x <- range(eixo_x)
  if (diff(limite_x) == 0) limite_x <- limite_x + c(-0.5, 0.5)
  marcas_x <- pretty(limite_x, n = 10)
  marcas_x <- sort(unique(c(min(eixo_x), marcas_x, max(eixo_x))))
  marcas_x <- marcas_x[
    marcas_x >= min(eixo_x) & marcas_x <= max(eixo_x) &
      marcas_x == floor(marcas_x)
  ]

  parametros <- graphics::par(mar = c(4.5, 5.2, 5.4, 1.2), las = 1)
  on.exit(graphics::par(parametros), add = TRUE)

  graphics::plot(
    eixo_x,
    rep(NA_real_, length(eixo_x)),
    type = "n",
    xaxt = "n",
    xlim = limite_x,
    ylim = c(0, limite_y * 1.08),
    xlab = "Época",
    ylab = "Perda (MSE no alvo normalizado)",
    bty = "l"
  )
  graphics::axis(1, at = marcas_x, labels = marcas_x)
  graphics::grid(nx = NA, ny = NULL, col = "#e5e7eb", lty = 1)
  graphics::mtext(
    "Perdas de treinamento e validação",
    side = 3,
    line = 3.6,
    font = 2,
    cex = 1.1
  )

  escopos <- character()
  if (any(grepl("^LSTM_", dados$Serie))) {
    escopos <- c(escopos, paste("LSTM:", localidade))
  }
  if (any(grepl("^GRU_", dados$Serie))) {
    escopos <- c(escopos, "GRU: modelo global")
  }
  graphics::mtext(
    paste(escopos, collapse = " | "),
    side = 3,
    line = 2.4,
    cex = 0.82
  )

  series <- c(
    "LSTM_Treino",
    "LSTM_Validacao",
    "GRU_Treino",
    "GRU_Validacao"
  )
  rotulos <- c(
    LSTM_Treino = "LSTM - treino",
    LSTM_Validacao = "LSTM - validação",
    GRU_Treino = "GRU - treino",
    GRU_Validacao = "GRU - validação"
  )
  cores <- c(
    LSTM_Treino = "#1D44B8",
    LSTM_Validacao = "#1D44B8",
    GRU_Treino = "#C0392B",
    GRU_Validacao = "#C0392B"
  )
  tipos <- c(
    LSTM_Treino = 1,
    LSTM_Validacao = 2,
    GRU_Treino = 1,
    GRU_Validacao = 2
  )
  pontos <- c(
    LSTM_Treino = 16,
    LSTM_Validacao = 1,
    GRU_Treino = 17,
    GRU_Validacao = 2
  )

  for (serie in series) {
    serie_dados <- dados[dados$Serie == serie, , drop = FALSE]
    if (!nrow(serie_dados) || !any(is.finite(serie_dados$Valor))) next

    serie_dados <- serie_dados[order(serie_dados$Epoca), , drop = FALSE]

    # Nao une lacunas de epocas nem prolonga a GRU ate a ultima epoca da LSTM.
    grupos <- cumsum(c(TRUE, diff(serie_dados$Epoca) != 1L))

    for (grupo in unique(grupos)) {
      pos <- which(grupos == grupo)
      graphics::lines(
        serie_dados$Epoca[pos],
        serie_dados$Valor[pos],
        col = cores[[serie]],
        lty = tipos[[serie]],
        lwd = 2
      )
    }

    validos <- is.finite(serie_dados$Valor)
    graphics::points(
      serie_dados$Epoca[validos],
      serie_dados$Valor[validos],
      col = cores[[serie]],
      pch = pontos[[serie]],
      cex = 0.45
    )
  }

  presentes <- series[vapply(
    series,
    function(serie) any(
      is.finite(dados$Valor[dados$Serie == serie])
    ),
    logical(1)
  )]

  graphics::legend(
    x = mean(limite_x),
    y = graphics::par("usr")[4],
    xjust = 0.5,
    yjust = 0,
    xpd = NA,
    ncol = 2,
    legend = unname(rotulos[presentes]),
    col = unname(cores[presentes]),
    lty = unname(tipos[presentes]),
    pch = unname(pontos[presentes]),
    lwd = 2,
    bg = "white",
    bty = "n",
    cex = 0.82
  )

  invisible(NULL)
}

# --- Gráfico dos gradientes ----------------------------------------------------

pt_desenhar_gradientes <- function(dados, localidade) {
  dados <- dados[order(dados$Epoca), , drop = FALSE]

  valores <- c(dados$Norma_Gradiente, dados$Max_Gradiente)
  valores <- valores[is.finite(valores)]

  if (!nrow(dados) || !length(valores)) {
    stop("Não há gradientes numéricos para desenhar.", call. = FALSE)
  }

  eixo_x <- dados$Epoca[is.finite(dados$Epoca)]
  limite_x <- range(eixo_x)
  if (diff(limite_x) == 0) limite_x <- limite_x + c(-0.5, 0.5)

  limite_y <- max(valores)
  if (limite_y <= 0) limite_y <- 1

  par(mar = c(4.5, 5.2, 5.2, 1.2), las = 1)

  plot(
    eixo_x,
    rep(NA_real_, length(eixo_x)),
    type = "n",
    xlim = limite_x,
    ylim = c(0, limite_y * 1.08),
    xaxt = "n",
    xlab = "Época",
    ylab = "Magnitude do gradiente",
    bty = "l"
  )

  axis(1, at = pretty(limite_x, n = 10))
  grid(nx = NA, ny = NULL, col = "#e5e7eb", lty = 1)

  mtext(
    "Gradientes registrados durante o treinamento do LSTM",
    side = 3, line = 3.3, font = 2, cex = 1.05
  )
  mtext(
    paste("Localidade:", localidade),
    side = 3, line = 2.1, cex = 0.82
  )

  series <- c("Norma_Gradiente", "Max_Gradiente")
  rotulos <- c(
    Norma_Gradiente = "Norma global",
    Max_Gradiente = "Maior componente"
  )
  cores <- c(
    Norma_Gradiente = "#1D44B8",
    Max_Gradiente = "#C0392B"
  )
  tipos <- c(Norma_Gradiente = 1, Max_Gradiente = 2)

  presentes <- character()

  for (serie in series) {
    ok <- is.finite(dados[[serie]])
    if (!any(ok)) next

    x <- dados$Epoca[ok]
    y <- dados[[serie]][ok]
    ordem <- order(x)
    x <- x[ordem]
    y <- y[ordem]

    if (length(x) > 1L) {
      lines(x, y, col = cores[[serie]], lty = tipos[[serie]], lwd = 2)
    }

    points(x, y, col = cores[[serie]], pch = 16, cex = 0.45)
    presentes <- c(presentes, serie)
  }

  if (length(presentes)) {
    legend(
      "topright",
      legend = unname(rotulos[presentes]),
      col = unname(cores[presentes]),
      lty = unname(tipos[presentes]),
      lwd = 2,
      bty = "n",
      cex = 0.82
    )
  }

  instabilidade <-
    any(dados$Gradiente_NaN %in% TRUE, na.rm = TRUE) ||
    any(dados$Gradiente_Inf %in% TRUE, na.rm = TRUE)

  if (instabilidade) {
    mtext(
      "Atenção: foram detectados valores NaN ou Inf.",
      side = 3, line = 0.8, col = "#B91C1C", cex = 0.78
    )
  }

  invisible(NULL)
}

# --- Interface -----------------------------------------------------------------

previsoes_todos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      bs4Dash::box(
        title = "Resultados dos modelos e treinamento LSTM × GRU",
        width = 12,
        status = "primary",
        solidHeader = FALSE,

        shiny::tags$p(
          shiny::tags$strong("Versão do módulo: "),
          shiny::tags$code(previsoes_todos_versao)
        ),

        shiny::fluidRow(
          shiny::column(
            8,
            shiny::selectInput(
              ns("select_local"),
              "MUNICÍPIO OU ESTADO",
              choices = character()
            )
          ),
          shiny::column(
            4,
            shiny::tags$br(),
            shiny::actionButton(
              ns("recarregar"),
              "Recarregar arquivos"
            )
          )
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Projeções pré-compiladas — LSTM × GRU",
        width = 12,
        status = "success",
        solidHeader = FALSE,
        maximizable = TRUE,

        shiny::tags$p(
          "Comparação das projeções pré-calculadas pelos dois modelos. ",
          "A linha real é mostrada apenas nos períodos em que há observação; ",
          "para ela, usa-se preferencialmente o valor real do CSV da GRU. ",
          "As datas e os valores apresentados são os registrados nos arquivos ",
          "CSV de previsão de cada modelo."
        ),

        shiny::uiOutput(ns("status_projecoes")),

        shiny::plotOutput(
          ns("grafico_projecoes"),
          height = "520px"
        ),

        shiny::tags$p(
          "As métricas abaixo consideram somente períodos com valor real ",
          "e previsão simultaneamente disponíveis para cada modelo. ",
          "Cada previsão é comparada ao valor real do próprio arquivo; ",
          "projeções futuras não entram no cálculo. O R² pode ser negativo ",
          "e fica indefinido quando há menos de dois meses válidos ou ",
          "quando os valores reais não variam."
        ),

        DT::dataTableOutput(
          ns("tabela_erros_projecoes")
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Perdas de treinamento e validação — LSTM × GRU",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        maximizable = TRUE,

        shiny::tags$p(
          "Valores registrados pelo Keras durante fit(): Loss no treino e ",
          "Val_Loss na validação interna, por época. A função de perda é MSE ",
          "no alvo normalizado de cada rede. O LSTM corresponde à localidade selecionada; ",
          "a GRU corresponde ao modelo global. Os históricos são lidos de ",
          "historico_perdas_lstm.csv e historico_perdas_gru.csv, na pasta data. ",
          "Este gráfico não usa as previsões mensais."
        ),

        shiny::tags$p(
          "Eixo X: épocas. Eixo Y: perda MSE. Linhas contínuas: treino; ",
          "tracejadas: validação. Cada curva termina na última época gravada."
        ),

        shiny::uiOutput(ns("status_historico")),
        shiny::plotOutput(ns("grafico_perdas"), height = "520px"),
        shiny::downloadButton(
          ns("baixar_perdas_png"),
          "Baixar gráfico PNG"
        ),
        shiny::downloadButton(
          ns("baixar_perdas_pdf"),
          "Baixar gráfico PDF"
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Diagnóstico dos gradientes do LSTM",
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        maximizable = TRUE,

        shiny::tags$p(
          "A norma global e o maior componente dos gradientes são ",
          "apresentados por época. Esses indicadores permitem verificar ",
          "possíveis instabilidades numéricas durante o treinamento."
        ),

        shiny::uiOutput(ns("status_gradientes")),

        shiny::plotOutput(
          ns("grafico_gradientes"),
          height = "460px"
        ),

        DT::dataTableOutput(
          ns("tabela_gradientes")
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Perdas registradas por época — LSTM × GRU",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        DT::dataTableOutput(ns("tabela_perdas"))
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Resultados pré-compilados por algoritmo",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,

        shiny::tags$p(
          "Valores reais e previsões mensais. Esta tabela é independente ",
          "das perdas de treinamento exibidas acima. A coluna Frota Real ",
          "prefere o CSV da GRU, quando disponível."
        ),

        shiny::uiOutput(ns("status_previsoes")),
        DT::dataTableOutput(ns("tabela_resultados"))
      )
    )
  )
}

# --- Servidor ------------------------------------------------------------------

previsoes_todos_server <- function(
    id,
    data_dir = here::here("data"),
    previsoes_dir = here::here("Previsoes_output")
) {
  shiny::moduleServer(id, function(input, output, session) {
    caminho_historico_lstm <- normalizePath(
      file.path(data_dir, "historico_perdas_lstm.csv"),
      winslash = "/",
      mustWork = FALSE
    )

    caminho_historico_gru <- normalizePath(
      file.path(data_dir, "historico_perdas_gru.csv"),
      winslash = "/",
      mustWork = FALSE
    )

    caminho_gradientes_lstm <- normalizePath(
      file.path(data_dir, "historico_gradientes_lstm.csv"),
      winslash = "/",
      mustWork = FALSE
    )

    arquivos <- shiny::reactive({
      input$recarregar

      list(
        historico_lstm = pt_ler_historico(
          caminho_historico_lstm,
          modelo_esperado = "LSTM"
        ),
        historico_gru = pt_ler_historico(
          caminho_historico_gru,
          modelo_esperado = "GRU"
        ),
        gradientes_lstm = pt_ler_gradientes_lstm(
          caminho_gradientes_lstm
        ),
        previsoes = pt_carregar_previsoes(previsoes_dir)
      )
    })

    shiny::observe({
      carregado <- arquivos()

      locais_historico <- unique(
        carregado$historico_lstm$dados$Localidade
      )

      locais <- sort(unique(c(
        locais_historico,
        carregado$previsoes$dados$Localidade
      )))

      locais <- locais[!is.na(locais) & nzchar(locais)]

      atual <- shiny::isolate(input$select_local)

      inicial <- if ("Pará" %in% locais_historico) {
        "Pará"
      } else {
        if (length(locais_historico)) {
          sort(locais_historico)[1]
        } else {
          head(locais, 1)
        }
      }

      selecionado <- if (
        length(atual) == 1L && atual %in% locais
      ) {
        atual
      } else {
        inicial
      }

      shiny::updateSelectInput(
        session,
        "select_local",
        choices = locais,
        selected = selecionado
      )
    })

    projecoes_comparacao <- shiny::reactive({
      carregado <- arquivos()

      dados <- pt_dados_projecoes_comparacao(
        dados = carregado$previsoes$dados,
        localidade = input$select_local
      )

      modelos <- unique(dados$Modelo)

      shiny::validate(shiny::need(
        all(c("LSTM", "GRU") %in% modelos),
        paste(
          "Não existem projeções simultâneas de LSTM e GRU para",
          input$select_local,
          ". Verifique a localidade e os arquivos pré-compilados."
        )
      ))

      list(
        projecoes = dados,
        completos = carregado$previsoes$dados
      )
    })

    output$status_projecoes <- shiny::renderUI({
      carregado <- arquivos()

      dados <- pt_dados_projecoes_comparacao(
        dados = carregado$previsoes$dados,
        localidade = input$select_local
      )

      modelos <- unique(dados$Modelo)
      faltantes <- setdiff(c("LSTM", "GRU"), modelos)

      if (length(faltantes)) {
        return(shiny::tags$p(
          style = "color:#B91C1C;",
          paste(
            "Modelos sem projeções para a localidade selecionada:",
            paste(faltantes, collapse = ", ")
          )
        ))
      }

      shiny::tags$p(
        style = "color:#4B5563;",
        paste(
          "Modelos exibidos: LSTM e GRU.",
          "Localidade:",
          input$select_local
        )
      )
    })

    output$grafico_projecoes <- shiny::renderPlot({
      dados <- projecoes_comparacao()

      pt_desenhar_projecoes(
        projecoes = dados$projecoes,
        dados_completos = dados$completos,
        localidade = input$select_local
      )
    }, res = 96, execOnResize = TRUE)

    output$tabela_erros_projecoes <- DT::renderDataTable({
      dados <- projecoes_comparacao()

      tabela <- pt_tabela_erros_projecoes(
        projecoes = dados$projecoes,
        dados_completos = dados$completos,
        localidade = input$select_local
      )

      shiny::validate(shiny::need(
        nrow(tabela) > 0L,
        paste(
          "Não existem períodos observados com Real e Previsao",
          "simultaneamente disponíveis para calcular as métricas."
        )
      ))

      names(tabela) <- c(
        "Modelo",
        "Períodos válidos",
        "MAPE (%)",
        "MAE (veículos)",
        "RMSE (veículos)",
        "R²"
      )

      DT::datatable(
        tabela,
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = "t",
          ordering = FALSE
        )
      ) |>
        DT::formatRound(
          columns = c(
            "MAPE (%)",
            "MAE (veículos)",
            "RMSE (veículos)"
          ),
          digits = 2
        ) |>
        DT::formatRound(
          columns = "R²",
          digits = 3
        )
    })

    perdas_comparacao <- shiny::reactive({
      carregado <- arquivos()

      dados <- pt_dados_perdas_comparacao(
        historico_lstm = carregado$historico_lstm,
        historico_gru = carregado$historico_gru,
        localidade = input$select_local
      )

      shiny::validate(shiny::need(
        nrow(dados) > 0L,
        paste(
          "Não há histórico de treinamento válido para exibir.",
          "Verifique os arquivos historico_perdas_lstm.csv e historico_perdas_gru.csv."
        )
      ))

      shiny::validate(shiny::need(
        any(is.finite(dados$Valor)),
        "Não há valores numéricos de perda para desenhar."
      ))

      dados
    })

    output$status_historico <- shiny::renderUI({
      carregado <- arquivos()
      historico_lstm <- carregado$historico_lstm
      historico_gru <- carregado$historico_gru

      aviso <- function(texto) {
        shiny::tags$p(style = "color:#9a3412;", texto)
      }

      mensagens <- list(
        shiny::tags$p(
          shiny::tags$strong("LSTM — arquivo: "),
          shiny::tags$code(caminho_historico_lstm)
        ),
        shiny::tags$p(
          shiny::tags$strong("GRU — arquivo: "),
          shiny::tags$code(caminho_historico_gru)
        )
      )

      if (!is.null(historico_lstm$erro)) {
        mensagens <- c(
          mensagens,
          list(aviso(paste("LSTM:", historico_lstm$erro)))
        )
      }

      if (!is.null(historico_gru$erro)) {
        mensagens <- c(
          mensagens,
          list(aviso(paste("GRU:", historico_gru$erro)))
        )
      }

      dados_lstm <- historico_lstm$dados[
        historico_lstm$dados$Localidade %in% input$select_local,
        ,
        drop = FALSE
      ]

      if (nrow(dados_lstm)) {
        mensagens <- c(
          mensagens,
          list(shiny::tags$p(
            "LSTM: ", nrow(dados_lstm), " épocas; ",
            sum(is.finite(dados_lstm$Loss)), " perdas de treino; ",
            sum(is.finite(dados_lstm$Val_Loss)), " perdas de validação."
          ))
        )
      } else if (is.null(historico_lstm$erro)) {
        mensagens <- c(
          mensagens,
          list(aviso(
            "LSTM: selecione uma localidade com histórico; somente a GRU disponível será exibida."
          ))
        )
      }

      dados_gru <- historico_gru$dados[
        historico_gru$dados$Localidade %in% "Global",
        ,
        drop = FALSE
      ]

      if (nrow(dados_gru)) {
        mensagens <- c(
          mensagens,
          list(shiny::tags$p(
            "GRU global: ", nrow(dados_gru), " épocas; ",
            sum(is.finite(dados_gru$Loss)), " perdas de treino; ",
            sum(is.finite(dados_gru$Val_Loss)), " perdas de validação."
          ))
        )
      }

      avisos <- c(
        if (length(historico_lstm$avisos)) {
          paste("LSTM:", historico_lstm$avisos)
        },
        if (length(historico_gru$avisos)) {
          paste("GRU:", historico_gru$avisos)
        }
      )

      if (length(avisos)) {
        mensagens <- c(mensagens, lapply(avisos, aviso))
      }

      mensagens <- c(
        mensagens,
        list(shiny::tags$p(
          style = "color:#4b5563;",
          paste(
            "A LSTM usa resíduo aditivo padronizado;",
            "a GRU usa ruído multiplicativo padronizado.",
            "Além dos alvos diferentes, as amostras e o número de",
            "atualizações por época diferem entre a rede local e a global.",
            "Este gráfico descreve o treinamento: menor perda entre",
            "modelos não demonstra maior precisão das previsões."
          )
        ))
      )

      do.call(shiny::tagList, mensagens)
    })

    output$grafico_perdas <- shiny::renderPlot({
      pt_desenhar_perdas(
        perdas_comparacao(),
        input$select_local
      )
    },
    res = 96,
    execOnResize = TRUE,
    alt = "Perdas de treinamento e validação do LSTM e da GRU por época."
    )

    gradientes_selecionados <- shiny::reactive({
      resultado <- arquivos()$gradientes_lstm

      shiny::validate(shiny::need(
        is.null(resultado$erro),
        paste("Histórico de gradientes indisponível:", resultado$erro)
      ))

      dados <- resultado$dados[
        resultado$dados$Localidade %in% input$select_local,
        ,
        drop = FALSE
      ]

      shiny::validate(shiny::need(
        nrow(dados) > 0L,
        paste(
          "Não há gradientes registrados para a localidade",
          input$select_local,
          "."
        )
      ))

      dados
    })

    output$status_gradientes <- shiny::renderUI({
      resultado <- arquivos()$gradientes_lstm

      if (!is.null(resultado$erro)) {
        return(shiny::tags$p(
          style = "color:#B91C1C;",
          paste(
            "Arquivo de gradientes não disponível:",
            resultado$erro
          )
        ))
      }

      dados <- resultado$dados[
        resultado$dados$Localidade %in% input$select_local,
        ,
        drop = FALSE
      ]

      if (!nrow(dados)) {
        return(shiny::tags$p(
          style = "color:#B91C1C;",
          "Não há registros de gradientes para a localidade selecionada."
        ))
      }

      houve_nan <- any(
        dados$Gradiente_NaN %in% TRUE,
        na.rm = TRUE
      )

      houve_inf <- any(
        dados$Gradiente_Inf %in% TRUE,
        na.rm = TRUE
      )

      mensagem <- paste(
        "Arquivo:", caminho_gradientes_lstm,
        "| Épocas registradas:", nrow(dados)
      )

      mensagem <- paste(
        mensagem,
        if (houve_nan || houve_inf) {
          "| Atenção: foram encontrados NaN ou Inf."
        } else {
          "| Nenhum NaN ou Inf foi registrado."
        }
      )

      shiny::tags$p(
        style = "color:#4B5563;",
        mensagem
      )
    })

    output$grafico_gradientes <- shiny::renderPlot({
      pt_desenhar_gradientes(
        dados = gradientes_selecionados(),
        localidade = input$select_local
      )
    }, res = 96, execOnResize = TRUE)

    output$tabela_gradientes <- DT::renderDataTable({
      dados <- gradientes_selecionados()

      tabela <- dados[
        ,
        c(
          "Epoca",
          "Loss_Gradiente",
          "Norma_Gradiente",
          "Max_Gradiente",
          "Gradiente_NaN",
          "Gradiente_Inf"
        ),
        drop = FALSE
      ]

      names(tabela) <- c(
        "Época",
        "Perda usada no diagnóstico",
        "Norma global",
        "Maior componente",
        "NaN",
        "Inf"
      )

      colunas_numericas <- c(
        "Perda usada no diagnóstico",
        "Norma global",
        "Maior componente"
      )

      DT::datatable(
        tabela,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, "asc")))
      ) |>
        DT::formatSignif(
          columns = colunas_numericas,
          digits = 7
        )
    })

    output$tabela_perdas <- DT::renderDataTable({
      dados <- perdas_comparacao()

      tabela <- dados |>
        tidyr::pivot_wider(
          names_from = Serie,
          values_from = Valor
        ) |>
        dplyr::arrange(Epoca)

      ordem <- c(
        "Epoca",
        "LSTM_Treino",
        "LSTM_Validacao",
        "GRU_Treino",
        "GRU_Validacao"
      )

      tabela <- tabela[
        ,
        intersect(ordem, names(tabela)),
        drop = FALSE
      ]

      rotulos <- c(
        Epoca = "Época",
        LSTM_Treino = "LSTM — treino",
        LSTM_Validacao = "LSTM — validação",
        GRU_Treino = "GRU — treino",
        GRU_Validacao = "GRU — validação"
      )

      DT::datatable(
        tabela,
        rownames = FALSE,
        colnames = unname(rotulos[names(tabela)]),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, "asc"))
        )
      ) |>
        DT::formatSignif(
          columns = names(tabela)[names(tabela) != "Epoca"],
          digits = 7
        )
    })

    output$status_previsoes <- shiny::renderUI({
      avisos <- arquivos()$previsoes$avisos

      if (!length(avisos)) return(NULL)

      shiny::tags$details(
        shiny::tags$summary(
          "Arquivos de previsão indisponíveis (não impedem o gráfico de perdas)"
        ),
        shiny::tags$p(
          "Pasta de previsões: ",
          shiny::tags$code(previsoes_dir)
        ),
        shiny::tags$ul(
          lapply(avisos, shiny::tags$li)
        )
      )
    })

    output$tabela_resultados <- DT::renderDataTable({
      shiny::req(input$select_local)

      dados <- arquivos()$previsoes$dados

      dados <- dados[
        dados$Localidade %in% input$select_local,
        ,
        drop = FALSE
      ]

      tabela <- pt_tabela_previsoes(dados)

      shiny::validate(shiny::need(
        nrow(tabela) > 0L,
        "Não há previsões pré-compiladas para esta localidade."
      ))

      colunas <- names(tabela)[
        vapply(tabela, is.numeric, logical(1))
      ]

      DT::datatable(
        tabela,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        )
      ) |>
        DT::formatRound(
          columns = colunas,
          digits = 0
        )
    })

    nome_download <- function(extensao) {
      local <- if (length(input$select_local) == 1L) {
        input$select_local
      } else {
        "global"
      }

      local <- iconv(
        local,
        from = "",
        to = "ASCII//TRANSLIT",
        sub = ""
      )

      local <- gsub("[^A-Za-z0-9_-]+", "_", local)

      paste0(
        "perdas_lstm_gru_",
        local,
        ".",
        extensao
      )
    }

    output$baixar_perdas_png <- shiny::downloadHandler(
      filename = function() nome_download("png"),
      contentType = "image/png",
      content = function(file) {
        dados <- perdas_comparacao()

        grDevices::png(
          file,
          width = 2400,
          height = 1400,
          res = 300,
          bg = "white"
        )

        on.exit(grDevices::dev.off(), add = TRUE)

        pt_desenhar_perdas(
          dados,
          input$select_local
        )
      }
    )

    output$baixar_perdas_pdf <- shiny::downloadHandler(
      filename = function() nome_download("pdf"),
      contentType = "application/pdf",
      content = function(file) {
        dados <- perdas_comparacao()

        grDevices::pdf(
          file,
          width = 8,
          height = 4.7,
          onefile = FALSE
        )

        on.exit(grDevices::dev.off(), add = TRUE)

        pt_desenhar_perdas(
          dados,
          input$select_local
        )
      }
    )

    invisible(list(
      perdas = perdas_comparacao,
      projecoes = projecoes_comparacao,
      arquivos = arquivos
    ))
  })
}