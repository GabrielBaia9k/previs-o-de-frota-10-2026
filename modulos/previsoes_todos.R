# ==============================================================================
# previsoes_todos.R | COMPARACAO-NOVE-MODELOS-13-EXPORTACAO-VALIDACAO
#
# Previsões: nove arquivos CSV em Previsoes_output/.
# Históricos e gradientes: permanecem em data/.
# Este módulo não treina modelos nem altera arquivos de dados.
# ==============================================================================

previsoes_todos_versao <- "COMPARACAO-NOVE-MODELOS-13-EXPORTACAO-VALIDACAO"

# As ordens de leitura, exibição e preferência do Real são independentes.
pt_catalogo_modelos <- data.frame(
  Modelo = c("ARIMA", "ETS", "SARIMA", "SARIMAX", "LSTM", "GRU", "XGBOOST", "PROPHET", "TCN"),
  Cor = c("#0891B2", "#D97706", "#059669", "#7C3AED", "#1D44B8",
    "#C0392B", "#DB2777", "#8B5A2B", "#4D7C0F"),
  Ordem_Leitura = c(3L, 4L, 5L, 6L, 1L, 2L, 7L, 8L, 9L),
  Prioridade_Real = c(6L, 5L, 4L, 3L, 2L, 1L, 7L, 8L, 9L),
  stringsAsFactors = FALSE
)
pt_catalogo_modelos$Arquivo <- paste0("previsoes_",
  tolower(pt_catalogo_modelos$Modelo), "_pre_comp.csv")
pt_catalogo_modelos$Arquivo[pt_catalogo_modelos$Modelo == "GRU"] <-
  "previsoes_gru_global_pre_comp.csv"
pt_modelos <- pt_catalogo_modelos$Modelo
pt_cores <- stats::setNames(pt_catalogo_modelos$Cor, pt_modelos)
pt_prioridade_real <- pt_modelos[order(pt_catalogo_modelos$Prioridade_Real)]
pt_redes_perdas <- c("LSTM", "GRU", "TCN")
pt_series_perdas <- as.vector(rbind(paste0(pt_redes_perdas, "_Treino"),
  paste0(pt_redes_perdas, "_Validacao")))
pt_rotulos_perdas <- stats::setNames(paste(rep(pt_redes_perdas, each = 2L),
  rep(c("- treino", "- validação"), length(pt_redes_perdas))), pt_series_perdas)

# --- Leitura e normalização ----------------------------------------------------

pt_normalizar_localidade <- function(x) {
  x <- trimws(as.character(x))
  estado <- toupper(x) %in% c(
    "PARÁ", "PARA", "PARÁ (GERAL)", "PARA (GERAL)"
  )
  x[estado] <- "Pará"
  x
}

pt_chave_localidade <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- chartr(
    "ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ",
    "AAAAAEEEEIIIIOOOOOUUUUC",
    x
  )
  trimws(gsub("[^A-Z0-9]+", " ", x))
}

pt_numero <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(sub(
    ",", ".", trimws(as.character(x)), fixed = TRUE
  )))
}

pt_ler_csv <- function(caminho) {
  if (!file.exists(caminho) || dir.exists(caminho)) {
    stop("Arquivo não encontrado: ", caminho, call. = FALSE)
  }

  primeira <- readLines(
    caminho, n = 1L, warn = FALSE, encoding = "UTF-8"
  )

  if (length(primeira) == 0L || !nzchar(primeira)) {
    stop("O arquivo está vazio: ", caminho, call. = FALSE)
  }

  separador <- if (grepl(";", primeira, fixed = TRUE)) ";" else ","

  dados <- utils::read.table(
    caminho,
    header = TRUE,
    sep = separador,
    quote = "\"",
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = "",
    fill = FALSE,
    fileEncoding = "UTF-8-BOM",
    na.strings = c("", "NA", "NaN", "NULL")
  )

  names(dados) <- trimws(sub("^\ufeff", "", names(dados)))
  dados
}

# --- Históricos de perdas -----------------------------------------------------

pt_historico_vazio <- function() {
  data.frame(
    Modelo = character(),
    Localidade = character(),
    Epoca = integer(),
    Loss = numeric(),
    Val_Loss = numeric(),
    stringsAsFactors = FALSE
  )
}

pt_ler_historico <- function(caminho, modelo_esperado) {
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
        modelo_esperado, ".",
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
      global <- !is.na(dados$Localidade) &
        toupper(dados$Localidade) == "GLOBAL"

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

    chaves_validas <- !is.na(dados$Localidade) &
      nzchar(dados$Localidade) &
      is.finite(dados$Epoca) &
      dados$Epoca >= 1 &
      dados$Epoca <= .Machine$integer.max &
      dados$Epoca == floor(dados$Epoca)

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
          sum(invalidos),
          "valor(es) ausente(s) ou inválido(s) em",
          coluna,
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

    dados <- dados[order(dados$Localidade, dados$Epoca), , drop = FALSE]
    rownames(dados) <- NULL

    list(
      dados = dados,
      erro = NULL,
      avisos = avisos,
      caminho = caminho
    )
  }, error = function(e) {
    list(
      dados = pt_historico_vazio(),
      erro = conditionMessage(e),
      avisos = avisos,
      caminho = caminho
    )
  })
}

# --- Histórico de gradientes do LSTM ------------------------------------------

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
      "Loss_Gradiente", "Norma_Gradiente", "Max_Gradiente"
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

    list(
      dados = dados,
      erro = NULL,
      avisos = avisos,
      caminho = caminho
    )
  }, error = function(e) {
    list(
      dados = pt_gradientes_vazio(),
      erro = conditionMessage(e),
      avisos = avisos,
      caminho = caminho
    )
  })
}

# --- Previsões mensais --------------------------------------------------------

pt_previsoes_vazias <- function() {
  data.frame(
    Modelo = character(),
    Localidade = character(),
    Data = as.Date(character()),
    Real = numeric(),
    Previsao = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    stringsAsFactors = FALSE
  )
}

pt_carregar_previsoes <- function(
    data_dir = here::here("Previsoes_output"), especificacoes = NULL
) {
  if (is.null(especificacoes)) especificacoes <- pt_catalogo_modelos[
    order(pt_catalogo_modelos$Ordem_Leitura), c("Modelo", "Arquivo")]

  resultados <- list()
  avisos <- character()

  for (i in seq_len(nrow(especificacoes))) {
    modelo <- especificacoes$Modelo[i]
    caminho <- file.path(data_dir, especificacoes$Arquivo[i])

    resultado <- tryCatch({
      if (!file.exists(caminho)) {
        stop("arquivo não encontrado: ", caminho, call. = FALSE)
      }

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

  rotulos <- dados[
    dados$Modelo %in% c("LSTM", "SARIMAX") &
      !is.na(dados$Localidade) &
      nzchar(dados$Localidade),
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

    idx <- match(
      pt_chave_localidade(dados$Localidade),
      chaves_rotulos
    )
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
    tidyr::pivot_wider(
      names_from = Modelo,
      values_from = Previsao
    )

  dplyr::full_join(reais, previstas, by = "Data") |>
    dplyr::arrange(Data) |>
    dplyr::rename("Frota Real" = Real)
}

# --- Comparação dos nove modelos ---------------------------------------------

pt_filtrar_previsoes_localidade <- function(dados, localidade) {
  if (!length(localidade)) {
    return(dados[FALSE, , drop = FALSE])
  }

  chave <- pt_chave_localidade(
    pt_normalizar_localidade(localidade)
  )
  chaves <- pt_chave_localidade(
    pt_normalizar_localidade(dados$Localidade)
  )

  resultado <- dados[chaves %in% chave, , drop = FALSE]

  # Compatibilidade com saídas antigas que usavam Global para o estado.
  if ("PARA" %in% chave && !any(resultado$Modelo == "GRU")) {
    global <- dados[
      dados$Modelo == "GRU" & chaves %in% "GLOBAL",
      ,
      drop = FALSE
    ]

    if (nrow(global)) {
      global$Localidade <- "Pará"
      resultado <- rbind(resultado, global)
    }
  }

  resultado
}

pt_reais_projecao <- function(dados_completos, localidade) {
  reais <- pt_filtrar_previsoes_localidade(
    dados_completos, localidade
  ) |>
    dplyr::filter(!is.na(Data), is.finite(Real)) |>
    dplyr::mutate(
      Prioridade = match(Modelo, pt_prioridade_real)
    ) |>
    dplyr::arrange(Prioridade)

  reais |>
    dplyr::group_by(Data) |>
    dplyr::summarise(
      Valores_distintos = dplyr::n_distinct(Real),
      Real = dplyr::first(Real),
      Fonte = dplyr::first(Modelo),
      .groups = "drop"
    ) |>
    dplyr::arrange(Data)
}

pt_dados_projecoes_comparacao <- function(dados, localidade) {
  pt_filtrar_previsoes_localidade(dados, localidade) |>
    dplyr::filter(
      Modelo %in% pt_modelos,
      !is.na(Data),
      is.finite(Previsao)
    ) |>
    dplyr::group_by(Data, Modelo) |>
    dplyr::summarise(
      Previsao = dplyr::first(Previsao),
      .groups = "drop"
    ) |>
    dplyr::arrange(Data, Modelo)
}

# Produção: Real de 2016 a 2024. Validação: limites do treino da rodada.
# MASE/RMSSE sazonais usam pares do mesmo mês em anos consecutivos (m = 12).
# Referências: Hewamalage et al. (2023), DOI 10.1007/s10618-022-00894-5,
# e Forecasting: Principles and Practice, seção 5.8 (escalas sazonais).
pt_escalas_sazonais <- function(
    dados_completos, localidade,
    inicio = as.Date("2016-01-01"), fim = as.Date("2024-12-01")
) {
  if (!inherits(inicio, "Date") || !inherits(fim, "Date") ||
      length(inicio) != 1L || length(fim) != 1L ||
      is.na(inicio) || is.na(fim) || inicio > fim) {
    stop("Limites inválidos para a escala sazonal.", call. = FALSE)
  }
  inicio <- as.Date(format(inicio, "%Y-%m-01"))
  fim <- as.Date(format(fim, "%Y-%m-01"))
  historico <- pt_filtrar_previsoes_localidade(
    dados_completos, localidade
  ) |>
    dplyr::filter(
      !is.na(Data), Data >= inicio, Data <= fim,
      is.finite(Real)
    )

  referencia <- historico |>
    dplyr::filter(Modelo == "SARIMAX") |>
    dplyr::transmute(Data, Real, Fonte = "SARIMAX")

  # Fallback somente se não houver Real histórico SARIMAX. Uma escala zero
  # não autoriza trocar a fonte; nessa situação a métrica fica indisponível.
  if (!nrow(referencia)) {
    referencia <- pt_reais_projecao(historico, localidade)
  }

  escala <- list(
    inicio = inicio, fim = fim, periodo = 12L,
    fonte = if (nrow(referencia)) {
      paste(unique(referencia$Fonte), collapse = ", ")
    } else {
      NA_character_
    },
    pares = 0L, mae = NA_real_, mse = NA_real_
  )
  if (!nrow(referencia)) return(escala)

  # Calendário completo: meses sem Real ficam NA. Remover lacunas antes de
  # deslocar 12 linhas criaria pares que não estão separados por 12 meses.
  calendario <- seq(inicio, fim, by = "month")
  if (length(calendario) <= 12L) return(escala)
  meses <- as.Date(format(referencia$Data, "%Y-%m-01"))
  valores <- referencia$Real[match(calendario, meses)]
  atuais <- valores[seq.int(13L, length(valores))]
  anteriores <- valores[seq_len(length(valores) - 12L)]
  conhecidos <- is.finite(atuais) & is.finite(anteriores)
  escala$pares <- as.integer(sum(conhecidos))

  if (escala$pares > 0L) {
    diferencas <- atuais[conhecidos] - anteriores[conhecidos]
    d1 <- mean(abs(diferencas))
    d2 <- mean(diferencas^2)
    escala$mae <- if (is.finite(d1)) d1 else NA_real_
    escala$mse <- if (is.finite(d2)) d2 else NA_real_
  }
  escala
}

pt_razao_erro <- function(numerador, denominador) {
  if (length(numerador) != 1L || length(denominador) != 1L ||
      !is.finite(numerador) || !is.finite(denominador) || denominador <= 0) {
    return(NA_real_)
  }
  valor <- numerador / denominador
  if (is.finite(valor)) valor else NA_real_
}

# Metadados de datas/contagens continuam no cálculo, mas não são colunas da UI.
pt_tabela_erros_exibicao <- function(tabela) {
  colunas <- c(
    Modelo = "Modelo", MAPE = "MAPE (%)", MAE = "MAE (veículos)",
    RMSE = "RMSE (veículos)", R2 = "R²", ME = "ME (veículos)",
    MASE = "MASE", RMSSE = "RMSSE", RelMAE = "RelMAE (SARIMAX)"
  )
  resultado <- tabela[, names(colunas), drop = FALSE]
  attr(resultado, "escala_sazonal") <- NULL
  names(resultado) <- unname(colunas)
  for (coluna in names(resultado)[-1L]) {
    resultado[[coluna]][!is.finite(resultado[[coluna]])] <- NA_real_
  }
  resultado
}

pt_widget_erros <- function(tabela, ano = NULL) {
  exibida <- pt_tabela_erros_exibicao(tabela)
  if (!is.null(ano)) exibida <- cbind(Ano = ano, exibida)
  DT::datatable(exibida, rownames = FALSE,
    options = list(paging = FALSE, scrollX = TRUE,
      columnDefs = list(list(targets = "_all", defaultContent = "N/A")),
      dom = "t", ordering = FALSE)) |>
    DT::formatRound(columns = c("MAPE (%)", "MAE (veículos)",
      "RMSE (veículos)", "ME (veículos)"), digits = 2) |>
    DT::formatRound(columns = c("R²", "MASE", "RMSSE", "RelMAE (SARIMAX)"), digits = 3)
}

# Preparação compartilhada entre o total e os anos de uma mesma origem.
pt_preparar_avaliacao <- function(
    dados_completos, localidade, modelos, inicio_escala, fim_escala
) {
  dados_localidade <- pt_filtrar_previsoes_localidade(
    dados_completos, localidade
  )
  escala <- pt_escalas_sazonais(
    dados_completos, localidade, inicio = inicio_escala, fim = fim_escala
  )

  # Cada previsão é avaliada contra o Real do próprio modelo.
  reais <- dados_localidade |>
    dplyr::filter(
      Modelo %in% modelos,
      !is.na(Data),
      is.finite(Real)
    ) |>
    dplyr::group_by(Modelo, Data) |>
    dplyr::summarise(
      Real = dplyr::first(Real),
      .groups = "drop"
    )

  referencia <- dados_localidade |>
    dplyr::filter(
      Modelo == "SARIMAX", !is.na(Data), is.finite(Previsao)
    ) |>
    dplyr::group_by(Data) |>
    dplyr::summarise(
      Previsao = dplyr::first(Previsao), .groups = "drop"
    )

  list(reais = reais, referencia = referencia, escala = escala)
}

pt_tabela_erros_projecoes <- function(
    projecoes,
    dados_completos,
    localidade,
    periodo = "comum",
    inicio_escala = as.Date("2016-01-01"),
    fim_escala = as.Date("2024-12-01"),
    preparacao = NULL
) {
  periodo <- match.arg(periodo, c("comum", "individual"))

  modelos <- pt_modelos[
    pt_modelos %in% unique(projecoes$Modelo)
  ]

  if (!length(modelos)) return(data.frame())

  if (is.null(preparacao)) {
    preparacao <- pt_preparar_avaliacao(
      dados_completos, localidade, modelos, inicio_escala, fim_escala
    )
  }
  reais <- preparacao$reais
  referencia <- preparacao$referencia
  escala <- preparacao$escala

  avaliacao <- projecoes |>
    dplyr::inner_join(reais, by = c("Modelo", "Data")) |>
    dplyr::filter(
      is.finite(Real),
      is.finite(Previsao)
    )

  # Modelos somente futuros ficam com métricas indisponíveis.
  # Eles não eliminam a avaliação dos modelos com observações.
  if (periodo == "comum" && nrow(avaliacao)) {
    datas_modelos <- split(
      as.numeric(avaliacao$Data),
      avaliacao$Modelo
    )

    datas_comuns <- Reduce(intersect, datas_modelos)

    avaliacao <- avaliacao[
      as.numeric(avaliacao$Data) %in% datas_comuns,
      ,
      drop = FALSE
    ]
  }

  resultados <- lapply(modelos, function(modelo) {
    parte <- avaliacao[
      avaliacao$Modelo == modelo,
      ,
      drop = FALSE
    ]

    n <- nrow(parte)
    erro <- parte$Previsao - parte$Real
    sem_zero <- parte$Real != 0
    mae <- if (n) mean(abs(erro)) else NA_real_
    rmse <- if (n) sqrt(mean(erro^2)) else NA_real_
    me <- if (n) mean(parte$Real - parte$Previsao) else NA_real_
    if (!is.finite(me)) me <- NA_real_

    # Reavaliar SARIMAX contra os MESMOS Real/meses da linha atual. O MAE
    # exibido na linha SARIMAX pode usar outra amostra no período individual.
    indices_ref <- match(parte$Data, referencia$Data)
    cobre_periodo <- n > 0L && all(!is.na(indices_ref))
    mae_ref <- if (cobre_periodo) {
      mean(abs(parte$Real - referencia$Previsao[indices_ref]))
    } else {
      NA_real_
    }
    relmae <- pt_razao_erro(mae, mae_ref)
    motivo_relmae <- if (!n) {
      "Sem meses observados com previsão."
    } else if (!cobre_periodo) {
      "SARIMAX sem previsão em todos os meses avaliados."
    } else if (!is.finite(mae_ref)) {
      "MAE do SARIMAX indisponível."
    } else if (mae_ref == 0) {
      "MAE do SARIMAX igual a zero nos meses avaliados."
    } else if (!is.finite(relmae)) {
      "Razão de erros indisponível."
    } else {
      ""
    }

    sst <- if (n) {
      sum((parte$Real - mean(parte$Real))^2)
    } else {
      NA_real_
    }

    data.frame(
      Modelo = modelo,
      Inicio = if (n) min(parte$Data) else as.Date(NA_character_),
      Fim = if (n) max(parte$Data) else as.Date(NA_character_),
      Periodos_validos = n,
      Periodos_MAPE = sum(sem_zero),

      MAPE = if (any(sem_zero)) {
        mean(abs(
          erro[sem_zero] / parte$Real[sem_zero]
        )) * 100
      } else {
        NA_real_
      },

      MAE = mae,
      RMSE = rmse,

      R2 = if (n >= 2L && is.finite(sst) && sst > 0) {
        1 - sum(erro^2) / sst
      } else {
        NA_real_
      },

      ME = me,
      MASE = pt_razao_erro(mae, escala$mae),
      RMSSE = pt_razao_erro(rmse, sqrt(escala$mse)),
      RelMAE = relmae,
      MAE_SARIMAX_ref = mae_ref,
      Motivo_RelMAE = motivo_relmae,

      stringsAsFactors = FALSE
    )
  })

  tabela <- do.call(rbind, resultados)
  attr(tabela, "escala_sazonal") <- escala
  tabela
}

# --- Validação temporal: CSVs independentes produzidos na máquina local --------

pt_localidades_validacao <- function(dados, abrangencia = c("geral", "municipio")) {
  abrangencia <- match.arg(abrangencia)
  if (!"Localidade" %in% names(dados)) return(character())
  locais <- unique(pt_normalizar_localidade(dados$Localidade))
  locais <- locais[!is.na(locais) & nzchar(locais)]
  estado <- pt_chave_localidade(locais) == "PARA"
  sort(locais[if (abrangencia == "geral") estado else !estado])
}

pt_carregar_validacao <- function(
    previsoes_dir, ano_teste, ano_fim_teste = ano_teste, modelos = pt_modelos
) {
  ano <- pt_numero(ano_teste)
  fim <- pt_numero(ano_fim_teste)
  if (length(ano) != 1L || !is.finite(ano) || ano != floor(ano) ||
      ano < 1900 || ano > 9998) stop("Ano de teste inválido.", call. = FALSE)
  if (length(fim) != 1L || !is.finite(fim) || fim != floor(fim) ||
      fim < ano || fim > 9998) stop("Ano final do teste inválido.", call. = FALSE)
  ano <- as.integer(ano)
  fim <- as.integer(fim)
  inicio_teste <- as.Date(sprintf("%04d-01-01", ano))
  fim_teste <- as.Date(sprintf("%04d-12-01", fim))
  modelos <- pt_modelos[pt_modelos %in% modelos]
  sufixo <- if (ano == fim) sprintf("%04d", ano) else sprintf("%04d_%04d", ano, fim)
  especificacoes <- data.frame(Modelo = modelos,
    Arquivo = file.path(modelos, paste0("previsoes_", tolower(modelos),
      "_teste_", sufixo, ".csv")), stringsAsFactors = FALSE)
  pasta <- file.path(previsoes_dir, "validacao_temporal")
  presentes <- file.exists(file.path(pasta, especificacoes$Arquivo)) &
    !dir.exists(file.path(pasta, especificacoes$Arquivo))
  # Um único leitor para todos os modelos. Ausências são esperadas enquanto
  # os outros controladores não tiverem gerado os CSVs da mesma janela.
  carregado <- pt_carregar_previsoes(
    pasta, especificacoes = especificacoes[presentes, , drop = FALSE]
  )
  carregado$modelos_ausentes <- modelos[!presentes]
  if (length(carregado$modelos_ausentes)) carregado$avisos <- c(carregado$avisos,
    paste0("Sem arquivo desta janela: ", paste(carregado$modelos_ausentes, collapse = ", "), "."))
  # Rejeitar somente o modelo inválido; a chave inclui Modelo para que dois
  # algoritmos possam prever a mesma localidade no mesmo mês.
  for (modelo in unique(carregado$dados$Modelo)) {
    dados <- carregado$dados[carregado$dados$Modelo == modelo, , drop = FALSE]
    erro <- if (anyNA(dados$Data) || anyNA(dados$Localidade) ||
        any(!nzchar(dados$Localidade)) || any(format(dados$Data, "%d") != "01")) {
      "Há datas ou localidades inválidas no CSV da rodada."
    } else if (anyDuplicated(dados[c("Modelo", "Localidade", "Data")])) {
      "Há meses duplicados por localidade no CSV da rodada."
    } else if (any(dados$Data > fim_teste) || any(is.finite(dados$Previsao) &
        (dados$Data < inicio_teste | dados$Data > fim_teste))) {
      "O CSV contém datas ou previsões fora da janela de teste solicitada."
    } else NULL
    if (!is.null(erro)) {
      carregado$dados <- carregado$dados[carregado$dados$Modelo != modelo, , drop = FALSE]
      carregado$avisos <- c(carregado$avisos, paste0(modelo, ": ", erro))
    }
  }
  rownames(carregado$dados) <- NULL
  carregado$modelos_disponiveis <- modelos[modelos %in% unique(carregado$dados$Modelo)]
  carregado$ano_teste <- ano
  carregado$ano_fim_teste <- fim
  carregado$meses_teste <- as.integer(12L * (fim - ano + 1L))
  carregado$inicio_teste <- inicio_teste
  carregado$fim_teste <- fim_teste
  carregado
}

pt_avaliar_validacao <- function(carregado, localidade, periodo = c("comum", "individual")) {
  periodo <- match.arg(periodo)
  dados <- pt_filtrar_previsoes_localidade(carregado$dados, localidade)
  if (!nrow(dados)) stop("Não há resultados da rodada para esta localidade.", call. = FALSE)
  datas_treino <- dados$Data[dados$Data < carregado$inicio_teste]
  fim_treino <- carregado$inicio_teste - 1L
  fim_escala <- as.Date(format(fim_treino, "%Y-%m-01"))
  # Uma escala por localidade/origem, compartilhada por total e anos. Quando
  # SARIMAX está presente, preservar seu histórico de referência, mesmo se
  # outro modelo trouxer um início diferente. Nunca incluir Real do teste.
  datas_referencia <- dados$Data[dados$Modelo == "SARIMAX" & dados$Data < carregado$inicio_teste]
  inicio_escala <- if (length(datas_referencia)) min(datas_referencia) else
    if (length(datas_treino)) min(datas_treino) else fim_escala
  projecoes <- pt_dados_projecoes_comparacao(dados, localidade)
  modelos <- pt_modelos[pt_modelos %in% unique(projecoes$Modelo)]
  modelos_locais <- pt_modelos[pt_modelos %in% unique(dados$Modelo)]
  datas <- seq(carregado$inicio_teste, carregado$fim_teste, by = "month")
  # Calendário completo por modelo: match nunca cruza dados de algoritmos.
  mensal <- do.call(rbind, lapply(modelos_locais, function(modelo) {
    parte <- dados[dados$Modelo == modelo, , drop = FALSE]
    idx <- match(datas, parte$Data)
    data.frame(Modelo = modelo, Data = datas, Real = parte$Real[idx],
      Previsao = parte$Previsao[idx], Horizonte = seq_along(datas))
  }))
  mensal$Real[!is.finite(mensal$Real)] <- NA_real_
  mensal$Previsao[!is.finite(mensal$Previsao)] <- NA_real_
  mensal$Erro <- mensal$Real - mensal$Previsao
  mensal$Erro_absoluto <- abs(mensal$Erro)
  mensal$Erro[!is.finite(mensal$Erro)] <- NA_real_
  mensal$Erro_absoluto[!is.finite(mensal$Erro_absoluto)] <- NA_real_
  pareados <- is.finite(mensal$Real) & is.finite(mensal$Previsao)
  datas_modelos <- split(as.numeric(mensal$Data[pareados]), mensal$Modelo[pareados])
  datas_comuns <- if (length(datas_modelos)) Reduce(intersect, datas_modelos) else numeric()
  mensal$Usado_metricas <- pareados &
    (periodo == "individual" | as.numeric(mensal$Data) %in% datas_comuns)
  preparacao <- if (length(modelos)) pt_preparar_avaliacao(
    dados, localidade, modelos, inicio_escala, fim_escala) else NULL
  # Aplicar a mesma amostra comum no total e em cada bloco anual. Modelos
  # sem qualquer par observado não eliminam a avaliação dos demais.
  if (periodo == "comum" && !is.null(preparacao)) preparacao$reais <-
    preparacao$reais[as.numeric(preparacao$reais$Data) %in% datas_comuns, , drop = FALSE]
  tabela <- pt_tabela_erros_projecoes(projecoes, dados, localidade,
    periodo = periodo, inicio_escala = inicio_escala,
    fim_escala = fim_escala, preparacao = preparacao)
  # Mesmo treino/escala em todos os blocos anuais desta origem. Permite ler
  # o comportamento em 2025 separadamente do erro de todo o horizonte.
  anuais <- lapply(seq.int(carregado$ano_teste, carregado$ano_fim_teste), function(ano) {
    parte <- projecoes[format(projecoes$Data, "%Y") == as.character(ano), , drop = FALSE]
    erros <- pt_tabela_erros_projecoes(parte, dados, localidade,
      periodo = periodo, inicio_escala = inicio_escala,
      fim_escala = fim_escala, preparacao = preparacao)
    if (!nrow(erros)) return(NULL)
    erros$Ano <- as.integer(ano)
    erros
  })
  anuais <- Filter(Negate(is.null), anuais)
  anuais <- if (length(anuais)) do.call(rbind, anuais) else data.frame()
  cobertura <- do.call(rbind, lapply(modelos_locais, function(modelo) {
    parte <- mensal[mensal$Modelo == modelo, , drop = FALSE]
    treino <- dados$Data[dados$Modelo == modelo & dados$Data < carregado$inicio_teste]
    data.frame(Modelo = modelo,
      Inicio_treino = if (length(treino)) min(treino) else as.Date(NA_character_),
      Fim_treino = if (length(treino)) max(treino) else as.Date(NA_character_),
      Meses_Real = sum(is.finite(parte$Real)),
      Meses_Previsao = sum(is.finite(parte$Previsao)),
      Meses_pareados = sum(is.finite(parte$Real) & is.finite(parte$Previsao)),
      Meses_avaliados = sum(parte$Usado_metricas))
  }))
  avisos <- character()
  ausentes_local <- setdiff(carregado$modelos_disponiveis, modelos_locais)
  if (length(ausentes_local)) avisos <- c(avisos, paste0(
    "Sem dados desta localidade: ", paste(ausentes_local, collapse = ", "), "."))
  reais <- pt_reais_projecao(dados, localidade)
  divergentes <- reais$Valores_distintos > 1L
  if (any(divergentes)) avisos <- c(avisos, paste0(
    "Valores reais divergentes entre modelos em ", sum(divergentes), " mês(es), sendo ",
    sum(divergentes & reais$Data >= carregado$inicio_teste),
    " no teste. Cada métrica usa o Real do próprio CSV; confira a comparação."))
  if (nrow(tabela)) {
    sem_referencia <- nzchar(tabela$Motivo_RelMAE)
    if (any(sem_referencia)) avisos <- c(avisos, paste0(
      tabela$Modelo[sem_referencia], ": RelMAE indisponível. ", tabela$Motivo_RelMAE[sem_referencia]))
  }
  list(completos = dados, projecoes = projecoes, metricas = tabela,
    metricas_anuais = anuais, mensal = mensal, meses_teste = carregado$meses_teste,
    inicio_treino = if (length(datas_treino)) min(datas_treino) else as.Date(NA_character_),
    fim_treino = fim_treino,
    meses_validos = length(datas_comuns), cobertura = cobertura, avisos = avisos,
    periodo = periodo)
}

# Compatibilidade com chamadas anteriores; o painel usa as funções genéricas.
pt_carregar_validacao_sarimax <- function(previsoes_dir, ano_teste, ano_fim_teste = ano_teste) {
  pt_carregar_validacao(previsoes_dir, ano_teste, ano_fim_teste, modelos = "SARIMAX")
}

pt_avaliar_validacao_sarimax <- function(carregado, localidade) {
  resultado <- pt_avaliar_validacao(carregado, localidade, periodo = "individual")
  resultado$mensal <- resultado$mensal[c("Data", "Real", "Previsao", "Horizonte", "Erro", "Erro_absoluto")]
  resultado[c("completos", "projecoes", "metricas", "metricas_anuais", "mensal",
    "meses_teste", "inicio_treino", "fim_treino", "meses_validos")]
}

# --- Preparacao compartilhada e escrita dos downloads --------------------------
# Recebe os arquivos ja lidos no cache. Avaliar uma vez por origem preserva a
# amostra comum e a escala usadas no total e nos respectivos blocos anuais.
pt_preparar_exportacao_validacao <- function(arquivos, localidade, periodo = "comum") {
  periodo <- match.arg(periodo, c("comum", "individual"))
  if (length(localidade) != 1L || is.na(localidade) || !nzchar(localidade)) {
    stop("Selecione uma localidade para exportar a validacao.", call. = FALSE)
  }
  rodadas <- list()
  avisos <- character()
  for (carregado in arquivos) {
    origem <- paste0(carregado$ano_teste, "–", carregado$ano_fim_teste)
    if (length(carregado$avisos)) avisos <- c(avisos, paste0(origem, ": ", carregado$avisos))
    locais <- pt_filtrar_previsoes_localidade(carregado$dados, localidade)
    if (!nrow(locais)) {
      avisos <- c(avisos, paste0(origem, ": sem resultados para ", localidade, "."))
      next
    }
    avaliacao <- tryCatch(pt_avaliar_validacao(carregado, localidade, periodo),
      error = function(e) {
        avisos <<- c(avisos, paste0(origem, ": ", conditionMessage(e)))
        NULL
      })
    if (is.null(avaliacao)) next
    if (length(avaliacao$avisos)) avisos <- c(avisos, paste0(origem, ": ", avaliacao$avisos))
    if (!nrow(avaliacao$projecoes)) {
      avisos <- c(avisos, paste0(origem, ": sem previsoes validas para ", localidade, "."))
      next
    }
    rodadas[[as.character(carregado$ano_teste)]] <- list(
      ano_teste = carregado$ano_teste, ano_fim_teste = carregado$ano_fim_teste,
      inicio_teste = carregado$inicio_teste, fim_teste = carregado$fim_teste,
      meses_teste = carregado$meses_teste, localidade = localidade,
      avaliacao = avaliacao)
  }
  list(rodadas = rodadas, avisos = unique(avisos))
}

# CSV para Excel/R: UTF-8 com BOM, separador ';', decimal ',', NA explicito,
# datas ISO e valores originais, sem aplicar o arredondamento visual do DT.
pt_gravar_csv_validacao <- function(tabela, arquivo) {
  if (!is.data.frame(tabela) || !nrow(tabela)) stop("Nao ha resultados para exportar.", call. = FALSE)
  for (nome in names(tabela)) {
    if (inherits(tabela[[nome]], "Date")) tabela[[nome]] <- format(tabela[[nome]], "%Y-%m-%d")
    if (is.character(tabela[[nome]])) tabela[[nome]] <- enc2utf8(tabela[[nome]])
    if (is.numeric(tabela[[nome]])) tabela[[nome]][!is.finite(tabela[[nome]])] <- NA_real_
  }
  utils::write.csv2(tabela, arquivo, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  bytes <- readBin(arquivo, what = "raw", n = file.info(arquivo)$size)
  writeBin(c(as.raw(c(239L, 187L, 191L)), bytes), arquivo)
  invisible(arquivo)
}

# --- Exportacao da validacao: transformar resultados ja avaliados --------------
# rodadas: lista de registros com ano_teste, ano_fim_teste, inicio_teste,
# fim_teste (primeiro dia do ultimo mes), meses_teste, localidade e avaliacao.
# avaliacao e o retorno existente de pt_avaliar_validacao(); estas funcoes
# nao leem CSVs, nao ajustam modelos e nao recalculam nenhuma metrica.

pt_metadados_exportacao_validacao <- function(rodada, modelos) {
  avaliacao <- rodada$avaliacao
  cobertura <- avaliacao$cobertura
  n <- length(modelos)
  idx <- match(as.character(modelos), cobertura$Modelo)
  ultimo_mes <- as.Date(format(as.Date(rodada$fim_teste), "%Y-%m-01"))
  # A grade mensal do leitor usa o primeiro dia; no CSV o fim da janela
  # identifica o ultimo dia do mes. Data/Horizonte continuam na grade original.
  fim_teste <- seq(ultimo_mes, by = "month", length.out = 2L)[2L] - 1L
  data.frame(
    Localidade = rep(as.character(rodada$localidade), n),
    Modelo = as.character(modelos),
    Ano_inicio_teste = rep(as.integer(rodada$ano_teste), n),
    Ano_fim_teste = rep(as.integer(rodada$ano_fim_teste), n),
    Inicio_treino_modelo = as.Date(cobertura$Inicio_treino[idx]),
    Ultimo_mes_historico_treino = as.Date(cobertura$Fim_treino[idx]),
    Corte_treino = rep(as.Date(avaliacao$fim_treino), n),
    Inicio_teste = rep(as.Date(rodada$inicio_teste), n),
    Fim_teste = rep(fim_teste, n),
    Meses_teste = rep(as.integer(rodada$meses_teste), n),
    Periodo_metricas = rep(as.character(avaliacao$periodo), n),
    stringsAsFactors = FALSE
  )
}

# Uma linha por origem/modelo/mes da localidade selecionada. Inclui lacunas
# mensais e flag Usado_metricas; nao aplica filtro de legenda ou paginacao DT.
pt_exportar_previsoes_validacao <- function(rodadas) {
  partes <- lapply(rodadas, function(rodada) {
    tabela <- rodada$avaliacao$mensal
    if (is.null(tabela) || !nrow(tabela)) return(NULL)
    campos <- c("Data", "Horizonte", "Real", "Previsao", "Erro", "Erro_absoluto", "Usado_metricas")
    valores <- tabela[campos]
    valores$Data <- as.Date(valores$Data)
    valores$Horizonte <- as.integer(valores$Horizonte)
    valores$Usado_metricas <- as.logical(valores$Usado_metricas)
    for (nome in c("Real", "Previsao", "Erro", "Erro_absoluto")) {
      valores[[nome]][!is.finite(valores[[nome]])] <- NA_real_
    }
    cbind(pt_metadados_exportacao_validacao(rodada, tabela$Modelo), valores)
  })
  partes <- Filter(Negate(is.null), partes)
  if (!length(partes)) return(data.frame())
  resultado <- do.call(rbind, partes)
  resultado <- resultado[order(resultado$Ano_inicio_teste, resultado$Modelo, resultado$Data), , drop = FALSE]
  rownames(resultado) <- NULL
  resultado
}

# Uma linha por origem/modelo/escopo e, no escopo anual, ano previsto. TOTAL
# tem Ano_avaliado=NA; o mesmo ano observado pode aparecer em varias origens.
pt_exportar_metricas_validacao <- function(rodadas) {
  partes <- lapply(rodadas, function(rodada) {
    avaliacao <- rodada$avaliacao
    # Copiar o atributo antes de consolidar tabelas. Os blocos anuais usam a
    # mesma escala do total desta origem; nao estimar outra escala por ano.
    escala <- attr(avaliacao$metricas, "escala_sazonal")
    campos <- c("Inicio", "Fim", "Periodos_validos", "Periodos_MAPE", "MAPE", "MAE", "RMSE", "R2",
      "ME", "MASE", "RMSSE", "RelMAE", "MAE_SARIMAX_ref", "Motivo_RelMAE")
    calendario <- seq(as.Date(rodada$inicio_teste), as.Date(rodada$fim_teste), by = "month")
    anos_calendario <- as.integer(format(calendario, "%Y"))
    blocos <- lapply(c("total", "anual"), function(escopo) {
      tabela <- if (escopo == "total") avaliacao$metricas else avaliacao$metricas_anuais
      if (is.null(tabela) || !nrow(tabela)) return(NULL)
      n <- nrow(tabela)
      anos <- if (escopo == "total") rep(NA_integer_, n) else as.integer(tabela$Ano)
      limites <- lapply(seq_len(n), function(i) {
        # Limites NOMINAIS na grade mensal. Inicio/Fim da tabela original
        # identificam os meses efetivamente avaliados, com suas contagens.
        indices <- if (escopo == "total") seq_along(calendario) else which(anos_calendario == anos[i])
        if (length(indices)) as.integer(range(indices)) else c(NA_integer_, NA_integer_)
      })
      valores <- tabela[campos]
      attr(valores, "escala_sazonal") <- NULL
      valores$Inicio <- as.Date(valores$Inicio)
      valores$Fim <- as.Date(valores$Fim)
      for (nome in c("MAPE", "MAE", "RMSE", "R2", "ME", "MASE", "RMSSE", "RelMAE", "MAE_SARIMAX_ref")) {
        valores[[nome]][!is.finite(valores[[nome]])] <- NA_real_
      }
      escala_colunas <- data.frame(
        Fonte_escala = rep(if (is.null(escala$fonte)) NA_character_ else as.character(escala$fonte), n),
        Inicio_escala = rep(if (is.null(escala$inicio)) as.Date(NA_character_) else as.Date(escala$inicio), n),
        Fim_escala = rep(if (is.null(escala$fim)) as.Date(NA_character_) else as.Date(escala$fim), n),
        Periodo_escala = rep(if (is.null(escala$periodo)) NA_integer_ else as.integer(escala$periodo), n),
        Pares_escala = rep(if (is.null(escala$pares)) NA_integer_ else as.integer(escala$pares), n),
        MAE_escala = rep(if (is.null(escala$mae)) NA_real_ else as.numeric(escala$mae), n),
        MSE_escala = rep(if (is.null(escala$mse)) NA_real_ else as.numeric(escala$mse), n),
        stringsAsFactors = FALSE
      )
      for (nome in c("MAE_escala", "MSE_escala")) escala_colunas[[nome]][!is.finite(escala_colunas[[nome]])] <- NA_real_
      cbind(pt_metadados_exportacao_validacao(rodada, tabela$Modelo),
        data.frame(Escopo = rep(escopo, n), Ano_avaliado = anos,
          Horizonte_inicial = vapply(limites, function(x) x[1L], integer(1L)),
          Horizonte_final = vapply(limites, function(x) x[2L], integer(1L)),
          stringsAsFactors = FALSE), valores, escala_colunas)
    })
    blocos <- Filter(Negate(is.null), blocos)
    if (length(blocos)) do.call(rbind, blocos) else NULL
  })
  partes <- Filter(Negate(is.null), partes)
  if (!length(partes)) return(data.frame())
  resultado <- do.call(rbind, partes)
  resultado <- resultado[order(resultado$Ano_inicio_teste, resultado$Modelo,
    match(resultado$Escopo, c("total", "anual")), resultado$Ano_avaliado, na.last = TRUE), , drop = FALSE]
  rownames(resultado) <- NULL
  resultado
}

pt_desenhar_projecoes <- function(
    projecoes,
    dados_completos,
    localidade,
    data_corte = NULL
) {
  opcoes_anteriores <- options(OutDec = ".")
  on.exit(options(opcoes_anteriores), add = TRUE)

  reais <- pt_reais_projecao(
    dados_completos, localidade
  )

  datas <- sort(unique(c(
    reais$Data,
    projecoes$Data
  )))
  datas <- datas[!is.na(datas)]

  valores <- c(reais$Real, projecoes$Previsao)
  valores <- valores[is.finite(valores)]

  if (!length(datas) || !length(valores)) {
    stop(
      "Não há projeções ou valores reais para desenhar.",
      call. = FALSE
    )
  }

  janela <- range(datas)

  if (diff(as.numeric(janela)) == 0) {
    janela <- janela + c(-15, 15)
  }

  presentes <- pt_modelos[
    pt_modelos %in% unique(projecoes$Modelo)
  ]

  # Regularizar apenas a visualizacao: lacunas continuam NA, sem interpolacao.
  datas <- sort(unique(c(datas, seq(min(datas), max(datas), by = "month"))))
  # Timestamp em UTC evita deslocar o primeiro dia do mes no navegador.
  dados_grafico <- data.frame(Data = as.numeric(datas) * 86400000)
  if (nrow(reais)) {
    dados_grafico$Real <- reais$Real[match(datas, reais$Data)]
  }
  for (modelo in presentes) {
    parte <- projecoes[projecoes$Modelo == modelo, , drop = FALSE]
    dados_grafico[[modelo]] <- parte$Previsao[match(datas, parte$Data)]
  }

  grafico <- echarts4r::e_charts(
    dados_grafico, Data, useUTC = TRUE,
    animationDuration = 300, animationDurationUpdate = 150
  )
  if (nrow(reais)) {
    grafico <- echarts4r::e_line_(
      grafico, "Real", name = "Real (referência)",
      smooth = FALSE, connectNulls = FALSE, showSymbol = FALSE,
      lineStyle = list(color = "#333333", width = 2),
      itemStyle = list(color = "#333333")
    )
  }
  for (modelo in presentes) {
    grafico <- echarts4r::e_line_(
      grafico, modelo, name = modelo,
      smooth = FALSE, connectNulls = FALSE, showSymbol = FALSE,
      lineStyle = list(color = pt_cores[[modelo]], width = 2),
      itemStyle = list(color = pt_cores[[modelo]])
    )
  }

  rotulos <- c(if (nrow(reais)) "Real (referência)", presentes)
  grafico <- grafico |>
    echarts4r::e_title(
      text = paste("Localidade:", localidade),
      subtext = paste(
        "Janela:", format(min(datas), "%m/%Y"), "a", format(max(datas), "%m/%Y")
      ),
      left = "center", top = 5, textStyle = list(fontSize = 14)
    ) |>
    echarts4r::e_grid(top = 78, bottom = 90, left = 85, right = 25, containLabel = TRUE) |>
    echarts4r::e_legend(
      type = "scroll", orient = "horizontal", bottom = 0, left = 10, right = 10,
      selectedMode = "multiple",
      selected = as.list(stats::setNames(rep(TRUE, length(rotulos)), rotulos)),
      inactiveColor = "#b8b8b8", textStyle = list(fontSize = 12)
    ) |>
    echarts4r::e_x_axis(
      type = "time", min = as.numeric(janela[1]) * 86400000,
      max = as.numeric(janela[2]) * 86400000,
      name = "Data", nameLocation = "middle", nameGap = 30,
      axisLabel = list(formatter = htmlwidgets::JS(
        "function(value) {
          var d = new Date(value);
          return String(d.getUTCMonth() + 1).padStart(2, '0') + '/' + d.getUTCFullYear();
        }"
      ))
    ) |>
    echarts4r::e_y_axis(
      name = "Quantidade de veículos", nameLocation = "middle", nameGap = 65,
      min = htmlwidgets::JS(
        "function(value) { return isFinite(value.min) ? Math.min(0, value.min) : 0; }"
      ),
      axisLabel = list(formatter = htmlwidgets::JS(
        "function(value) { return value.toLocaleString('pt-BR', {maximumFractionDigits: 0}); }"
      )),
      splitLine = list(show = TRUE, lineStyle = list(color = "#e5e7eb"))
    ) |>
    echarts4r::e_tooltip(
      trigger = "axis", axisPointer = list(type = "line"),
      formatter = htmlwidgets::JS(
        "function(params) {
          if (!params || !params.length) return '';
          var stamp = Array.isArray(params[0].value) ? params[0].value[0] : params[0].axisValue;
          var date = new Date(stamp).toLocaleDateString('pt-BR', {
            month: '2-digit', year: 'numeric', timeZone: 'UTC'
          });
          var result = '<b>' + date + '</b><br/>';
          params.forEach(function(item) {
            var value = Array.isArray(item.value) ? item.value[1] : item.value;
            if (value !== null && value !== undefined && value !== '' && isFinite(Number(value))) {
              result += item.marker + item.seriesName + ': <b>' +
                Number(value).toLocaleString('pt-BR', {maximumFractionDigits: 2}) +
                ' veículos</b><br/>';
            }
          });
          return result;
        }"
      )
    )

  # Anotacao independente da legenda: nao e um modelo e nao altera a escala Y.
  if (nrow(reais) || !is.null(data_corte)) {
    marcador <- if (is.null(data_corte)) max(reais$Data) else data_corte
    rotulo_marcador <- if (is.null(data_corte)) "Último Real" else "Fim do treino"
    grafico$x$opts$series <- append(grafico$x$opts$series, list(list(
      id = if (is.null(data_corte)) "pt_ultimo_real" else "pt_corte_treino",
      name = if (is.null(data_corte)) "Último mês observado" else rotulo_marcador,
      type = "line",
      data = list(), silent = TRUE, showSymbol = FALSE,
      tooltip = list(show = FALSE),
      markLine = list(
        silent = TRUE, symbol = c("none", "none"),
        label = list(formatter = rotulo_marcador, position = "insideEndTop"),
        lineStyle = list(color = "#6B7280", type = "dotted", width = 1),
        data = list(list(xAxis = as.numeric(marcador) * 86400000))
      )
    )))
  }

  grafico
}

# --- Comparação das perdas: somente LSTM e GRU -------------------------------

pt_historico_disponivel <- function(historico) {
  is.list(historico) &&
    is.null(historico$erro) &&
    is.data.frame(historico$dados) &&
    nrow(historico$dados) > 0L
}

pt_dados_perdas_comparacao <- function(
    historico_lstm, historico_gru, localidade, historico_tcn = NULL
) {
  historicos <- list(LSTM = historico_lstm, GRU = historico_gru, TCN = historico_tcn)
  partes <- list()
  colunas <- c(Treino = "Loss", Validacao = "Val_Loss")
  for (modelo in names(historicos)) {
    historico <- historicos[[modelo]]
    if (!pt_historico_disponivel(historico)) next
    escopo <- if (modelo == "GRU") "Global" else localidade
    dados <- historico$dados[historico$dados$Localidade %in% escopo, , drop = FALSE]
    if (!nrow(dados)) next
    for (tipo in names(colunas)) {
      partes[[length(partes) + 1L]] <- data.frame(
        Epoca = dados$Epoca, Serie = paste0(modelo, "_", tipo),
        Valor = dados[[colunas[[tipo]]]], stringsAsFactors = FALSE
      )
    }
  }
  if (!length(partes)) return(data.frame(
    Epoca = integer(), Serie = character(), Valor = numeric(), stringsAsFactors = FALSE
  ))
  dados <- do.call(rbind, partes)
  dados$Epoca <- as.integer(dados$Epoca)
  dados$Valor <- as.numeric(dados$Valor)
  dados <- dados[is.finite(dados$Epoca) & dados$Epoca >= 1, , drop = FALSE]
  dados[order(dados$Epoca, dados$Serie), , drop = FALSE]
}

pt_desenhar_perdas <- function(dados, localidade) {
  if (!nrow(dados) || !any(is.finite(dados$Valor))) {
    stop(
      "Não há perdas de treinamento válidas para desenhar.",
      call. = FALSE
    )
  }

  dados <- dados[order(dados$Epoca, dados$Serie), , drop = FALSE]
  valores <- dados$Valor[is.finite(dados$Valor)]
  limite_y <- max(valores)

  if (limite_y <= 0) limite_y <- 1

  eixo_x <- dados$Epoca[is.finite(dados$Epoca)]
  limite_x <- range(eixo_x)

  if (diff(limite_x) == 0) {
    limite_x <- limite_x + c(-0.5, 0.5)
  }

  marcas_x <- pretty(limite_x, n = 10)
  marcas_x <- sort(unique(c(
    min(eixo_x), marcas_x, max(eixo_x)
  )))

  marcas_x <- marcas_x[
    marcas_x >= min(eixo_x) &
      marcas_x <= max(eixo_x) &
      marcas_x == floor(marcas_x)
  ]

  parametros <- graphics::par(
    mar = c(4.5, 5.2, 5.4, 1.2),
    las = 1
  )
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

  redes_locais <- c(
    if (any(grepl("^LSTM_", dados$Serie))) "LSTM",
    if (any(grepl("^TCN_", dados$Serie))) "TCN"
  )
  if (length(redes_locais)) {
    escopos <- c(escopos, paste0(paste(redes_locais, collapse = "/"), ": ", localidade))
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

  series <- pt_series_perdas
  rotulos <- pt_rotulos_perdas
  cores <- stats::setNames(rep(unname(pt_cores[pt_redes_perdas]), each = 2L), series)
  tipos <- stats::setNames(rep(c(1, 2), length(pt_redes_perdas)), series)
  pontos <- stats::setNames(c(16, 1, 17, 2, 15, 0), series)

  for (serie in series) {
    serie_dados <- dados[dados$Serie == serie, , drop = FALSE]

    if (!nrow(serie_dados) ||
        !any(is.finite(serie_dados$Valor))) {
      next
    }

    grupos <- cumsum(c(
      TRUE,
      diff(serie_dados$Epoca) != 1L
    ))

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
    function(serie) {
      any(is.finite(
        dados$Valor[dados$Serie == serie]
      ))
    },
    logical(1)
  )]

  graphics::legend(
    x = mean(limite_x),
    y = graphics::par("usr")[4],
    xjust = 0.5,
    yjust = 0,
    xpd = NA,
    ncol = if (length(presentes) > 4L) 3L else 2L,
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

# --- Gradientes: somente LSTM ------------------------------------------------

pt_desenhar_gradientes <- function(dados, localidade) {
  dados <- dados[order(dados$Epoca), , drop = FALSE]

  valores <- c(
    dados$Norma_Gradiente,
    dados$Max_Gradiente
  )
  valores <- valores[is.finite(valores)]

  if (!nrow(dados) || !length(valores)) {
    stop(
      "Não há gradientes numéricos para desenhar.",
      call. = FALSE
    )
  }

  eixo_x <- dados$Epoca[is.finite(dados$Epoca)]
  limite_x <- range(eixo_x)

  if (diff(limite_x) == 0) {
    limite_x <- limite_x + c(-0.5, 0.5)
  }

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

  tipos <- c(
    Norma_Gradiente = 1,
    Max_Gradiente = 2
  )

  presentes <- character()

  for (serie in series) {
    ok <- is.finite(dados[[serie]])
    if (!any(ok)) next

    x <- dados$Epoca[ok]
    y <- dados[[serie]][ok]

    if (length(x) > 1L) {
      lines(
        x, y,
        col = cores[[serie]],
        lty = tipos[[serie]],
        lwd = 2
      )
    }

    points(
      x, y,
      col = cores[[serie]],
      pch = 16,
      cex = 0.45
    )

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
      side = 3,
      line = 0.8,
      col = "#B91C1C",
      cex = 0.78
    )
  }

  invisible(NULL)
}

# --- Interface ---------------------------------------------------------------

previsoes_todos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      bs4Dash::box(
        title = "Resultados dos modelos e treinamento LSTM × GRU × TCN",
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
        title = "Comparação das previsões por algoritmo",
        width = 12,
        status = "success",
        solidHeader = FALSE,
        maximizable = TRUE,

        shiny::tags$p(
          "Comparação de ARIMA, ETS, SARIMA, SARIMAX, LSTM, GRU, XGBoost, Prophet e TCN. ",
          "A linha real é mostrada apenas nos períodos em que há observação; ",
          "para ela, usa-se preferencialmente o valor real do CSV da GRU, ",
          "com complemento dos demais modelos, incluindo XGBoost, Prophet e TCN. ",
          "As datas e os valores apresentados são os registrados nos arquivos ",
          "CSV de previsão de cada modelo."
        ),

        shiny::uiOutput(ns("status_projecoes")),

        echarts4r::echarts4rOutput(
          ns("grafico_projecoes"),
          width = "100%",
          height = "520px"
        ),

        shiny::tags$p(
          "Clique nos nomes da legenda para ocultar ou mostrar as curvas. ",
          "A seleção da legenda afeta somente este gráfico."
        ),

        shiny::tags$p(
          "As métricas usam somente meses com valor real e previsão disponíveis. ",
          "Cada previsão é comparada ao valor real do próprio modelo; ",
          "projeções futuras sem observação não entram no cálculo. ",
          "No período comum, usam-se os mesmos meses entre os modelos com observações. ",
          "No período individual, cada modelo usa todos os seus meses disponíveis. ",
          "MAPE exclui valores reais iguais a zero; as outras métricas mantêm esses meses. ",
          "O R² pode ser negativo ",
          "e fica indefinido quando há menos de dois meses válidos ou ",
          "quando os valores reais não variam."
        ),

        shiny::tags$p(
          "ME = Real − Previsão: positivo indica subestimação; negativo, superestimação. ",
          "MASE e RMSSE usam uma escala sazonal de 12 meses, calculada somente com ",
          "valores reais de 2016–2024; valores abaixo de 1 indicam erros menores ",
          "que essa escala histórica. RelMAE compara o MAE do modelo com o do ",
          "SARIMAX nos mesmos meses e valores reais; abaixo de 1 indica menor erro. ",
          "Escalas sem dados ou iguais a zero e referências indisponíveis aparecem como N/A."
        ),

        shiny::selectInput(
          ns("periodo_metricas"),
          "PERÍODO PARA AS MÉTRICAS",
          choices = c(
            "Meses em comum entre modelos com observações" = "comum",
            "Meses disponíveis de cada modelo" = "individual"
          ),
          selected = "comum",
          width = "100%"
        ),

        shiny::uiOutput(ns("status_metricas")),
        DT::dataTableOutput(ns("tabela_erros_projecoes"))
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Validação temporal — comparação de modelos",
        width = 12,
        status = "info",
        solidHeader = FALSE,
        maximizable = TRUE,
        shiny::tags$p(
          "Cada rodada ajusta os modelos até dezembro do ano anterior e prevê ",
          "continuamente até dezembro de 2025, sem incorporar os valores reais do teste. ",
          "Escolha a janela e a localidade para comparar ",
          "os valores reais com as previsões dos arquivos disponíveis. ",
          "Clique na legenda para mostrar ou ocultar modelos no gráfico. ",
          "Os erros são calculados ao abrir esta seção."
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("ano_validacao"),
            "JANELA DE TESTE", choices = c("2021–2025 (60 meses)" = "2021",
              "2022–2025 (48 meses)" = "2022", "2023–2025 (36 meses)" = "2023",
              "2024–2025 (24 meses)" = "2024", "Últimos 12 meses (2025)" = "2025"), selected = "2024")),
          shiny::column(3, shiny::selectInput(ns("tipo_local_validacao"),
            "ABRANGÊNCIA", choices = c("Pará (Geral)" = "geral", "Município" = "municipio"),
            selected = "geral")),
          shiny::column(5, shiny::selectInput(ns("local_validacao"),
            "LOCALIDADE DA VALIDAÇÃO", choices = character()))
        ),
        shiny::selectInput(ns("periodo_metricas_validacao"),
          "PERÍODO DAS MÉTRICAS DA VALIDAÇÃO",
          choices = c("Meses comuns aos modelos com dados observados" = "comum",
            "Período disponível de cada modelo" = "individual"), selected = "comum"),
        shiny::tags$div(
          style = "display:flex;align-items:center;flex-wrap:wrap;gap:6px;margin:4px 0 8px;",
          shiny::downloadButton(ns("baixar_previsoes_validacao_csv"), "Previsões CSV",
            class = "btn-sm", style = "padding:3px 8px;font-size:12px;",
            title = "Baixar previsões de todas as janelas e modelos da localidade selecionada"),
          shiny::downloadButton(ns("baixar_metricas_validacao_csv"), "Métricas CSV",
            class = "btn-sm", style = "padding:3px 8px;font-size:12px;",
            title = "Baixar métricas totais e anuais de todas as janelas da localidade selecionada"),
          shiny::tags$small(class = "text-muted", "Todas as janelas da localidade selecionada.")
        ),
        shiny::uiOutput(ns("status_exportacao_validacao")),
        shiny::uiOutput(ns("status_validacao")),
        echarts4r::echarts4rOutput(ns("grafico_validacao"),
          width = "100%", height = "460px"),
        shiny::tags$p(
          "A linha tracejada marca o fim do treino. Somente os meses da janela de teste ",
          "entram nos erros. MASE e RMSSE usam a escala sazonal do treino desta ",
          "rodada. RelMAE compara cada modelo com o SARIMAX da mesma janela, nos mesmos ",
          "meses; fica indisponível quando não há referência válida. ",
          "A legenda filtra somente as curvas; as tabelas seguem o período escolhido acima."
        ),
        shiny::tags$h4("Erros do período completo"),
        DT::dataTableOutput(ns("metricas_validacao")),
        shiny::tags$h4("Erros por ano previsto"),
        shiny::tags$p(
          "As janelas têm durações diferentes. Use os blocos anuais para identificar ",
          "quando a previsão perde precisão e para comparar um mesmo ano observado ",
          "entre as origens, considerando também a distância de cada corte."
        ),
        DT::dataTableOutput(ns("anuais_validacao")),
        shiny::tags$details(
          shiny::tags$summary("Ver valores mensais e erros do teste"),
          DT::dataTableOutput(ns("mensal_validacao"))
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Perdas de treinamento e validação — LSTM × GRU × TCN",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        maximizable = TRUE,

        shiny::tags$p(
          "Valores registrados pelo Keras durante fit(): Loss no treino e ",
          "Val_Loss na validação interna, por época. A função de perda é MSE ",
          "no alvo normalizado de cada rede. LSTM e TCN correspondem à localidade selecionada; ",
          "a GRU corresponde ao modelo global. Os históricos são lidos de ",
          "historico_perdas_lstm.csv, historico_perdas_gru.csv e historico_perdas_tcn.csv, na pasta data. ",
          "Este gráfico não usa as previsões mensais."
        ),

        shiny::tags$p(
          "Eixo X: épocas. Eixo Y: perda MSE. Linhas contínuas: treino; ",
          "tracejadas: validação. Cada curva termina na última época gravada."
        ),

        shiny::uiOutput(ns("status_historico")),

        shiny::plotOutput(
          ns("grafico_perdas"),
          height = "520px"
        ),

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

        DT::dataTableOutput(ns("tabela_gradientes"))
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        title = "Perdas registradas por época — LSTM × GRU × TCN",
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

# --- Servidor ----------------------------------------------------------------

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

    caminho_historico_tcn <- normalizePath(
      file.path(data_dir, "historico_perdas_tcn.csv"),
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
        historico_tcn = pt_ler_historico(
          caminho_historico_tcn,
          modelo_esperado = "TCN"
        ),
        gradientes_lstm = pt_ler_gradientes_lstm(
          caminho_gradientes_lstm
        ),
        previsoes = pt_carregar_previsoes(previsoes_dir)
      )
    })

    shiny::observe({
      carregado <- arquivos()

      locais_historico <- unique(c(
        carregado$historico_lstm$dados$Localidade,
        carregado$historico_tcn$dados$Localidade
      ))

      locais <- sort(unique(c(
        locais_historico,
        carregado$previsoes$dados$Localidade
      )))

      locais <- locais[
        !is.na(locais) & nzchar(locais)
      ]

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

    # Preparação compartilhada pelo gráfico e seu status, inclusive se vazia.
    projecoes_locais <- shiny::reactive({
      pt_dados_projecoes_comparacao(arquivos()$previsoes$dados, input$select_local)
    })

    # Previsões dos modelos disponíveis para a localidade.
    projecoes_comparacao <- shiny::reactive({
      shiny::req(input$select_local)

      carregado <- arquivos()

      dados <- projecoes_locais()

      shiny::validate(shiny::need(
        nrow(dados) > 0L,
        paste(
          "Não existem previsões disponíveis para",
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
      shiny::req(input$select_local)

      carregado <- arquivos()

      dados <- projecoes_locais()

      modelos <- pt_modelos[
        pt_modelos %in% unique(dados$Modelo)
      ]

      faltantes <- setdiff(pt_modelos, modelos)

      reais <- pt_reais_projecao(
        carregado$previsoes$dados,
        input$select_local
      )

      divergencias <- sum(
        reais$Valores_distintos > 1L
      )

      shiny::tagList(
        shiny::tags$p(
          "Modelos exibidos: ",
          if (length(modelos)) {
            paste(modelos, collapse = ", ")
          } else {
            "nenhum"
          },
          ". Localidade: ",
          input$select_local,
          "."
        ),

        if (length(faltantes)) {
          shiny::tags$p(
            style = "color:#9a3412;",
            "Sem previsão para esta localidade: ",
            paste(faltantes, collapse = ", "),
            ". Os demais modelos continuam disponíveis."
          )
        },

        if (divergencias > 0L) {
          shiny::tags$p(
            style = "color:#9a3412;",
            "Os modelos apresentam valores reais diferentes em ",
            divergencias,
            " meses. A linha real prioriza a GRU, com complemento dos demais modelos. ",
            "As métricas usam o valor real de cada modelo; os meses em comum ",
            "não garantem que os históricos reais sejam iguais."
          )
        }
      )
    })

    output$grafico_projecoes <- echarts4r::renderEcharts4r({
      dados <- projecoes_comparacao()

      pt_desenhar_projecoes(
        projecoes = dados$projecoes,
        dados_completos = dados$completos,
        localidade = input$select_local
      )
    })

    metricas_projecoes <- shiny::reactive({
      shiny::req(input$periodo_metricas)

      dados <- projecoes_comparacao()

      pt_tabela_erros_projecoes(
        projecoes = dados$projecoes,
        dados_completos = dados$completos,
        localidade = input$select_local,
        periodo = input$periodo_metricas
      )
    })

    output$status_metricas <- shiny::renderUI({
      tabela <- metricas_projecoes()
      shiny::req(nrow(tabela) > 0L)
      escala <- attr(tabela, "escala_sazonal")
      fonte <- if (is.na(escala$fonte)) "indisponível" else escala$fonte
      escalas_invalidas <- c(
        if (!is.finite(escala$mae) || escala$mae <= 0) "MASE",
        if (!is.finite(escala$mse) || escala$mse <= 0) "RMSSE"
      )
      sem_referencia <- tabela[
        tabela$Periodos_validos > 0L & !is.finite(tabela$RelMAE),
        c("Modelo", "Motivo_RelMAE"), drop = FALSE
      ]

      shiny::tagList(
        shiny::tags$p(shiny::tags$small(
          "Escala sazonal 2016–2024: Real de ", fonte,
          "; ", escala$pares, " pares anuais conhecidos."
        )),
        if (length(escalas_invalidas)) {
          shiny::tags$p(shiny::tags$small(
            paste(escalas_invalidas, collapse = " e "),
            ": N/A por escala histórica sem pares válidos, nula ou não finita."
          ))
        },
        lapply(unique(sem_referencia$Motivo_RelMAE), function(motivo) {
          modelos <- sem_referencia$Modelo[
            sem_referencia$Motivo_RelMAE == motivo
          ]
          shiny::tags$p(shiny::tags$small(
            "RelMAE indisponível para ", paste(modelos, collapse = ", "),
            ": ", motivo
          ))
        })
      )
    })

    output$tabela_erros_projecoes <- DT::renderDataTable({
      tabela <- metricas_projecoes()

      shiny::validate(shiny::need(
        nrow(tabela) > 0L &&
          any(tabela$Periodos_validos > 0L),

        if (input$periodo_metricas == "comum") {
          "Não há meses observados em comum para avaliar. Consulte o período individual."
        } else {
          "Não há meses observados com previsão disponível para calcular as métricas."
        }
      ))

      pt_widget_erros(tabela)
    })

    # Perdas de treinamento: LSTM/TCN locais e GRU global.
    perdas_comparacao <- shiny::reactive({
      carregado <- arquivos()

      dados <- pt_dados_perdas_comparacao(
        historico_lstm = carregado$historico_lstm,
        historico_gru = carregado$historico_gru,
        localidade = input$select_local,
        historico_tcn = carregado$historico_tcn
      )

      shiny::validate(shiny::need(
        nrow(dados) > 0L,
        paste(
          "Não há histórico de treinamento válido para exibir.",
          "Verifique os históricos de perdas LSTM, GRU e TCN na pasta data."
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
      historicos <- list(LSTM = carregado$historico_lstm,
        GRU = carregado$historico_gru, TCN = carregado$historico_tcn)
      caminhos <- list(LSTM = caminho_historico_lstm,
        GRU = caminho_historico_gru, TCN = caminho_historico_tcn)
      aviso <- function(texto) shiny::tags$p(style = "color:#9a3412;", texto)
      mensagens <- lapply(names(historicos), function(modelo) {
        shiny::tags$p(shiny::tags$strong(paste0(modelo, " — arquivo: ")),
          shiny::tags$code(caminhos[[modelo]]))
      })
      for (modelo in names(historicos)) {
        erro <- historicos[[modelo]]$erro
        if (!is.null(erro)) mensagens <- c(mensagens,
          list(aviso(paste(paste0(modelo, ":"), erro))))
      }
      for (modelo in names(historicos)) {
        historico <- historicos[[modelo]]
        escopo <- if (modelo == "GRU") "Global" else input$select_local
        dados <- historico$dados[historico$dados$Localidade %in% escopo, , drop = FALSE]
        if (nrow(dados)) {
          rotulo <- if (modelo == "GRU") "GRU global: " else paste0(modelo, ": ")
          mensagens <- c(mensagens, list(shiny::tags$p(
            rotulo, nrow(dados), " épocas; ", sum(is.finite(dados$Loss)),
            " perdas de treino; ", sum(is.finite(dados$Val_Loss)),
            " perdas de validação."
          )))
        } else if (modelo != "GRU" && is.null(historico$erro)) {
          mensagens <- c(mensagens, list(aviso(paste0(modelo,
            ": sem histórico para esta localidade; os demais históricos disponíveis continuam exibidos."))))
        }
      }
      avisos <- unlist(lapply(names(historicos), function(modelo) {
        x <- historicos[[modelo]]$avisos
        if (length(x)) paste(paste0(modelo, ":"), x)
      }), use.names = FALSE)
      if (length(avisos)) mensagens <- c(mensagens, lapply(avisos, aviso))

      mensagens <- c(
        mensagens,
        list(shiny::tags$p(
          style = "color:#4b5563;",
          paste(
            "A LSTM usa resíduo aditivo padronizado;",
            "a GRU usa ruído multiplicativo padronizado;",
            "a TCN usa quantidade mensal em nível padronizado.",
            "Além dos alvos diferentes, as amostras e o número de",
            "atualizações por época diferem entre a rede local e a global.",
            "Este gráfico descreve o treinamento: menor perda entre",
            "modelos não demonstra maior precisão das previsões.",
            "A validação temporal da TCN é de um passo, anterior ao ano de teste;",
            "boa perda interna não garante estabilidade da previsão recursiva longa."
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
    alt = "Perdas de treinamento e validação de LSTM, GRU e TCN por época."
    )

    # Gradientes: somente LSTM.
    gradientes_selecionados <- shiny::reactive({
      resultado <- arquivos()$gradientes_lstm

      shiny::validate(shiny::need(
        is.null(resultado$erro),
        paste(
          "Histórico de gradientes indisponível:",
          resultado$erro
        )
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
        "Arquivo:",
        caminho_gradientes_lstm,
        "| Épocas registradas:",
        nrow(dados)
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
          order = list(list(0, "asc"))
        )
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

      ordem <- c("Epoca", pt_series_perdas)
      tabela <- tabela[, intersect(ordem, names(tabela)), drop = FALSE]
      rotulos <- c(Epoca = "Época", stats::setNames(
        sub(" - ", " — ", pt_rotulos_perdas, fixed = TRUE), names(pt_rotulos_perdas)))

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

    # Tabela geral de previsões.
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

      dados <- pt_filtrar_previsoes_localidade(
        dados,
        input$select_local
      )

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

    # Downloads das curvas de perda.
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

      local <- gsub(
        "[^A-Za-z0-9_-]+",
        "_",
        local
      )

      paste0(
        "perdas_lstm_gru_tcn_",
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

    # Validação temporal: lê os arquivos do controlador, sem ajustar modelos.
    arquivos_validacao <- shiny::reactive({
      input$recarregar
      ano <- if (length(input$ano_validacao)) input$ano_validacao else 2024L
      pt_carregar_validacao(previsoes_dir, ano, ano_fim_teste = 2025L)
    })

    tipo_validacao <- shiny::reactive({
      if (isTRUE(input$tipo_local_validacao == "municipio")) "municipio" else "geral"
    })

    localidades_validacao <- shiny::reactive({
      pt_localidades_validacao(arquivos_validacao()$dados, tipo_validacao())
    })

    # Resolver a seleção também no servidor evita usar a série anterior enquanto
    # o navegador aplica a atualização dos filtros de abrangência/janela.
    localidade_validacao <- shiny::reactive({
      locais <- localidades_validacao()
      atual <- input$local_validacao
      if (length(atual) == 1L && !is.na(atual) && atual %in% locais) atual else head(locais, 1L)
    })

    shiny::observe({
      locais <- localidades_validacao()
      selecionado <- shiny::isolate(localidade_validacao())
      rotulos <- ifelse(pt_chave_localidade(locais) == "PARA", "Pará (Geral)", locais)
      shiny::updateSelectInput(session, "local_validacao",
        choices = stats::setNames(locais, rotulos), selected = selecionado)
    })

    dados_validacao <- shiny::reactive({
      carregado <- arquivos_validacao()
      shiny::validate(shiny::need(nrow(carregado$dados) > 0L,
        "Sem resultados desta rodada. Execute o controlador local e recarregue os arquivos."))
      shiny::validate(shiny::need(length(localidades_validacao()) > 0L,
        if (tipo_validacao() == "municipio") "Não há resultados municipais nesta janela." else
          "Não há resultado de Pará (Geral) nesta janela."))
      localidade <- localidade_validacao()
      shiny::req(length(localidade) == 1L)
      periodo <- if (isTRUE(input$periodo_metricas_validacao == "individual")) "individual" else "comum"
      pt_avaliar_validacao(carregado, localidade, periodo = periodo)
    })

    # Downloads: caches separados da janela exibida, avaliados sob demanda.
    # Recarregar invalida os arquivos; localidade/politica invalidam somente
    # a avaliacao. Os dois exportadores compartilham o mesmo conjunto de rodadas.
    arquivos_exportacao_validacao <- shiny::reactive({
      input$recarregar
      stats::setNames(lapply(2021:2025, function(ano) {
        pt_carregar_validacao(previsoes_dir, ano, ano_fim_teste = 2025L)
      }), as.character(2021:2025))
    })

    rodadas_exportacao_validacao <- shiny::reactive({
      localidade <- localidade_validacao()
      shiny::req(length(localidade) == 1L)
      periodo <- if (isTRUE(input$periodo_metricas_validacao == "individual")) "individual" else "comum"
      pt_preparar_exportacao_validacao(arquivos_exportacao_validacao(), localidade, periodo)
    })

    ultimo_download_validacao <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$recarregar, localidade_validacao(), input$periodo_metricas_validacao), {
      ultimo_download_validacao(NULL)
    }, ignoreInit = TRUE)

    nome_download_validacao <- function(tipo) {
      localidade <- localidade_validacao()
      shiny::req(length(localidade) == 1L)
      nome <- tolower(gsub(" ", "_", pt_chave_localidade(localidade), fixed = TRUE))
      periodo <- if (isTRUE(input$periodo_metricas_validacao == "individual")) "individual" else "comum"
      paste0("validacao_", tipo, "_", nome, "_2021_2025_", periodo, ".csv")
    }

    concluir_download_validacao <- function(tabela, arquivo, preparado) {
      if (!nrow(tabela)) {
        ultimo_download_validacao(list(anos = character(), avisos = preparado$avisos))
        shiny::showNotification("Sem resultados disponíveis para exportar nesta localidade.", type = "warning")
        shiny::validate(shiny::need(FALSE, "Sem resultados disponíveis para exportar."))
      }
      ultimo_download_validacao(NULL)
      pt_gravar_csv_validacao(tabela, arquivo)
      ultimo_download_validacao(list(anos = names(preparado$rodadas), avisos = preparado$avisos))
    }

    output$baixar_previsoes_validacao_csv <- shiny::downloadHandler(
      filename = function() nome_download_validacao("previsoes"),
      contentType = "text/csv; charset=utf-8",
      content = function(arquivo) {
        preparado <- rodadas_exportacao_validacao()
        concluir_download_validacao(pt_exportar_previsoes_validacao(preparado$rodadas), arquivo, preparado)
      }
    )

    output$baixar_metricas_validacao_csv <- shiny::downloadHandler(
      filename = function() nome_download_validacao("metricas"),
      contentType = "text/csv; charset=utf-8",
      content = function(arquivo) {
        preparado <- rodadas_exportacao_validacao()
        concluir_download_validacao(pt_exportar_metricas_validacao(preparado$rodadas), arquivo, preparado)
      }
    )

    # Somente ocupa espaco depois de um download; arquivos/modelos ausentes
    # ficam em detalhes, sem impedir a exportacao das rodadas disponiveis.
    output$status_exportacao_validacao <- shiny::renderUI({
      estado <- ultimo_download_validacao()
      if (is.null(estado)) return(NULL)
      texto <- if (length(estado$anos)) paste0("Janelas incluídas no CSV: ",
        paste(estado$anos, collapse = ", "), ".") else "Nenhuma janela disponível para esta localidade."
      if (!length(estado$avisos)) return(shiny::tags$small(class = "text-muted", texto))
      shiny::tags$details(style = "font-size:12px;margin-bottom:6px;",
        shiny::tags$summary(texto, " Avisos de cobertura"),
        shiny::tags$ul(lapply(estado$avisos, shiny::tags$li)))
    })

    output$status_validacao <- shiny::renderUI({
      carregado <- arquivos_validacao()
      listar_avisos <- function(x) if (length(x)) shiny::tags$details(
        shiny::tags$summary("Avisos da rodada"), shiny::tags$ul(lapply(x, shiny::tags$li)))
      if (!nrow(carregado$dados)) return(shiny::tagList(
        shiny::tags$p("Ainda não há resultados válidos para ", carregado$ano_teste,
          "–", carregado$ano_fim_teste,
          ". Execute os controladores na máquina local e clique em Recarregar arquivos."),
        listar_avisos(carregado$avisos)))
      if (!length(localidades_validacao())) return(shiny::tagList(
        shiny::tags$p(if (tipo_validacao() == "municipio") {
          "Não há previsões municipais nos arquivos desta janela. Gere a validação dos municípios e recarregue os arquivos."
        } else "Não há resultado de Pará (Geral) nos arquivos desta janela."),
        listar_avisos(carregado$avisos)))
      dados <- dados_validacao()
      escala <- attr(dados$metricas, "escala_sazonal")
      localidade <- localidade_validacao()
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong("Localidade: "),
          if (tipo_validacao() == "geral") "Pará (Geral)" else localidade),
        shiny::tags$p(shiny::tags$strong(paste0("Teste: ",
          format(carregado$inicio_teste, "%m/%Y"), " a ",
          format(carregado$fim_teste, "%m/%Y"), ". ")),
          "Fim do treino: ", format(dados$fim_treino, "%m/%Y"), ". Modelos: ",
          paste(dados$cobertura$Modelo, collapse = ", "), "."),
        shiny::tags$ul(lapply(seq_len(nrow(dados$cobertura)), function(i) {
          linha <- dados$cobertura[i, ]
          shiny::tags$li(shiny::tags$strong(paste0(linha$Modelo, ": ")),
            linha$Meses_pareados, " de ", carregado$meses_teste,
            " meses com Real e Previsão disponíveis; ", linha$Meses_avaliados,
            " avaliados. Real: ", linha$Meses_Real, "; previsão: ", linha$Meses_Previsao,
            ". Histórico de treino no CSV: ",
            if (is.na(linha$Inicio_treino)) "não informado" else paste0(
              format(linha$Inicio_treino, "%m/%Y"), " a ", format(linha$Fim_treino, "%m/%Y")), ".")
        })),
        shiny::tags$p(shiny::tags$small(if (dados$periodo == "comum") paste0(
          "Métricas nos meses comuns: ", dados$meses_validos,
          ". Modelos sem pares observados não restringem a amostra dos demais.") else
            "Métricas no período disponível de cada modelo; as amostras podem ser diferentes.")),
        shiny::tags$p(shiny::tags$small(
          "Real e previsão pertencem ao CSV da rodada selecionada. ",
          "Este painel não utiliza as previsões de produção nem seus históricos de perdas.")),
        if (!is.null(escala)) shiny::tags$p(shiny::tags$small(
          "Escala sazonal do treino: ", format(escala$inicio, "%m/%Y"), " a ",
          format(escala$fim, "%m/%Y"), "; fonte: ",
          if (is.na(escala$fonte)) "indisponível" else escala$fonte,
          "; ", escala$pares, " pares anuais conhecidos. ",
          if (!is.finite(escala$mae) || escala$mae <= 0 ||
              !is.finite(escala$mse) || escala$mse <= 0) {
            "MASE/RMSSE indisponíveis quando a escala não tem pares válidos ou é nula."
          } else "ME = Real − Previsão: positivo indica subestimação.")),
        listar_avisos(c(carregado$avisos, dados$avisos))
      )
    })

    output$grafico_validacao <- echarts4r::renderEcharts4r({
      dados <- dados_validacao()
      shiny::validate(shiny::need(nrow(dados$projecoes) > 0L,
        "Não há previsões válidas para desenhar nesta rodada."))
      pt_desenhar_projecoes(dados$projecoes, dados$completos,
        localidade_validacao(), data_corte = dados$fim_treino)
    })

    output$metricas_validacao <- DT::renderDataTable({
      tabela <- dados_validacao()$metricas
      shiny::validate(shiny::need(nrow(tabela) > 0L && any(tabela$Periodos_validos > 0L),
        "Não há meses de teste com Real e Previsão para calcular os erros."))
      pt_widget_erros(tabela)
    })

    output$mensal_validacao <- DT::renderDataTable({
      tabela <- dados_validacao()$mensal
      tabela <- tabela[c("Modelo", "Data", "Horizonte", "Real", "Previsao", "Erro", "Erro_absoluto", "Usado_metricas")]
      tabela$Data <- format(tabela$Data, "%m/%Y")
      tabela$Usado_metricas <- ifelse(tabela$Usado_metricas, "Sim", "Não")
      names(tabela) <- c("Modelo", "Mês", "Horizonte (meses)", "Real", "Previsão",
        "Erro (Real − Previsão)", "Erro absoluto", "Usado nas métricas")
      DT::datatable(tabela, rownames = FALSE,
        options = list(pageLength = 12, lengthChange = FALSE, scrollX = TRUE,
          dom = "tip", ordering = FALSE,
          columnDefs = list(list(targets = "_all", defaultContent = "N/A")))) |>
        DT::formatRound(columns = c("Real", "Previsão", "Erro (Real − Previsão)", "Erro absoluto"), digits = 2)
    })

    output$anuais_validacao <- DT::renderDataTable({
      tabela <- dados_validacao()$metricas_anuais
      shiny::validate(shiny::need(nrow(tabela) > 0L,
        "Não há meses observados para calcular os erros anuais."))
      pt_widget_erros(tabela, ano = tabela$Ano)
    })

    invisible(list(
      perdas = perdas_comparacao,
      projecoes = projecoes_comparacao,
      arquivos = arquivos,
      validacao = dados_validacao
    ))
  })
}
