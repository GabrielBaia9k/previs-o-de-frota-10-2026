# VALIDACAO-TEMPORAL-6-LSTM-CAUSAL
# Configuracao unica para os controladores de validacao; somente R base.
# source() define configuracao/funcoes: nao le dados, treina ou preve.

# -----------------------------------------------------------------------------
# 1. CONFIGURACAO CENTRAL: editar este bloco e executar o controlador desejado.
# Cada origem preve de janeiro de anos_teste ate dezembro de ano_fim_teste.
# Excecoes por modelo ficam em modelos; hiperparametros ficam nos produtores.
# -----------------------------------------------------------------------------
config_validacao_temporal <- list(
  entrada = "data/veiculos_por_mes_municipio.csv",
  script_recortes = "previsoes_validacao_temporal/config_validacao_temporal.r",
  inicio_treino = as.Date("2016-01-01"),
  anos_teste = c(2021L, 2022L, 2023L, 2024L, 2025L),
  ano_fim_teste = 2025L,
  localidades = NULL, # Todos os municipios e PARA (Geral); ou vetor de localidades.
  paralelo = TRUE, processos = 6L, # PSOCK local; janelas continuam sequenciais.
  reutilizar_2025 = TRUE, # Reaproveitar o teste anual do CSV de producao conferido.
  pasta_saida_base = "Previsoes_output/validacao_temporal",
  modelos = list(
    SARIMAX = list(
      # SARIMAX A: calendario das janelas, com treino desde 2015 e teste ate 2025.
      # Excecao historica: preservar o inicio do SARIMAX publicado.
      inicio_treino = as.Date("2015-01-01"),
      anos_teste = c(2021L, 2022L, 2023L, 2024L, 2025L),
      # SARIMAX B: localidades e paralelismo (janelas continuam sequenciais).
      localidades = NULL, # Todos os municipios e PARA (Geral).
      paralelo = TRUE,
      processos = 6L, # Seis processos R de calculo; Windows/PSOCK.
      # SARIMAX C: produtor e referencia opcional para o teste anual de 12 meses.
      script_pre_compilacao = "Previsoes_pre_compilacao/pre_compilar_sarimax.r",
      reutilizar_2025 = TRUE,
      referencia_2025 = "Previsoes_output/previsoes_sarimax_pre_comp.csv"
    ),
    XGBOOST = list(
      script_pre_compilacao = "Previsoes_pre_compilacao/pre_compilar_xgboost.r",
      referencia_2025 = "Previsoes_output/previsoes_xgboost_pre_comp.csv"
    ),
    PROPHET = list(
      script_pre_compilacao = "Previsoes_pre_compilacao/pre_compilar_prophet.r",
      referencia_2025 = "Previsoes_output/previsoes_prophet_pre_comp.csv"
    ),
    LSTM = list(
      script_pre_compilacao = "Previsoes_pre_compilacao/pre_compilar_LSTM.r",
      # Recalcular inclusive 2025: o CSV antigo pode usar preparacao com
      # vazamento. Igualdade de Real nao comprova a origem causal da previsao.
      reutilizar_2025 = FALSE,
      referencia_2025 = "Previsoes_output/previsoes_lstm_pre_comp.csv"
    )
  )
)

# -----------------------------------------------------------------------------
# 2. FUNCOES COMUNS: configuracao por modelo, janelas, arquivos e nomes de saida.
# -----------------------------------------------------------------------------
configurar_validacao_temporal <- function(
    modelo, ajustes = list(), configuracao = config_validacao_temporal
) {
  modelo <- toupper(trimws(as.character(modelo)))
  if (length(modelo) != 1L || is.na(modelo) ||
      !modelo %in% names(configuracao$modelos)) stop("Modelo nao cadastrado na configuracao.")
  if (!is.list(ajustes) || !is.list(configuracao$modelos[[modelo]])) {
    stop("Configuracoes e ajustes devem ser listas.")
  }
  comum <- configuracao[setdiff(names(configuracao), "modelos")]
  config <- utils::modifyList(comum, configuracao$modelos[[modelo]], keep.null = TRUE)
  config <- utils::modifyList(config, ajustes, keep.null = TRUE)
  config$modelo <- modelo
  # [[ faz busca exata: $pasta_saida tambem encontra pasta_saida_base e
  # impediria a criacao do campo de destino quando so a base esta definida.
  if (is.null(config[["pasta_saida"]])) {
    config$pasta_saida <- file.path(config[["pasta_saida_base"]], modelo)
  }
  config
}

validar_anos_validacao <- function(anos, fim) {
  if (!is.numeric(anos) || !length(anos) || any(!is.finite(anos) |
      anos != floor(anos) | anos < 1900 | anos > 9998)) stop("anos_teste invalidos.")
  if (!is.numeric(fim) || length(fim) != 1L || !is.finite(fim) ||
      fim != floor(fim) || fim < max(anos) || fim > 9998) {
    stop("Informe ano_fim_teste inteiro, maior ou igual aos anos de inicio do teste.")
  }
  list(anos = sort(unique(as.integer(anos))), fim = as.integer(fim))
}

definir_rodadas_validacao <- function(config = config_validacao_temporal) {
  limites <- validar_anos_validacao(config$anos_teste, config$ano_fim_teste)
  rodadas <- lapply(limites$anos, function(ano) {
    definir_periodos_validacao(list(inicio_treino = config$inicio_treino,
      fim_treino = as.Date(sprintf("%04d-01-01", ano)) - 1L,
      meses_teste = as.integer(12L * (limites$fim - ano + 1L))))
  })
  names(rodadas) <- as.character(limites$anos)
  rodadas
}

validar_arquivos_validacao <- function(config) {
  for (campo in c("entrada", "script_pre_compilacao", "script_recortes", "pasta_saida")) {
    valor <- config[[campo]]
    if (!is.character(valor) || length(valor) != 1L || is.na(valor) || !nzchar(valor)) {
      stop("Caminho invalido em ", campo, ".")
    }
    if (campo != "pasta_saida" && !file.exists(valor)) stop("Arquivo necessario: ", valor)
  }
  invisible(config)
}

nome_arquivo_validacao <- function(modelo, ano_teste, ano_fim_teste) {
  modelo <- toupper(trimws(as.character(modelo)))
  if (length(modelo) != 1L || is.na(modelo) || !grepl("^[A-Z][A-Z0-9_]*$", modelo)) {
    stop("Identificador de modelo invalido.")
  }
  if (length(ano_teste) != 1L) stop("Informe um ano de inicio por arquivo.")
  limites <- validar_anos_validacao(ano_teste, ano_fim_teste)
  sufixo <- if (limites$anos == limites$fim) sprintf("%04d", limites$anos) else
    sprintf("%04d_%04d", limites$anos, limites$fim)
  paste0("previsoes_", tolower(modelo), "_teste_", sufixo, ".csv")
}

definir_periodos_validacao <- function(config = config_validacao_temporal) {
  # Sem limites de um recorte especifico, usar a primeira origem configurada.
  # Listas explicitas com inicio_treino/fim_treino/meses_teste mantem a API antiga.
  if (is.null(config$fim_treino) && is.null(config$meses_teste)) {
    return(definir_rodadas_validacao(config)[[1L]])
  }
  inicio <- config$inicio_treino
  fim <- config$fim_treino
  meses <- config$meses_teste
  if (length(inicio) != 1L || !inherits(inicio, "Date") || is.na(inicio) ||
      length(fim) != 1L || !inherits(fim, "Date") || is.na(fim) || inicio > fim) {
    stop("Informe inicio_treino e fim_treino como datas validas, em ordem cronologica.")
  }
  if (length(meses) != 1L || !is.numeric(meses) || !is.finite(meses) ||
      meses < 1 || meses != floor(meses)) {
    stop("meses_teste deve ser um inteiro positivo.")
  }

  primeiro_mes_final <- as.Date(format(fim, "%Y-%m-01"))
  proximo_mes <- seq(primeiro_mes_final, by = "month", length.out = 2L)[2L]
  if (format(inicio, "%d") != "01" || fim != proximo_mes - 1L) {
    stop("Use o primeiro dia do mes no inicio e o ultimo dia do mes no fim do treino.")
  }
  meses <- as.integer(meses)
  limites_teste <- seq(proximo_mes, by = "month", length.out = meses + 1L)

  list(
    inicio_treino = inicio,
    fim_treino = fim,
    meses_treino = length(seq(inicio, primeiro_mes_final, by = "month")),
    inicio_teste = limites_teste[1L],
    fim_teste = limites_teste[meses + 1L] - 1L,
    meses_teste = meses,
    datas_teste = limites_teste[seq_len(meses)]
  )
}

# -----------------------------------------------------------------------------
# 3. SEPARACAO: receber dados ja lidos, com Data ou ANO/MES do CSV de entrada.
# Apenas recorta as linhas; quantidades zero/NA e categorias sao preservadas.
# Agregacao, regularizacao mensal e preparacao dos modelos entram depois.
# -----------------------------------------------------------------------------
separar_treino_teste <- function(dados, config = config_validacao_temporal) {
  if (!is.data.frame(dados)) stop("dados deve ser um data.frame.")
  periodos <- definir_periodos_validacao(config)

  if ("Data" %in% names(dados)) {
    dados$Data <- as.Date(dados$Data)
  } else if (all(c("ANO", "MES") %in% names(dados))) {
    ano <- suppressWarnings(as.numeric(as.character(dados$ANO)))
    mes <- suppressWarnings(as.numeric(as.character(dados$MES)))
    if (any(!is.finite(ano) | ano != floor(ano) | ano < 1 | ano > 9999 |
            !is.finite(mes) | mes != floor(mes) | mes < 1 | mes > 12)) {
      stop("ANO/MES contem valores invalidos; confira as datas antes do recorte.")
    }
    dados$Data <- as.Date(sprintf("%04d-%02d-01", as.integer(ano), as.integer(mes)))
  } else {
    stop("Os dados precisam de Data ou das colunas ANO e MES.")
  }
  if (anyNA(dados$Data)) stop("Data contem datas ausentes ou invalidas.")

  dados <- dados[order(dados$Data), , drop = FALSE]
  treino <- dados[dados$Data >= periodos$inicio_treino &
    dados$Data <= periodos$fim_treino, , drop = FALSE]
  teste <- dados[dados$Data >= periodos$inicio_teste &
    dados$Data <= periodos$fim_teste, , drop = FALSE]

  # meses_treino/meses_teste sao duracoes nominais do calendario. Nao contam
  # observacoes conhecidas: dados podem conter categorias, municipios e lacunas.
  list(treino = treino, teste = teste, periodos = periodos)
}

# Exemplo, depois de ler os dados:
# config <- configurar_validacao_temporal("XGBOOST")
# rodadas <- definir_rodadas_validacao(config)
# recorte <- separar_treino_teste(dados, rodadas[["2021"]])
# recorte$treino  # 01/01/2016 a 31/12/2020.
# recorte$teste   # 01/01/2021 a 31/12/2025.
# Para um teste de 12 meses explicito, continua valido:
# definir_periodos_validacao(list(inicio_treino=as.Date("2016-01-01"),
#   fim_treino=as.Date("2020-12-31"), meses_teste=12L))
