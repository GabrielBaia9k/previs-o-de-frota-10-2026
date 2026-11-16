# VALIDACAO-TEMPORAL-1-CORTES
# Etapas 1 e 2: definir os periodos e separar treinamento/teste em memoria.
# Caminhos relativos a raiz do projeto; usa somente R base.
# source("previsoes_validacao_temporal/config_validacao_temporal.r", encoding = "UTF-8")

# -----------------------------------------------------------------------------
# 1. CONFIGURACAO: alterar somente estas tres variaveis para escolher a rodada.
# O treinamento inclui as duas datas. O teste comeca no mes seguinte.
# -----------------------------------------------------------------------------
config_validacao_temporal <- list(
  inicio_treino = as.Date("2016-01-01"),
  fim_treino = as.Date("2020-12-31"),
  meses_teste = 12L
)

definir_periodos_validacao <- function(config = config_validacao_temporal) {
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
# 2. SEPARACAO: receber dados ja lidos, com Data ou ANO/MES do CSV de entrada.
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

# Exemplo para usar na proxima etapa, depois de ler os dados:
# periodos <- definir_periodos_validacao()
# recorte <- separar_treino_teste(dados)
# recorte$treino  # 01/01/2016 a 31/12/2020.
# recorte$teste   # 01/01/2021 a 31/12/2021.
# Este arquivo define configuracao/funcoes; source() nao le CSV nem treina modelos.
