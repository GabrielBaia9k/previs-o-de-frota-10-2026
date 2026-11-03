# Execute na raiz do projeto: Rscript --vanilla tests/test_gru_csv.R
# Testa a entrada e preparacao da GRU sem carregar TensorFlow ou treinar a rede.
if (!file.exists("pre_compilar_GRU_global.r")) {
  stop("Execute este teste a partir da raiz do projeto.", call. = FALSE)
}
suppressPackageStartupMessages(library(dplyr))

expressoes <- parse("pre_compilar_GRU_global.r")
selecionadas <- c(
  "config", "config$n_features_base", "ler_historico_gru", "one_hot_meses",
  "montar_tensor_amostras", "agregar_localidade", "preparar_localidade"
)
ambiente <- new.env(parent = globalenv())
for (expr in expressoes) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-"))) {
    nome <- paste(deparse(expr[[2L]]), collapse = "")
    if (nome %in% selecionadas) eval(expr, envir = ambiente)
  }
}
faltantes <- setdiff(c(
  "config", "ler_historico_gru", "one_hot_meses", "montar_tensor_amostras",
  "agregar_localidade", "preparar_localidade"
), ls(ambiente))
if (length(faltantes)) {
  stop(
    "O teste GRU depende da migracao para data/veiculos_por_mes_municipio.csv. ",
    "Atualize pre_compilar_GRU_global.r antes de executar esta suite. Ausentes: ",
    paste(faltantes, collapse = ", "),
    call. = FALSE
  )
}

verificacoes <- 0L
verificar <- function(condicao, descricao) {
  if (!isTRUE(condicao)) stop("Falhou: ", descricao)
  verificacoes <<- verificacoes + 1L
}
esperar_erro <- function(expr, padrao) {
  erro <- tryCatch({ force(expr); NULL }, error = identity)
  verificar(
    inherits(erro, "error") && grepl(padrao, conditionMessage(erro)),
    paste("erro esperado:", padrao)
  )
}

diretorio <- tempfile("gru-csv-")
dir.create(diretorio)
executar_testes <- function() {
  on.exit(unlink(diretorio, recursive = TRUE), add = TRUE)
  ler_fixture <- function(dados, bom = FALSE) {
    caminho <- file.path(diretorio, "historico.csv")
    utils::write.csv(dados, caminho, row.names = FALSE, fileEncoding = "UTF-8")
    if (bom) {
      bytes <- readBin(caminho, "raw", n = file.info(caminho)$size)
      writeBin(c(as.raw(c(239, 187, 191)), bytes), caminho)
    }
    config_fixture <- ambiente$config
    config_fixture$arquivo_entrada <- caminho
    ambiente$ler_historico_gru(config_fixture)
  }

  fixture <- data.frame(
    LOCAL = c(
      " Belém ", " Belém ", " Belém ", "MARABA", "PARÁ", "Pará (Geral)",
      "PARA", "PARA (GERAL)", "BELÉM", "BELÉM"
    ),
    ANO = c(rep(2016, 8), 2015, 2026),
    MES = 1,
    TIPO_VEICULO = c("AUTO", "MOTO", "AUTO", rep("AUTO", 7)),
    QTD_VEICULO = c(1, 2, 1, 4, rep(100, 6)),
    stringsAsFactors = FALSE
  )
  dados <- ler_fixture(fixture, bom = TRUE)
  verificar(nrow(dados) == 4L, "corte temporal e exclusao de totais estaduais")
  verificar(identical(unique(dados$LOCAL), c("Belém", "MARABA")), "BOM, acentos e espacos")
  verificar(identical(names(dados), c("LOCAL", "ANO", "MES", "QTD_VEICULO")), "schema interno")
  verificar(is.integer(dados$ANO) && is.integer(dados$MES), "ano e mes inteiros")
  verificar(ambiente$agregar_localidade(dados, "Belém")$valor_mes == 4, "soma tipos e linhas repetidas")
  verificar(ambiente$agregar_localidade(dados, "PARÁ (Geral)")$valor_mes == 8, "total estadual sem duplicacao")
  verificar(!any(grepl("parquet", list.files(diretorio))), "leitura sem arquivo SARIMAX")

  config_ausente <- ambiente$config
  config_ausente$arquivo_entrada <- file.path(diretorio, "ausente.csv")
  esperar_erro(ambiente$ler_historico_gru(config_ausente), "Arquivo de entrada nao encontrado")
  for (coluna in c("LOCAL", "ANO", "MES", "QTD_VEICULO")) {
    esperar_erro(ler_fixture(fixture[, setdiff(names(fixture), coluna)]), "Colunas ausentes")
  }
  linha <- fixture[1L, ]
  casos_invalidos <- list(
    LOCAL = list("", " ", NA_character_),
    ANO = list(NA_real_, Inf, NaN, "texto", 0, 10000, 2016.5),
    MES = list(NA_real_, Inf, "texto", 0, 13, 1.5),
    QTD_VEICULO = list(NA_real_, Inf, NaN, "texto", -1)
  )
  for (coluna in names(casos_invalidos)) {
    for (valor in casos_invalidos[[coluna]]) {
      invalida <- linha
      invalida[[coluna]] <- valor
      esperar_erro(ler_fixture(invalida), "Valores invalidos.*linhas: 2")
    }
  }
  esperar_erro(ler_fixture(fixture[9:10, ]), "Nenhum dado municipal")
  esperar_erro(ler_fixture(fixture[5:8, ]), "Nenhum dado municipal")

  verificar(identical(
    ambiente$config$arquivo_entrada, "data/veiculos_por_mes_municipio.csv"
  ), "CSV configurado como entrada padrao")
  reais <- ambiente$ler_historico_gru(ambiente$config)
  verificar(nrow(reais) == 94920L, "linhas reais no periodo 2016-2025")
  verificar(length(unique(reais$LOCAL)) == 144L, "todos os 144 municipios")
  verificar(sum(reais$QTD_VEICULO) == 1433667, "total real no periodo")

  # Agregacao independente para conferir a soma mensal do CSV.
  bruto <- utils::read.csv(ambiente$config$arquivo_entrada, fileEncoding = "UTF-8-BOM")
  bruto <- bruto[bruto$ANO >= 2016 & bruto$ANO <= 2025, ]
  esperado <- stats::aggregate(QTD_VEICULO ~ ANO + MES, bruto, sum)
  estadual <- ambiente$agregar_localidade(reais, "PARÁ (Geral)")
  comparacao <- merge(estadual, esperado, by = c("ANO", "MES"))
  verificar(
    nrow(comparacao) == nrow(estadual) && all(comparacao$valor_mes == comparacao$QTD_VEICULO),
    "total estadual mensal igual a agregacao independente do CSV"
  )
  fim_2025 <- estadual[estadual$ANO == 2025 & estadual$MES %in% 10:12, ]
  verificar(identical(fim_2025$valor_mes, c(22469, 21324, 25002)), "totais outubro a dezembro de 2025")

  localidades <- c("PARÁ (Geral)", unique(reais$LOCAL))
  n_features <- ambiente$config$n_features_base + length(localidades)
  for (local in c("AFUA", "CHAVES")) {
    serie <- ambiente$agregar_localidade(reais, local)
    verificar(nrow(serie) == if (local == "AFUA") 7L else 19L, paste("meses de", local))
    verificar(sum(serie$valor_mes) == if (local == "AFUA") 7 else 24, paste("total de", local))
    verificar(is.null(suppressMessages(ambiente$preparar_localidade(
      reais, local, match(local, localidades), length(localidades), ambiente$config
    ))), paste("serie curta ignorada individualmente:", local))
  }
  sem_curtas <- reais[!reais$LOCAL %in% c("AFUA", "CHAVES"), ]
  verificar(
    sum(estadual$valor_mes) - sum(sem_curtas$QTD_VEICULO) == 31,
    "series curtas contribuem para o total estadual"
  )

  metadados <- lapply(c("PARÁ (Geral)", "BELEM"), function(local) {
    ambiente$preparar_localidade(
      reais, local, match(local, localidades), length(localidades), ambiente$config
    )
  })
  for (meta in metadados) {
    verificar(
      identical(dim(meta$X_train), c(84L, 24L, 158L)) &&
        identical(dim(meta$X_test), c(12L, 24L, 158L)),
      paste("dimensoes de treino e teste:", meta$local)
    )
    verificar(all(is.finite(meta$X_train)) && all(is.finite(meta$Y_train)), paste("tensores finitos:", meta$local))
    indice <- match(meta$local, localidades)
    verificar(all(meta$X_train[, , ambiente$config$n_features_base + indice] == 1), paste("identidade da localidade:", meta$local))
  }
  combinado <- ambiente$montar_tensor_amostras(
    lapply(metadados, `[[`, "X_train"), ambiente$config$lookback, n_features
  )
  verificar(identical(dim(combinado), c(168L, 24L, 158L)), "tensor global combinado")
  verificar(
    identical(combinado[1:84, , ], metadados[[1L]]$X_train) &&
      identical(combinado[85:168, , ], metadados[[2L]]$X_train),
    "combinacao preserva os eixos e valores de cada bloco"
  )
  cat(verificacoes, "verificacoes passaram; TensorFlow e treinamento nao foram executados.\n")
}
executar_testes()
