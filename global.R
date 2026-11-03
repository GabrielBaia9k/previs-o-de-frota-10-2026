#==============================================================================#
# DETRAN-PA | DASHBOARD DE FROTA VEICULAR
# ARQUIVO GLOBAL OTIMIZADO PARA PODMAN / KUBERNETES
#==============================================================================#

#------------------------------------------------------------------------------#
# 1. CONFIGURAÇÕES GLOBAIS DE AMBIENTE (KUBERNETES SAFE)
#------------------------------------------------------------------------------#
# Trava o fuso horário para o Brasil (Evita bugs de datas no Linux/Docker)
Sys.setenv(TZ = "America/Belem")

options(
  shiny.maxRequestSize = 100 * 1024^2, # Permite uploads de até 100MB
  dplyr.summarise.inform = FALSE,      # Silencia avisos do dplyr no log do container
  OutDec = ",",                       # Formatação global de decimais
  scipen = 999                        # Desliga notação científica (ex: 1e+06)
)

#------------------------------------------------------------------------------#
# 2. GERENCIAMENTO DE PACOTES OTIMIZADO
#------------------------------------------------------------------------------#
core_pkgs <- c(
  "here", "arrow", "shiny", "shinyWidgets", "shinycssloaders",
  "shinyjs", "bs4Dash", "tidyverse", "janitor", "reactable",
  "openxlsx", "readxl", "echarts4r", "leaflet", "leaflet.extras",
  "sf", "fontawesome", "DT", "htmltools", "scales", "forcats", "quarto"
)

# Carregamento rápido e silencioso
invisible(lapply(core_pkgs, library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

#------------------------------------------------------------------------------#
# 3. CARREGAMENTO DE DADOS (ARQUITETURA ARROW / LAZY EVALUATION)
#------------------------------------------------------------------------------#
# Bases do painel e históricos de treinamento permanecem em data/.
data_dir <- here::here("data")

# Os seis CSVs de previsão são lidos exclusivamente de Previsoes_output/.
previsoes_dir <- here::here("Previsoes_output")

# MAPA ESPACIAL (Lido e Simplificado para economizar RAM)
if (file.exists(file.path(data_dir, "geopa.rds"))) {
  geopa <- readRDS(file.path(data_dir, "geopa.rds"))
  if (inherits(geopa, "sf")) {
    geopa <- sf::st_simplify(geopa, preserveTopology = TRUE, dTolerance = 0.001)
  }
}

# ---> LAZY LOADING: open_dataset() NÃO TRAZ OS DADOS PARA A RAM! <---
# O R cria apenas pontes para os arquivos. Economia de Gigabytes de memória.

base_expandida                 <- arrow::open_dataset(file.path(data_dir, "base_expandida.parquet"))
db01_acumulado                 <- arrow::open_dataset(file.path(data_dir, "db01_acumulado.parquet"))
db01_ano                       <- arrow::open_dataset(file.path(data_dir, "db01_ano.parquet"))
db02_categoria_acumulado       <- arrow::open_dataset(file.path(data_dir, "db02_categoria_acumulado.parquet"))
db02_categoria_ano             <- arrow::open_dataset(file.path(data_dir, "db02_categoria_ano.parquet"))
db03_tipo_veiculo_acumulado    <- arrow::open_dataset(file.path(data_dir, "db03_tipo_veiculo_acumulado.parquet"))
db03_tipo_veiculo_ano          <- arrow::open_dataset(file.path(data_dir, "db03_tipo_veiculo_ano.parquet"))
db04_cores_acumulado           <- arrow::open_dataset(file.path(data_dir, "db04_cores_acumulado.parquet"))
db04_cores_ano                 <- arrow::open_dataset(file.path(data_dir, "db04_cores_ano.parquet"))
db05_combustivel_acumulado     <- arrow::open_dataset(file.path(data_dir, "db05_combustivel_acumulado.parquet"))
db05_combustivel_ano           <- arrow::open_dataset(file.path(data_dir, "db05_combustivel_ano.parquet"))
db06_especie_veiculo_acumulado <- arrow::open_dataset(file.path(data_dir, "db06_especie_veiculo_acumulado.parquet"))
db06_especie_veiculo_ano       <- arrow::open_dataset(file.path(data_dir, "db06_especie_veiculo_ano.parquet"))
db07_nacionalidade_acumulado   <- arrow::open_dataset(file.path(data_dir, "db07_nacionalidade_acumulado.parquet"))
db07_nacionalidade_ano         <- arrow::open_dataset(file.path(data_dir, "db07_nacionalidade_ano.parquet"))
db08_tipo_vs_categoria_acumulado <- arrow::open_dataset(file.path(data_dir, "db08_tipo_vs_categoria_acumulado.parquet"))
db09_tipo_vs_cor_pa_acumulado    <- arrow::open_dataset(file.path(data_dir, "db09_tipo_vs_cor_pa_acumulado.parquet"))
db10_tipo_vs_combustivel_pa_acumulado <- arrow::open_dataset(file.path(data_dir, "db10_tipo_vs_combustivel_pa_acumulado.parquet"))
db11_tipo_vs_especie_pa_acumulado <- arrow::open_dataset(file.path(data_dir, "db11_tipo_vs_especie_pa_acumulado.parquet"))
db12_tipo_vs_nacionalide_pa_acumulado <- arrow::open_dataset(file.path(data_dir, "db12_tipo_vs_nacionalide_pa_acumulado.parquet"))
frota_acumulado                <- arrow::open_dataset(file.path(data_dir, "frota_acumulado.parquet"))
frota_ano                      <- arrow::open_dataset(file.path(data_dir, "frota_ano.parquet"))
frota_geral                    <- arrow::open_dataset(file.path(data_dir, "frota_geral.parquet"))
frota_licenciamento            <- arrow::open_dataset(file.path(data_dir, "frota_licenciamento.parquet"))
frota2                         <- arrow::open_dataset(file.path(data_dir, "frota2.parquet"))

#df_original                    <- arrow::open_dataset(file.path(data_dir, "FROTA.parquet"))

#------------------------------------------------------------------------------#
# 3.1 CACHE DE LISTAS PARA A UI
#------------------------------------------------------------------------------#
# Executa UMA VEZ na inicialização do Pod, para não onerar as sessões dos usuários.
# Nota: O collect() entra imediatamente após o distinct() para isolar o trabalho em RAM.

lista_regioes <- frota_acumulado %>%
  select(ri) %>% distinct() %>% collect() %>% drop_na() %>% arrange(ri) %>% pull(ri)

lista_anos_acumulado <- frota_acumulado %>%
  select(ano) %>% distinct() %>% collect() %>% arrange(desc(ano)) %>% pull(ano)

lista_anos_ano <- db03_tipo_veiculo_ano %>%
  select(ano) %>% distinct() %>% collect() %>% arrange(desc(ano)) %>% pull(ano)

lista_tipo_total <- frota_acumulado %>%
  filter(tipo_veiculo == "Total de Veículos") %>%
  select(tipo_veiculo) %>% distinct() %>% collect() %>% pull(tipo_veiculo)

lista_tipo_outros <- db03_tipo_veiculo_acumulado %>%
  filter(tipo_veiculo != "Total de Veículos") %>%
  select(tipo_veiculo) %>% distinct() %>% collect() %>% drop_na() %>% arrange(tipo_veiculo) %>% pull(tipo_veiculo)

# Cria a lista combinada para o selectInput de Veículos (Com header de grupos)
lista_tipos_completa <- list("Total de Veículos" = lista_tipo_total, "Tipo" = lista_tipo_outros)

lista_municipios <- db01_acumulado %>%
  select(local) %>% distinct() %>% collect() %>% drop_na() %>% arrange(local) %>% pull(local)

# Guarda o ano máximo globalmente para os botões de "Limpar Filtro"
ano_maximo_global <- max(lista_anos_acumulado, na.rm = TRUE)

#------------------------------------------------------------------------------#
# 4. FUNÇÕES AUXILIARES
#------------------------------------------------------------------------------#

# Função helper para parágrafos estilizados (Aba Sobre, etc)
styled_paragraph <- function(text) {
  tags$p(
    style = "text-align: justify; font-size: 12pt;",
    strong(text)
  )
}

# Função de Formatação mapa Leaflet (Tratamento de decimais e pontos)
labelFormat_decimal <- function(prefix = "", suffix = "", between = " &ndash; ", digits = 3,
                                big.mark = ".", transform = identity, decimal.mark = ",") {
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark, decimal.mark = decimal.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}

#------------------------------------------------------------------------------#
# 5. CARREGAMENTO DOS MÓDULOS
#------------------------------------------------------------------------------#
# Array organizado com os módulos sustentadores do dashboard
# ATENÇÃO: Verifique se os nomes dos arquivos batem com os que você salvou na pasta!
modulos <- c(
  "modulos/01_geral.R",
  "modulos/02_1_carac_PA_RI.R",
  "modulos/02_2_carac_muni.R",
  "modulos/02_3_carac_tipo.R",
  "modulos/03_mod_downset.R",
  "modulos/frota_muni.R",
  "modulos/frota_pa.R",
  "modulos/previsoes.R",
  "modulos/previsoes_todos.R",
  "modulos/modelagem.R",
  "modulos/mod_pdf.R"
)

# local = FALSE garante que as funções fiquem acessíveis de forma global para o app.R
invisible(lapply(modulos, function(x) source(here::here(x), local = FALSE, encoding = "UTF-8")))

#------------------------------------------------------------------------------#
# 5.1 PREVISÕES PADRONIZADAS COMPARTILHADAS ENTRE AS TRÊS ABAS
#------------------------------------------------------------------------------#
# O leitor existente é definido em modulos/previsoes_todos.R, carregado acima.
# Executa uma vez na inicialização do app; reinicie o app após atualizar os CSVs.
# Os módulos podem acessar:
#   previsoes_compartilhadas$dados  -> Modelo, Localidade, Data, Real, Previsao,
#                                     AIC e BIC (NA quando indisponíveis).
#   previsoes_compartilhadas$avisos -> problemas encontrados na leitura.
previsoes_compartilhadas <- pt_carregar_previsoes(data_dir = previsoes_dir)

#------------------------------------------------------------------------------#
# 6. LIMPEZA FINAL DE MEMÓRIA (Boa prática K8s)
#------------------------------------------------------------------------------#
gc()