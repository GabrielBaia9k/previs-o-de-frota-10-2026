# Dashboard_Modulo_Frota_Veiculos_2024
Criação de DashBoard sobre Módulo Frota Veicular Registradas no DETRAN-PA.
Essa versão é a oficial sem falhas e com alguns dados até agosto de 2026

# links corretos para versionamento:

repositório: https://github.com/GabrielBaia9k/previs-o-de-frota-10-2026

ambiente Codex: https://chatgpt.com/local/01a17d7c-7dfb-7083-a9eb-df0cdfdba013

# abas de previsão

Previsão de frota: modulo previsoes.R
Modelagem: modelagem.R
comparação dos algoritmos: previsoes_todos.R

# executar os algoritmos:

scripts <- c(
  "pre_compilar_arima.R",
  "pre_compilar_ets.r",
  "pre_compilar_sarima.r",
  "pre_compilar_sarimax.r",
  "pre_compilar_LSTM.r",
  "pre_compilar_GRU_global.r"
)

for (script in scripts) {
  message("\nExecutando: ", script)

  source(
    script,
    local = new.env(parent = globalenv()),
    encoding = "UTF-8"
  )
}