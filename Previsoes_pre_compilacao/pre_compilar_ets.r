# ==============================================================================
# pre_compilar_ets.r - Script Offline para Pré-Calcular Projeções (ETS Damped Trend)
# ==============================================================================

# 1. Carregar pacotes
library(dplyr)
library(forecast)

cat("Iniciando o processo de pré-compilação das previsões do modelo ETS (Suavização Exponencial com Tendência Amortecida)...\n")

# 2. Carregar a base de dados central
dados_basicos <- read.csv("data/veiculos_por_mes_municipio.csv")

# 3. Definir as localidades a processar 
municipios <- unique(dados_basicos$LOCAL) 
localidades <- c("PARÁ (Geral)", municipios)

# Lista para armazenar os resultados iterados
lista_resultados_ets <- list()

# 4. Parâmetros de Horizonte
n_teste <- 12
passos_futuro <- 72 # 6 Anos de Previsão
horizonte_total <- n_teste + passos_futuro

# 5. Laço (Loop) iterando por cada localidade 
for (local in localidades) {
  cat("\nProcessando modelo ETS para:", local, "...")
  
  # Filtro e agregação (Mesmo período base - >= 2015)
  if (local == "PARÁ (Geral)") {
    df_local <- dados_basicos %>%
      filter(ANO >= 2015, ANO <= 2025) %>%
      group_by(ANO, MES) %>%
      summarise(valor_mes = sum(QTD_VEICULO, na.rm = TRUE), .groups = "drop")
  } else {
    df_local <- dados_basicos %>%
      filter(LOCAL == local, ANO >= 2015, ANO <= 2025) %>%
      group_by(ANO, MES) %>%
      summarise(valor_mes = sum(QTD_VEICULO, na.rm = TRUE), .groups = "drop")
  }
  
  # Ignora municípios com dados insuficientes (modelo quebra se não houver dados o suficiente)
  if (nrow(df_local) < 24) {
    cat(" Ignorado (Dados insuficientes).")
    next
  }
  
  df_local <- df_local %>% 
    mutate(Data = as.Date(paste(ANO, MES, "01", sep = "-"))) %>%
    arrange(Data)
  
  # 6. Transformar em Série Temporal
  valores <- df_local$valor_mes
  ano_inicio <- min(df_local$ANO)
  mes_inicio <- df_local$MES[which(df_local$ANO == ano_inicio)[1]]
  
  serie_ts <- ts(valores, start = c(ano_inicio, mes_inicio), frequency = 12)
  
  n_treino <- length(serie_ts) - n_teste
  serie_treino <- head(serie_ts, n_treino)
  
  # Preparar eixo X temporal real vs futuro
  Datas_Hist <- df_local$Data
  Datas_Fut  <- seq(max(Datas_Hist), by = "month", length.out = passos_futuro + 1)[-1]
  Datas_Completas <- c(Datas_Hist, Datas_Fut)
  
  # 7. Modelo ETS com Damped = TRUE
  tryCatch({
    # Treina a Suavização Exponencial (com penalização de crescimento ao longo do tempo)
    modelo_ets <- ets(
      serie_treino, 
      damped = TRUE
    )
    
    # Previsão Extrapolando Damped
    previsoes_ets <- forecast(
      modelo_ets, 
      h = horizonte_total
    )
    
    previsoes_reais <- as.numeric(previsoes_ets$mean)
    
    # Criar DataFrame temporário combinando o Histórico e as Previsões Exponenciais
    df_combinado <- data.frame(
      Localidade = local,
      Data = Datas_Completas,
      Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
      Previsao_ETS = rep(NA_real_, length(Datas_Completas)), 
      Modelo = "ETS_Damped"
    )
    
    # Injeta a previsão no espaço temporal correto
    idx_start <- n_treino + 1
    idx_end <- n_treino + length(previsoes_reais)
    df_combinado$Previsao_ETS[idx_start:idx_end] <- previsoes_reais
    
    lista_resultados_ets[[local]] <- df_combinado
    cat(" Concluído com Sucesso.")
    
  }, error = function(e) {
    cat(" Erro de modelagem ETS:", e$message)
  })
}

# 8. Consolidação Final
cat("\n\nEmpilhando resultados de todos os municípios...\n")
df_previsoes_ets_pronto <- bind_rows(lista_resultados_ets)

# 9. Salva o Dataframe Final exportável em CSV
# Salvando na pasta "data" seguindo a estrutura do seu projeto original
caminho_saida <- file.path(
  "Previsoes_output", "previsoes_ets_pre_comp.csv"
)

# Cria o diretório (se não existir, embora saibamos que data/ existe)
dir.create(dirname(caminho_saida), showWarnings = FALSE, recursive = TRUE)

# Salva em formato CSV
write.csv(df_previsoes_ets_pronto, caminho_saida, row.names = FALSE)

cat("=====================================================\n")
cat("Processo Offline Concluído! Arquivo CSV gerado em:\n", caminho_saida, "\n")
cat("=====================================================\n")