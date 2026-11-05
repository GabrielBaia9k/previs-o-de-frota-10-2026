# ==============================================================================
# pre_compilar_sarima.r - Script Offline para Pré-Calcular Projeções (SARIMA)
# ==============================================================================

# 1. Carregar pacotes
library(dplyr)
library(forecast)

cat("Iniciando o processo de pré-compilação das previsões do modelo SARIMA...\n")

# 2. Carregar a base de dados central
dados_basicos <- read.csv("data/veiculos_por_mes_municipio.csv")

# 3. Definir as localidades a processar 
municipios <- unique(dados_basicos$LOCAL) 
localidades <- c("PARÁ (Geral)", municipios)

# Lista para armazenar os resultados iterados
lista_resultados_sarima <- list()

# 4. Parâmetros de Horizonte
n_teste <- 12
passos_futuro <- 72 # 6 Anos de Previsão
horizonte_total <- n_teste + passos_futuro

# 5. Laço (Loop) iterando por cada localidade 
for (local in localidades) {
  cat("\nProcessando modelo SARIMA para:", local, "...")
  
  # Filtro e agregação condicional (SARIMA usa dados a partir de 2015 segundo o previsoes.R)
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
  
  # Se o município não tem dados suficientes (mínimo de 2 anos para sazonalidade)
  if (nrow(df_local) < 24) {
    cat(" Ignorado (Dados insuficientes).")
    next
  }
  
  df_local <- df_local %>% 
    mutate(Data = as.Date(paste(ANO, MES, "01", sep = "-"))) %>%
    arrange(Data)
  
  # 6. Transformar em Série Temporal Sazonal Mensal
  valores <- df_local$valor_mes
  ano_inicio <- min(df_local$ANO)
  mes_inicio <- df_local$MES[which(df_local$ANO == ano_inicio)[1]]
  
  serie_ts <- ts(valores, start = c(ano_inicio, mes_inicio), frequency = 12)
  
  # Cortando o grupo de treino para reproduzir exatamente o modelo
  n_treino <- length(serie_ts) - n_teste
  serie_treino <- head(serie_ts, n_treino)
  
  # 7. Modelo SARIMA Idêntico com Fallback
  tryCatch({
    # TENTATIVA 1: O modelo rigoroso com lambda = 0 e busca completa
    modelo_sarima <- auto.arima(
      serie_treino, 
      stepwise = FALSE, 
      approximation = FALSE, 
      seasonal = TRUE,   # <-- Sazonalidade Ativada
      lambda = 0,        # <-- Box-Cox de Log (estabiliza os ziguezagues)
      D = 1              # <-- Diferenciação na componente Anual
    )
    
    # Prever e capturar os dados numéricos
    previsoes_sarima <- forecast(modelo_sarima, h = horizonte_total)
    previsoes_reais <- as.numeric(previsoes_sarima$mean)
    
    # 8. Montar Calendário Completo
    Datas_Hist <- df_local$Data
    Datas_Fut  <- seq(max(Datas_Hist), by = "month", length.out = passos_futuro + 1)[-1]
    Datas_Completas <- c(Datas_Hist, Datas_Fut)
    
    # Criar DataFrame temporário combinando o Histórico e as Previsões

    aic_val <- AIC(modelo_sarima)
    bic_val <- BIC(modelo_sarima)

    df_combinado <- data.frame(
      Localidade = local,
      Data = Datas_Completas,
      Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
      Previsao_SARIMA = rep(NA_real_, length(Datas_Completas)),
      AIC = aic_val,
      BIC = bic_val,
      Modelo = "SARIMA"
    )
    
    # Injeta a previsão no espaço temporal correto (Teste + Futuro)
    idx_start <- n_treino + 1
    idx_end <- n_treino + length(previsoes_reais)
    df_combinado$Previsao_SARIMA[idx_start:idx_end] <- previsoes_reais
    
    lista_resultados_sarima[[local]] <- df_combinado
    cat(" Concluído com Sucesso.")
    
  }, error = function(e) {
    cat(" Erro com lambda=0 ou busca rigorosa, usando Plano B...")
    
    # PLANO B (Fallback): Roda o auto.arima sem o log e com busca simplificada para municípios esparsos
    tryCatch({
      modelo_sarima_b <- auto.arima(
        serie_treino, 
        stepwise = TRUE,       # Busca mais rápida (evita timeouts)
        approximation = TRUE,  # Permite aproximações estatísticas
        seasonal = TRUE,       # Mantém a sazonalidade garantida
        D = 1                  
      )
      
      previsoes_sarima <- forecast(modelo_sarima_b, h = horizonte_total)
      previsoes_reais <- as.numeric(previsoes_sarima$mean)
      
      # Montar Calendário Completo (Plano B)
      Datas_Hist <- df_local$Data
      Datas_Fut  <- seq(max(Datas_Hist), by = "month", length.out = passos_futuro + 1)[-1]
      Datas_Completas <- c(Datas_Hist, Datas_Fut)
      
      aic_val <- AIC(modelo_sarima_b)
      bic_val <- BIC(modelo_sarima_b)

      df_combinado <- data.frame(
        Localidade = local,
        Data = Datas_Completas,
        Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
        Previsao_SARIMA = rep(NA_real_, length(Datas_Completas)),
        AIC = aic_val,
        BIC = bic_val,
        Modelo = "SARIMA"
      )
      
      idx_start <- n_treino + 1
      idx_end <- n_treino + length(previsoes_reais)
      df_combinado$Previsao_SARIMA[idx_start:idx_end] <- previsoes_reais
      
      lista_resultados_sarima[[local]] <- df_combinado
      cat(" Concluído via auto.arima (simplificado).")
      
    }, error = function(e2) {
      cat(" Falha total na modelagem matemática:", e2$message)
    })
  })
}

# 9. Consolidação Final
cat("\n\nEmpilhando resultados de todos os municípios...\n")
df_previsoes_sarima_pronto <- bind_rows(lista_resultados_sarima)

# 10. Salva o Dataframe Final exportável em CSV
caminho_saida <- file.path(
  "Previsoes_output", "previsoes_sarima_pre_comp.csv"
)

# Cria o diretório se ele não existir
dir.create(dirname(caminho_saida), showWarnings = FALSE, recursive = TRUE)

# Salva em formato CSV
write.csv(df_previsoes_sarima_pronto, caminho_saida, row.names = FALSE)

cat("=====================================================\n")
cat("Processo Offline Concluído! Arquivo CSV gerado em:\n", caminho_saida, "\n")
cat("=====================================================\n")