# ==============================================================================
# pre_compilar_arima.R - Script Offline para Pré-Calcular Projeções (ARIMA Puro)
# ==============================================================================

# 1. Carregar pacotes no mesmo padrão do playprevisoes.R
library(dplyr)
library(forecast)

cat("Iniciando o processo de pré-compilação das previsões do modelo ARIMA (Sem Sazonalidade)...\n")

# 2. Carregar a base de dados central
dados_basicos <- read.csv("data/veiculos_por_mes_municipio.csv")

# 3. Definir as localidades a processar 
# Criamos "PARÁ (Geral)" e pegamos a lista de todos os municípios únicos
municipios <- unique(dados_basicos$LOCAL) 
localidades <- c("PARÁ (Geral)", municipios)

# Lista para armazenar os resultados iterados
lista_resultados_arima <- list()

# 4. Parâmetros de Horizonte
n_teste <- 12
passos_futuro <- 72 # 6 Anos de Previsão
horizonte_total <- n_teste + passos_futuro

# 5. Laço (Loop) iterando por cada localidade 
for (local in localidades) {
  cat("\nProcessando modelo ARIMA para:", local, "...")
  
  # Filtro e agregação condicional
  if (local == "PARÁ (Geral)") {
    df_local <- dados_basicos %>%
      filter(ANO >= 2016, ANO <= 2025) %>%
      group_by(ANO, MES) %>%
      summarise(valor_mes = sum(QTD_VEICULO, na.rm = TRUE), .groups = "drop")
  } else {
    df_local <- dados_basicos %>%
      filter(LOCAL == local, ANO >= 2016, ANO <= 2025) %>% # Alterado de MUNICIPIO para LOCAL
      group_by(ANO, MES) %>%
      summarise(valor_mes = sum(QTD_VEICULO, na.rm = TRUE), .groups = "drop")
  }
  
  # Se o município não tem dados suficientes, ele pula
  if (nrow(df_local) < 24) {
    cat(" Ignorado (Dados insuficientes).")
    next
  }
  
  df_local <- df_local %>% 
    mutate(Data = as.Date(paste(ANO, MES, "01", sep = "-"))) %>%
    arrange(Data)
  
  # 6. Transformar em Série Temporal (mesma configuração do previsoes.R)
  valores <- df_local$valor_mes
  ano_inicio <- min(df_local$ANO)
  mes_inicio <- df_local$MES[which(df_local$ANO == ano_inicio)[1]]
  
  serie_ts <- ts(valores, start = c(ano_inicio, mes_inicio), frequency = 12)
  
  # Cortando o grupo de treino para reproduzir exatamente o modelo
  n_treino <- length(serie_ts) - n_teste
  serie_treino <- head(serie_ts, n_treino)
  
  # 7. Modelo ARIMA Idêntico com Fallback
  tryCatch({
    # TENTATIVA 1: O modelo rigoroso com lambda="auto"
    modelo_arima <- Arima(
      serie_treino,
      order = c(24, 1, 1),        
      include.drift = TRUE,      
      lambda = "auto"            
    )
    
    # Prever e capturar os dados numéricos
    previsoes_arima <- forecast(modelo_arima, h = horizonte_total)
    previsoes_reais <- as.numeric(previsoes_arima$mean)
    
    # 8. Montar Calendário Completo
    Datas_Hist <- df_local$Data
    Datas_Fut  <- seq(max(Datas_Hist), by = "month", length.out = passos_futuro + 1)[-1]
    Datas_Completas <- c(Datas_Hist, Datas_Fut)
    
    # Extrai AIC e BIC nativos do modelo ajustado
    aic_val <- AIC(modelo_arima)
    bic_val <- BIC(modelo_arima)

    # Criar DataFrame temporário combinando o Histórico e as Previsões
    df_combinado <- data.frame(
      Localidade = local,
      Data = Datas_Completas,
      Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
      Previsao_ARIMA = rep(NA_real_, length(Datas_Completas)),
      AIC = aic_val,
      BIC = bic_val,
      Modelo = "ARIMA"
    )
    
    # Injeta a previsão no espaço temporal correto (Teste + Futuro)
    idx_start <- n_treino + 1
    idx_end <- n_treino + length(previsoes_reais)
    df_combinado$Previsao_ARIMA[idx_start:idx_end] <- previsoes_reais
    
    lista_resultados_arima[[local]] <- df_combinado
    cat(" Concluído com Sucesso.")
    
  }, error = function(e) {
    cat(" Erro na Tentativa 1, usando Plano B...")
    
    # PLANO B (Fallback): Roda sem o Box-Cox (sem o lambda) e testa automático
    tryCatch({
      modelo_arima_b <- auto.arima(
        serie_treino, 
        d = 1,                 # Força a tendência
        stepwise = TRUE,       # Busca mais segura e rápida
        approximation = TRUE
      )
      
      previsoes_arima <- forecast(modelo_arima_b, h = horizonte_total)
      previsoes_reais <- as.numeric(previsoes_arima$mean)
      
      # 8. Montar Calendário Completo (Plano B)
      Datas_Hist <- df_local$Data
      Datas_Fut  <- seq(max(Datas_Hist), by = "month", length.out = passos_futuro + 1)[-1]
      Datas_Completas <- c(Datas_Hist, Datas_Fut)
      
      aic_val <- AIC(modelo_arima_b)
      bic_val <- BIC(modelo_arima_b)

      df_combinado <- data.frame(
        Localidade = local,
        Data = Datas_Completas,
        Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
        Previsao_ARIMA = rep(NA_real_, length(Datas_Completas)),
        AIC = aic_val,
        BIC = bic_val,
        Modelo = "ARIMA"
      )
      
      idx_start <- n_treino + 1
      idx_end <- n_treino + length(previsoes_reais)
      df_combinado$Previsao_ARIMA[idx_start:idx_end] <- previsoes_reais
      
      lista_resultados_arima[[local]] <- df_combinado
      cat(" Concluído via auto.arima.")
      
    }, error = function(e2) {
      cat(" Falha total na modelagem matemática:", e2$message)
    })
  })
}

# 9. Consolidação Final
cat("\n\nEmpilhando resultados de todos os municípios...\n")
df_previsoes_arima_pronto <- bind_rows(lista_resultados_arima)

# 10. Salva o Dataframe Final exportável em CSV
caminho_saida <- file.path(
  "Previsoes_output", "previsoes_arima_pre_comp.csv"
)
# Cria o diretório se ele não existir
dir.create(dirname(caminho_saida), showWarnings = FALSE, recursive = TRUE)

# Salva em formato CSV
write.csv(df_previsoes_arima_pronto, caminho_saida, row.names = FALSE)

cat("=====================================================\n")
cat("Processo Offline Concluído! Arquivo CSV gerado em:\n", caminho_saida, "\n")
cat("=====================================================\n")