# ==============================================================================
# pre_compilar_sarimax.r - Script Offline para Pré-Calcular Projeções (SARIMAX)
# ==============================================================================

# 1. Carregar pacotes
library(dplyr)
library(forecast)

cat("Iniciando o processo de pré-compilação das previsões do modelo SARIMAX (Com Variável Exógena)...\n")

# 2. Carregar a base de dados central
dados_basicos <- read.csv("data/veiculos_por_mes_municipio.csv")

# 3. Definir as localidades a processar 
municipios <- unique(dados_basicos$LOCAL) 
localidades <- c("PARÁ (Geral)", municipios)

# Lista para armazenar os resultados iterados
lista_resultados_sarimax <- list()

# 4. Parâmetros de Horizonte
n_teste <- 12
passos_futuro <- 72 # 6 Anos de Previsão
horizonte_total <- n_teste + passos_futuro

# 5. Laço (Loop) iterando por cada localidade 
for (local in localidades) {
  cat("\nProcessando modelo SARIMAX para:", local, "...")
  
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
  
  # Ignora municípios com dados insuficientes
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
  
  # 7. Construção da Variável Exógena (Dummy da Crise Econômica 2013-2017)
  Datas_Hist <- df_local$Data
  Datas_Fut  <- seq(max(Datas_Hist), by = "month", length.out = passos_futuro + 1)[-1]
  Datas_Completas <- c(Datas_Hist, Datas_Fut)
  
  # Cria o vetor sinalizando onde ocorreu a crise
  dummy_crise_expandida <- ifelse(
    Datas_Completas >= as.Date("2013-01-01") & Datas_Completas <= as.Date("2017-12-31"),
    1, 0
  )
  
  # Divide a dummy matemática exatamente onde as séries se dividem
  dummy_treino <- head(dummy_crise_expandida, n_treino)
  dummy_previsao <- tail(dummy_crise_expandida, length(Datas_Completas) - n_treino)
  
  # 8. Modelo SARIMAX com Exógenas
  tryCatch({
    # TENTATIVA 1: O modelo rigoroso
    modelo_sarimax <- auto.arima(
      serie_treino, 
      xreg = dummy_treino,       # <-- Insere a Variável Exógena no treino
      stepwise = FALSE, 
      approximation = FALSE, 
      seasonal = TRUE,
      lambda = 0,                # <-- Box-Cox
      D = 1
    )
    
    # Prever passando o futuro da dummy (0)
    previsoes_sarimax <- forecast(
      modelo_sarimax, 
      xreg = dummy_previsao,     # <-- Insere a Variável Exógena no Teste + Futuro
      h = horizonte_total
    )
    previsoes_reais <- as.numeric(previsoes_sarimax$mean)

    aic_val <- AIC(modelo_sarimax)
    bic_val <- BIC(modelo_sarimax)
    
    # Criar DataFrame temporário combinando o Histórico e as Previsões
    df_combinado <- data.frame(
      Localidade = local,
      Data = Datas_Completas,
      Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
      Previsao_SARIMAX = rep(NA_real_, length(Datas_Completas)),
      AIC = aic_val,
      BIC = bic_val,
      Modelo = "SARIMAX"
    )
    
    # Injeta a previsão no espaço temporal correto
    idx_start <- n_treino + 1
    idx_end <- n_treino + length(previsoes_reais)
    df_combinado$Previsao_SARIMAX[idx_start:idx_end] <- previsoes_reais
    
    lista_resultados_sarimax[[local]] <- df_combinado
    cat(" Concluído com Sucesso.")
    
  }, error = function(e) {
    cat(" Erro rigosoro, usando Plano B...")
    
    # PLANO B (Fallback): Simplificado
    tryCatch({
      modelo_sarimax_b <- auto.arima(
        serie_treino, 
        xreg = dummy_treino,
        stepwise = TRUE,       
        approximation = TRUE,  
        seasonal = TRUE,       
        D = 1                  
      )
      
      previsoes_sarimax <- forecast(
        modelo_sarimax_b, 
        xreg = dummy_previsao,
        h = horizonte_total
      )
      previsoes_reais <- as.numeric(previsoes_sarimax$mean)

      aic_val <- AIC(modelo_sarimax_b)
      bic_val <- BIC(modelo_sarimax_b)
      
      df_combinado <- data.frame(
        Localidade = local,
        Data = Datas_Completas,
        Real = c(df_local$valor_mes, rep(NA_real_, passos_futuro)),
        Previsao_SARIMAX = rep(NA_real_, length(Datas_Completas)),
        AIC = aic_val,
        BIC = bic_val,
        Modelo = "SARIMAX"
      )
      
      idx_start <- n_treino + 1
      idx_end <- n_treino + length(previsoes_reais)
      df_combinado$Previsao_SARIMAX[idx_start:idx_end] <- previsoes_reais
      
      lista_resultados_sarimax[[local]] <- df_combinado
      cat(" Concluído via auto.arima (simplificado).")
      
    }, error = function(e2) {
      cat(" Falha total na modelagem matemática:", e2$message)
    })
  })
}

# 9. Consolidação Final
cat("\n\nEmpilhando resultados de todos os municípios...\n")
df_previsoes_sarimax_pronto <- bind_rows(lista_resultados_sarimax)

# 10. Salva o Dataframe Final exportável em CSV

caminho_saida <- file.path(
  "Previsoes_output", "previsoes_sarimax_pre_comp.csv"
)
# Cria o diretório se ele não existir
dir.create(dirname(caminho_saida), showWarnings = FALSE, recursive = TRUE)

# Salva em formato CSV
write.csv(df_previsoes_sarimax_pronto, caminho_saida, row.names = FALSE)

cat("=====================================================\n")
cat("Processo Offline Concluído! Arquivo CSV gerado em:\n", caminho_saida, "\n")
cat("=====================================================\n")