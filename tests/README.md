# Testes do projeto de previsao de frota

## Onde colocar os arquivos

Extraia a pasta `tests/` deste ZIP diretamente na raiz do projeto, no mesmo nivel
de `app.R` e `global.R`. Preserve o README principal do repositorio.

```text
<raiz-do-projeto>/
  app.R
  global.R
  pre_compilar_GRU_global.r
  modulos/
  data/
  Previsoes_output/
  tests/
    README.md
    test_frontend.R
    test_gru_csv.R
```

Versione os tres arquivos da pasta `tests/`. Nao e necessario versionar o ZIP.
Os testes nao dependem de caminhos como `/workspace`, `/tmp` ou de uma conta Codex.

## Como executar no Windows / VS Code

Abra a raiz do projeto no VS Code e o terminal PowerShell. Com o Rscript no PATH:

```powershell
Rscript --vanilla tests/test_frontend.R
Rscript --vanilla tests/test_gru_csv.R
```

Tambem e possivel executar pelo console R, com o diretorio de trabalho na raiz:

```r
source("tests/test_frontend.R", encoding = "UTF-8")
source("tests/test_gru_csv.R", encoding = "UTF-8")
```

Execute cada teste em uma sessao R nova quando possivel. O teste do frontend
carrega `global.R`, que altera opcoes da sessao, como o separador decimal.

## Dependencias e dados

- Frontend: mesmas dependencias do aplicativo, listadas em `global.R`.
- GRU CSV: pacote `dplyr`; TensorFlow e Keras nao sao carregados pelo teste.
- Mantenha as bases do aplicativo em `data/` e os seis CSVs de previsao em
  `Previsoes_output/`, conforme a estrutura atual do projeto.
- Para o frontend completo, mantenha tambem em `data/` os arquivos
  `historico_perdas_lstm.csv`, `historico_perdas_gru.csv` e
  `historico_gradientes_lstm.csv`.

Os testes nao instalam pacotes, nao treinam redes e nao regeneram previsoes.
Exportacoes de verificacao e dados sinteticos sao criados em pastas temporarias do R.

## O que cada arquivo testa

### test_frontend.R

- Sintaxe dos arquivos R da raiz, de `modulos/` e de `tests/`.
- Leitura padronizada dos seis modelos.
- Projecoes acumuladas sobre a base de 2025, filtros e totais de regiao/selecao.
- Modelagem por modelo/localidade e metricas calculadas nos pares observados.
- MAPE com zeros, valores ausentes, AIC/BIC indisponiveis e graficos.
- Exportacoes CSV, Excel, RDS e graficos de perdas PNG/PDF.
- Comparacao nos periodos comum e individual, incluindo modelos sem observacoes.
- Preservacao das perdas LSTM/GRU, gradientes LSTM e integracao com `app.R`.

Sucesso termina com `TESTES_FRONTEND_CONCLUIDOS`.

### test_gru_csv.R

- Leitura do CSV geral, BOM/acentos, esquema e rejeicao de dados invalidos.
- Agregacao municipal/estadual, ausencia de duplicacao de totais e series curtas.
- Preparacao dos tensores sem executar treinamento.

Sucesso informa `56 verificacoes passaram`.

**Pendencia conhecida:** no commit GitHub `6b4b2fd`, o script GRU ainda usa o
Parquet do SARIMAX. Este teste exige a versao migrada para
`data/veiculos_por_mes_municipio.csv`, com a funcao `ler_historico_gru()`.
Ele falhara ate que essa migracao tambem seja incorporada ao repositorio.
O teste do frontend pode ser executado separadamente enquanto isso.

## Uso continuo

Rode os testes apos alterar leitores, filtros, metricas, exportacoes ou a entrada
do GRU, e antes de publicar mudancas no GitHub. Uma falha encerra o Rscript com
codigo diferente de zero, permitindo integrar esses comandos a uma automacao futura.

O teste GRU conserva referencias da base de 2016 a 2025, como 94.920 linhas,
144 municipios e tensores de treino/teste com dimensoes 84 x 24 x 158 e
12 x 24 x 158. Mudancas deliberadas da base ou da configuracao exigem revisar
essas expectativas; nao remova uma verificacao apenas para obter sucesso.

O teste do frontend exige os seis modelos e o horizonte de 2026 a 2031. Ele nao
fixa o numero de epocas nem um periodo de avaliacao observado de 12 meses.
Ainda usa Para, Belem, Ananindeua e a regiao Guajara como casos de integracao.

Os testes verificam funcionamento e consistencia dos calculos. Eles nao
substituem a revisao visual no navegador, o backtesting temporal nem a avaliacao
da qualidade estatistica dos algoritmos.
