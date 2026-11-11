# Configuracao do ambiente

Este guia orienta a preparacao do projeto em uma nova maquina Windows. O repositorio nao fixa as versoes do R, Python ou dos pacotes; para reproducao mais estrita, registre as versoes efetivamente usadas antes de atualizar o ambiente.

## 1. Obter o projeto

Instale Git, R (64 bits) e, de preferencia, RStudio ou a extensao R do VS Code. Clone o repositorio e abra `frota_detran.Rproj` para trabalhar a partir da raiz do projeto. Os comandos abaixo devem ser executados nessa raiz.

```powershell
git clone https://github.com/GabrielBaia9k/previs-o-de-frota-10-2026.git
Set-Location previs-o-de-frota-10-2026
```

## 2. Instalar pacotes R do dashboard

Em uma sessao R, instale os pacotes usados por `global.R`:

```r
install.packages(c(
  "here", "arrow", "shiny", "shinyWidgets", "shinycssloaders",
  "shinyjs", "bs4Dash", "tidyverse", "janitor", "reactable",
  "openxlsx", "readxl", "echarts4r", "leaflet", "leaflet.extras",
  "sf", "fontawesome", "DT", "htmltools", "scales", "forcats", "quarto"
), repos = "https://cloud.r-project.org")
```

O R pode solicitar ferramentas de compilacao ou bibliotecas do sistema, principalmente para `sf` e `arrow`. No Windows, prefira os binarios oferecidos pelo CRAN e instale as ferramentas indicadas pela mensagem do proprio pacote quando necessario.

## 3. Dados necessarios

O dashboard abre bases Parquet e um mapa RDS da pasta `data/`, alem dos CSVs de previsao em `Previsoes_output/`. Esses arquivos de dados podem nao estar todos no GitHub; obtenha-os da fonte autorizada do projeto e mantenha os nomes/caminhos esperados por `global.R`.

`conversao_parquet.R` nao e um instalador de dados: ele espera objetos e arquivos de origem locais, incluindo arquivos `.RData` que nao sao fornecidos por este guia. Execute-o somente quando tiver essas fontes e souber que a conversao deve ser refeita. Nao sobrescreva dados existentes sem manter uma copia.

## 4. Executar o dashboard e os testes

Com os pacotes e dados disponiveis, rode na raiz do projeto:

```r
shiny::runApp()
```

Os testes existentes podem ser executados em um terminal PowerShell:

```powershell
Rscript --vanilla tests/test_frontend.R
Rscript --vanilla tests/test_gru_csv.R
```

## 5. Modelos e pacotes R opcionais

Os modelos classicos e os scripts auxiliares usam pacotes adicionais. Instale apenas os necessarios para os scripts que pretende executar:

```r
install.packages(c("forecast", "prophet", "xgboost"),
                 repos = "https://cloud.r-project.org")
```

Os produtores LSTM, GRU e TCN precisam destes pacotes R:

```r
install.packages(c("keras3", "tensorflow", "reticulate"),
                 repos = "https://cloud.r-project.org")
```

O treinamento e separado do dashboard: nao e necessario instalar os backends de treinamento para abrir a interface e consultar os CSVs de previsao.

## 6. Python para LSTM, GRU e TCN

Instale Python e o launcher `py` para Windows. Use uma versao de Python compativel com as versoes de TensorFlow e `keras3` instaladas; o projeto nao fixa essa versao. Crie um ambiente virtual local na raiz do repositorio e instale TensorFlow nele:

```powershell
py -3 -m venv .venv
.\.venv\Scripts\python.exe -m pip install --upgrade pip
.\.venv\Scripts\python.exe -m pip install tensorflow
```

Antes de carregar `keras3` ou `tensorflow` em uma sessao R, aponte `reticulate` para esse Python e confira a configuracao:

```r
Sys.setenv(RETICULATE_PYTHON = normalizePath(
  ".venv/Scripts/python.exe", winslash = "/", mustWork = TRUE
))
reticulate::py_config()
reticulate::py_module_available("tensorflow")
tensorflow::tf_config()
keras3::config_backend()
```

O backend esperado pelos scripts LSTM, GRU e TCN e `tensorflow`. Configure o Python antes de inicializar `reticulate`; depois que o Python e carregado na sessao, a selecao nao pode ser trocada nessa mesma sessao. Se a versao instalada de TensorFlow nao for compativel com o Python local, escolha uma combinacao suportada pelos projetos oficiais e recrie `.venv`.

## 7. Configuracoes locais e segredos

Nao versione `.env`, `.Renviron`, credenciais, tokens ou chaves. O `.gitignore` do projeto ignora esses arquivos, ambientes virtuais e caches Python, mas isso nao remove arquivos que ja tenham sido commitados anteriormente. Confira os arquivos preparados antes de cada commit:

```powershell
git status --short
git check-ignore .env .Renviron .venv
```

Se uma variavel local for necessaria, configure-a no ambiente da maquina ou em um arquivo ignorado. Para compartilhar somente os nomes das variaveis, use um arquivo de exemplo sem valores secretos; nunca coloque credenciais reais nele. Nao adicione arquivos de dados restritos ou credenciais ao repositorio.