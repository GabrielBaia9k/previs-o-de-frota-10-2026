# CONTEXTO — previsão de frota e continuidade no Codex

Atualizado em **03/11/2026**. Este documento registra fatos verificados, decisões
anteriores e trabalho proposto. **XGBoost, Prophet e TCN ainda não foram
implementados.** Os passos para incluí-los abaixo são planejamento.

## 1. Objetivo e resultado esperado

Manter um dashboard **R/Shiny**, executado localmente com `shiny::runApp()`, para
consultar projeções de frota por município/estado, analisar modelos e comparar
algoritmos. O frontend já atende ARIMA, ETS, SARIMA, SARIMAX, LSTM e GRU.

A próxima etapa pretendida é consolidar os testes como parte do desenvolvimento
e incluir **XGBoost, Prophet e TCN**, usando o CSV geral de entrada e o mesmo
contrato de saída. O resultado esperado são previsões reproduzíveis, comparações
temporalmente corretas e exibição dos modelos disponíveis nas três abas.

## 2. Contexto necessário, requisitos e restrições

### Repositório e ambientes

- Repositório ativo: https://github.com/GabrielBaia9k/previs-o-de-frota-10-2026
- Clone: `https://github.com/GabrielBaia9k/previs-o-de-frota-10-2026.git`.
- Branch ativa e padrão verificada: `publicacao-codigo`; não presumir `main`.
- Base remota conferida para publicar contexto/testes: `6b4b2fd9edd4e4434bc0b78cd10c69e13bd09740`,
  de 03/11/2026, mensagem: “atualizando o front end para receber multiplos modelos de previsão.”
- Workspace principal: `/workspace/previs-o-de-frota-10-2026`.
- O **HEAD local continua em** `4bdb63b0ec5b6c977aee0edfa571fe12e1e4b454`.
  Há alterações locais autorizadas em `global.R`, nos três módulos de previsão e
  em `pre_compilar_GRU_global.r`. A publicação de contexto/testes usa um checkout
  isolado; o checkout principal não é sincronizado por esse processo e pode
  continuar exibindo esses arquivos como não rastreados. Não fazer reset/checkout
  que descarte o trabalho local de produção.
- **Este commit adiciona apenas** `CONTEXTO.md`, `tests/README.md`,
  `tests/test_frontend.R` e `tests/test_gru_csv.R`. Não inclui a migração do GRU
  nem novas alterações nos scripts de produção. Consultar o histórico Git para
  obter o identificador do commit que contém este documento.
- Os quatro arquivos do frontend foram publicados pelo usuário em `6b4b2fd`.
  A diferença de HEAD não significa que os arquivos locais estejam todos antigos.
  Há diferenças de formatação entre as cópias, e a migração local do GRU não está
  no remoto. Reconsultar o GitHub no novo chat antes de assumir esse estado.
- A revisão remota foi feita em cópia isolada:
  `/tmp/frota-revisao-github-20261103-AzyZ4J`. Esse caminho é temporário.

O usuário trabalha no **Windows, com VS Code e acesso ao GitHub**. O VS Code
local não acessa o Codex por restrições de firewall. Projeto local informado:
`C:\Users\gabriel.baia\Documents\Projetos vscode\DETRAN\previsão de frota 10-2026`.
O workspace do Codex, o computador Windows e o GitHub são ambientes diferentes.

### Preferências e limites de trabalho

- Usar R/Shiny; o usuário descartou o frontend Vue.js.
- Usar caminhos relativos à raiz do projeto; preservar os dados já presentes.
- Preferir alterações pequenas e preservar o comportamento existente.
- Prosseguir com leitura, validação e trabalho já autorizado sem pedir
  confirmações redundantes.
- **Entregar código novo completo pelo chat e também em arquivos no workspace**.
- Não publicar/pushar alterações nem enviar arquivos ao Drive sem solicitação
  correspondente. O usuário tem feito os commits pelo VS Code.
- O pedido deste commit autoriza publicar o contexto e os testes no GitHub,
  sem alterar scripts de produção ou implementar os novos modelos.

Os downloads na interface `https://chatgpt.com/local/…` apresentaram “Não foi
possível baixar o artefato”. Downloads funcionaram em um chat normal, segundo o
usuário. Links de workspace/ZIP não devem ser apresentados como solução garantida.
Outro chat não recebe automaticamente os arquivos desta máquina. Após a
publicação, obter este documento e `tests/` pelo GitHub; o ZIP é uma alternativa.
A alteração local do GRU ainda precisa ser levada separadamente ou versionada
em uma tarefa posterior.

O usuário instalou o plugin Google Drive e quer consultar a pasta
`Codex/Workspace-previsoes`. Nas últimas verificações não havia ferramentas do
Drive disponíveis neste chat. **Acesso à pasta e transferência de arquivos não
foram confirmados**; não concluir que a pasta inexiste ou que houve recusa de acesso.

## 3. Decisões tomadas e justificativas

1. **Separar dados e previsões:** bases e históricos de treinamento em `data/`;
   previsões dos modelos em `Previsoes_output/`, com essa grafia e sublinhado.
2. **Padronizar as previsões em CSV**, inclusive SARIMAX. Os outros Parquets de
   `data/`, usados pelas abas de caracterização e pela base acumulada, continuam
   necessários; não remover a pasta nem todos os Parquets.
3. **Reutilizar `pt_carregar_previsoes()`**, definido em
   `modulos/previsoes_todos.R`, em vez de criar leitores diferentes por modelo.
   `global.R` o chama após carregar os módulos e disponibiliza
   `previsoes_compartilhadas$dados` e `$avisos`.
4. Comparar localidades por chave sem acentos/diferenças de caixa, usando
   `pt_normalizar_localidade()` e `pt_chave_localidade()`. Preservar rótulos de
   exibição, evitando perda da base de 2025 em joins como `BELEM` versus `Belém`.
5. A aba Previsão de Frota mantém a regra **base acumulada de 2025 + soma
   acumulada das previsões mensais de 2026 a 2031**. O cartão usa o mesmo total
   da tabela; a linha estadual não é somada novamente aos municípios.
6. Métricas usam apenas pares finitos de `Real`/`Previsao`. MAPE exclui `Real=0`;
   MAE/RMSE/R² mantêm esses registros. R² fica indisponível para série constante
   ou menos de dois pares. AIC/BIC ausentes aparecem como indisponíveis.
7. A Comparação permite **meses comuns** entre modelos com observações ou
   **períodos individuais**, mostrando início, fim e contagens, incluindo a
   contagem específica do MAPE. Um modelo só com futuro não bloqueia os demais.
8. As curvas de perda são de **LSTM local e GRU global**; gradientes são do LSTM.
   Não fabricar esses históricos para modelos que não os produzem. Perdas MSE
   normalizadas de redes diferentes não são, por si, ranking de precisão.
9. O treinamento é offline. Não executar `fit()` nas abas nem tornar XGBoost,
   Prophet ou TensorFlow dependências de `global.R` apenas para visualizar CSVs.
   Dependências de treinamento pertencem aos scripts produtores.

## 4. O que já foi concluído e validado

### Frontend publicado

| Arquivo | Comportamento validado |
|---|---|
| `global.R` | Pastas separadas e leitura padronizada dos seis CSVs. |
| `modulos/previsoes.R` | Dropdown com seis modelos, filtros geográficos, acumulados, totais corretos de região/seleção, Limpar e nome de download dinâmico. |
| `modulos/modelagem.R` | Modelo + localidade controlam tabela, gráfico, indicadores e exportação; zeros no MAPE e AIC/BIC ausentes tratados. |
| `modulos/previsoes_todos.R` | Seis modelos no gráfico/métricas, cores/legendas, modelos ausentes e períodos explícitos; perdas/gradientes preservados. |
| `app.R` e `modulos/03_mod_downset.R` | As chamadas existentes continuam compatíveis; não precisaram de adaptação. |

Na cópia de `6b4b2fd`, foram verificados sintaxe de 23 arquivos R de produção,
carregamento dos seis CSVs sem avisos, testes dos três módulos com
`shiny::testServer()`, construção do aplicativo e exportações CSV/Excel/RDS e
PNG/PDF das perdas. A validação foi na nuvem; a revisão visual no Windows não
foi confirmada. Não houve novo treinamento nem regeneração das previsões.

### Testes preparados para versionamento — prioridade de continuidade

Pacote persistente no workspace: **`/workspace/outputs/testes_frota.zip`**.
Conteúdo, para extrair diretamente na raiz do projeto:

```text
tests/
  README.md
  test_frontend.R
  test_gru_csv.R
```

- Cópia aberta do pacote: `/workspace/outputs/testes_frota/tests/`.
- ZIP: 8.203 bytes; SHA-256
  `8dbfc4b4987f83ab26ffe8e46f7260267b6439586a285b2ab6a2886d4abe77d7`.
- **Os três arquivos do pacote estão em `tests/` neste commit**, incluindo as
  mensagens de pré-condição melhoradas para o GRU. Foram também instalados na
  raiz do projeto do workspace. Clonar a versão que contém este commit dispensa
  extrair o ZIP. O arquivo ZIP é a cópia de distribuição preparada anteriormente;
  o desenvolvimento contínuo deve usar os arquivos versionados em `tests/`.
- A versão durável deriva de `/tmp/revisar_frontend_github_20261103.R`; esse
  script temporário e outros scripts antigos não são a fonte recomendada para
  desenvolvimento contínuo. Arquivos de `/tmp` podem desaparecer.

Execução, **na raiz do projeto**, em sessões R novas:

```powershell
Rscript --vanilla tests/test_frontend.R
Rscript --vanilla tests/test_gru_csv.R
```

Alternativa no console R:

```r
source("tests/test_frontend.R", encoding = "UTF-8")
source("tests/test_gru_csv.R", encoding = "UTF-8")
```

No ambiente de nuvem atual, quando o runtime ainda existir:

```bash
cd /workspace/previs-o-de-frota-10-2026
bash /workspace/.cloud-setup/frota/with-r.sh Rscript --vanilla tests/test_frontend.R
bash /workspace/.cloud-setup/frota/with-r.sh Rscript --vanilla tests/test_gru_csv.R
```

O wrapper usa micromamba; preferir execução sequencial para evitar disputa de
locks. O pacote é portátil e não depende desse wrapper, do Codex nem de `/tmp`.
O R da nuvem foi preparado para o frontend, na versão 4.5.3; os backends dos
novos modelos ainda precisam ser conferidos, instalados e validados quando necessário.

**`test_frontend.R`:** passou após extração do ZIP sobre a cópia do GitHub.
Verifica sintaxe, leitor, seis modelos, acumulados, filtros, totais, métricas,
zeros, gráficos, exportações, períodos comum/individual, ausência de modelos,
perdas, gradientes e integração da UI. Termina com `TESTES_FRONTEND_CONCLUIDOS`.
Não fixa 100 épocas nem exatamente 12 meses observados. Ainda exige exatamente
os seis modelos atuais, horizonte 2026–2031 e casos Pará, Belém, Ananindeua e
Guajará. Deve ser atualizado quando o conjunto de modelos/contrato mudar.

**`test_gru_csv.R`:** passou com **56 verificações** usando o GRU CSV local.
Extrai configurações/funções do script por `parse()`; não carrega TensorFlow ou
Keras e não executa treinamento. Cobre BOM/acentos, esquema, valores inválidos,
agregação, séries curtas e preparação/combinação dos tensores.

Essa regressão usa a base/configuração atuais: 94.920 linhas em 2016–2025,
144 municípios, soma 1.433.667, treino `84 × 24 × 158`, teste `12 × 24 × 158`
para Pará/Belém e combinação de dois blocos `168 × 24 × 158`. Afuá e Chaves são
curtas e ignoradas individualmente, mas seus 31 veículos no período integram
o estado. Atualizações deliberadas da base/configuração exigem revisão dessas
referências; não apagar verificações apenas para conseguir resultado verde.

**Falha conhecida e esperada no GitHub:** em `6b4b2fd` e neste commit exclusivo
de documentação/testes, o GRU não define
`ler_historico_gru()` e ainda usa o Parquet SARIMAX. O teste empacotado informa
explicitamente que a migração CSV é necessária e encerra com falha. Foi
verificado esse caso; não é defeito de instalação do teste nem motivo para
silenciá-lo. O frontend pode ser testado separadamente.

Não existe CI configurado por esse pacote. Os testes não instalam dependências,
não treinam modelos e não substituem backtesting, avaliação estatística ou
revisão visual. Fixtures e exportações de verificação são temporárias.

## 5. Estado atual, problemas e pendências

### Entrada GRU e coerência dos dados

- **Workspace:** `pre_compilar_GRU_global.r` já lê
  `data/veiculos_por_mes_municipio.csv` por `ler_historico_gru(config)`.
  Valida LOCAL/ANO/MES/QTD_VEICULO, filtra 2016–2025 e remove totais estaduais
  eventualmente presentes, antes de agregar os municípios. Mantém o destino
  `Previsoes_output/previsoes_gru_global_pre_comp.csv`.
- **GitHub `6b4b2fd`:** esse script continua com entrada
  `data/previsoes_sarimax_pre_comp.parquet`. A migração local precisa ser levada
  ao repositório. Conferir `git diff -- pre_compilar_GRU_global.r` antes de
  sincronizar; preservar a versão local autorizada.
- Os CSVs de previsão e históricos não mudaram no commit do frontend. Para Pará,
  a soma de `Real` nos 12 meses de 2025 é **248.133 no GRU e 248.140 nos demais**.
  A agregação direta do CSV geral de 2025 também resulta em **248.140**.
  Reconciliar os meses/municípios que explicam a diferença antes de afirmar que
  os modelos foram avaliados contra a mesma verdade observada. O frontend avisa
  a divergência; usar datas comuns, sozinho, não resolve isso.
- Cobertura atual: ARIMA tem 125 localidades; os outros cinco têm 143,
  incluindo o estado. Totais regionais somam somente localidades com saída
  disponível. Não confundir cobertura parcial com previsão de todos os municípios.

### Pendências metodológicas anteriores — não estão corrigidas

- GRU e LSTM estimam tendência/normalização com o histórico completo antes da
  separação treino/teste: revisar vazamento temporal.
- O LSTM contém `array(unlist(X), dim = c(length(X), lookback, 13))`; a revisão
  anterior identificou perda do alinhamento das janelas. Reconfirmar com dados
  rotulados e corrigir antes de copiar esse trecho para a TCN. O GRU atual usa
  `aperm()` na formação das janelas; não generalizar a falha do LSTM ao GRU.
- A validação interna do GRU usa `validation_split` sobre blocos concatenados
  por localidade, não um corte cronológico comum explícito para todos os municípios.
- O teste das redes usa lags reais precedentes no período de teste, enquanto os
  clássicos preveem um bloco futuro recursivamente. Fixar um protocolo comparável.
- ARIMA/LSTM/GRU usam 2016–2025; ETS/SARIMA/SARIMAX usam 2015–2025.
- Há meses ausentes em séries municipais; decidir quando são zeros reais ou
  dados faltantes antes de regularizar/imputar. Não tratar linhas irregulares
  automaticamente como meses consecutivos.
- Rolling-origin, múltiplas sementes, early stopping e incerteza ainda não
  compõem uma avaliação completa comum. Os testes atuais não comprovam ausência
  desses problemas nem qualidade preditiva.

### Atualização e semântica da UI

- `previsoes_compartilhadas` é carregado ao iniciar o aplicativo. O botão
  “Recarregar arquivos” atualiza somente a Comparação. Reiniciar o app atualiza
  as três abas; recarga reativa compartilhada é melhoria ainda não implementada.
- Confirmar a semântica de QTD_VEICULO: a projeção acumulada atual interpreta
  a previsão mensal como acréscimo à frota de 2025. Se a variável representar
  estoque mensal, somá-la novamente seria inadequado. Não mudar esse significado
  silenciosamente ao implementar os novos modelos.

## 6. Arquivos, links, fontes de dados e trechos relevantes

### Fluxo e contrato existentes

```text
data/veiculos_por_mes_municipio.csv
  → validação, agregação municipal/estadual e preparação temporal
  → treinamento/avaliação por algoritmo
  → Previsoes_output/previsoes_<modelo>_pre_comp.csv
  → pt_carregar_previsoes()
  → abas Previsão de Frota, Modelagem e Comparação dos Algoritmos
```

O CSV geral tem `LOCAL, ANO, MES, TIPO_VEICULO, QTD_VEICULO`; contém anos
anteriores ao recorte de modelagem, portanto requer filtro explícito. Agregar
os tipos por município/mês e formar datas mensais. Os demais cinco algoritmos
implementados já leem esse CSV; a exceção remota é o GRU descrito acima.

Cada saída deve conter `Localidade`, `Data` ISO `YYYY-MM-DD`, `Real` e
`Previsao_<ID_EXATO_DO_MODELO>`. AIC/BIC são opcionais; `Modelo` pode ser exportado,
mas o leitor atualmente atribui o identificador conforme seu registro de arquivos.
Histórico sem previsão usa `NA` na previsão; futuro sem observação usa `NA` em
Real. Zero observado não é dado ausente. Não exportar a frota acumulada de 2031
no lugar da série mensal: esse acumulado é calculado pela aba Previsão de Frota.

**Importante para os novos produtores:** preencher `Previsao_<ID>` somente nos
meses de teste fora da amostra e no futuro. Deixar `NA` nos meses de treinamento.
O frontend atual identifica períodos avaliáveis pela presença de Real + previsão;
não há um filtro de partição treino/teste. Exportar valores ajustados no treino
nessa mesma coluna contaminaria as métricas. Se desejar exibir ajustes de treino,
criar um contrato explícito de partições e adaptar leitor/métricas antes.

| ID atual | Arquivo em Previsoes_output/ | Coluna bruta |
|---|---|---|
| ARIMA | previsoes_arima_pre_comp.csv | Previsao_ARIMA |
| ETS | previsoes_ets_pre_comp.csv | Previsao_ETS |
| SARIMA | previsoes_sarima_pre_comp.csv | Previsao_SARIMA |
| SARIMAX | previsoes_sarimax_pre_comp.csv | Previsao_SARIMAX |
| LSTM | previsoes_lstm_pre_comp.csv | Previsao_LSTM |
| GRU | previsoes_gru_global_pre_comp.csv | Previsao_GRU |

O leitor normaliza para `Modelo, Localidade, Data, Real, Previsao, AIC, BIC`.
Os trechos já implementados que sustentam as três abas são:

```r
data_dir <- here::here("data")
previsoes_dir <- here::here("Previsoes_output")
# Executado após carregar modulos/previsoes_todos.R:
previsoes_compartilhadas <- pt_carregar_previsoes(data_dir = previsoes_dir)
shiny::runApp() # executar na raiz do projeto
```

A base inicial acumulada vem de `data/db01_acumulado.parquet`; o mapa de regiões
vem de `data/frota_acumulado.parquet`. Históricos de treinamento permanecem em
`data/historico_perdas_lstm.csv`, `data/historico_perdas_gru.csv` e
`data/historico_gradientes_lstm.csv`.

### Situação dos três novos modelos, verificada no workspace

| Modelo | Arquivo atual | Situação |
|---|---|---|
| XGBoost | pre_compilar_xgboost.r | Vazio, 0 bytes. |
| Prophet | pre_compilar_prophet.r | Vazio, 0 bytes. |
| TCN | pre_compilar_TCN | Vazio, 0 bytes, sem extensão R. |

**Convenção proposta, ainda não implementada:**

| ID | Script | Saída em Previsoes_output/ | Coluna |
|---|---|---|---|
| XGBOOST | pre_compilar_xgboost.r | previsoes_xgboost_pre_comp.csv | Previsao_XGBOOST |
| PROPHET | pre_compilar_prophet.r | previsoes_prophet_pre_comp.csv | Previsao_PROPHET |
| TCN | pre_compilar_tcn.r | previsoes_tcn_pre_comp.csv | Previsao_TCN |

É preciso criar/renomear o arquivo TCN com extensão `.r`; o arquivo atual não
entra na varredura de sintaxe dos testes. Escopo local/global, hiperparâmetros
e arquitetura dos novos modelos ainda não foram escolhidos.

### Referências discutidas e fontes para continuidade

- Artigo enviado pelo usuário: https://arxiv.org/abs/2412.03700 e
  https://arxiv.org/pdf/2412.03700 — boas práticas de avaliação de ML, versão v1.
  PDF ainda disponível neste workspace em
  `/workspace/attachments/82ba0a5a-a87d-402c-97dc-6f2c285e5358/2412.03700v1.pdf`.
- Referências adicionais discutidas: validação temporal em
  https://robjhyndman.com/hyndsight/tscv-fable/ ; MASE em
  https://doi.org/10.1016/j.ijforecast.2006.03.001 ; modelos locais/globais em
  https://doi.org/10.1016/j.ijforecast.2021.03.004 .
- Fontes para consultar na implementação, sem assumir que os modelos estejam
  prontos: https://xgboost.readthedocs.io/ ;
  https://facebook.github.io/prophet/docs/quick_start.html ;
  https://keras3.posit.co/reference/layer_conv_1d.html ;
  referência TCN https://arxiv.org/abs/1803.01271 .

## 7. Próximos passos concretos

### A. Preservar e consolidar a base de desenvolvimento

1. No novo chat, conferir branch, último commit e alterações locais. Obter
   `CONTEXTO.md` e `tests/` pelo GitHub e levar a versão/diff local do GRU CSV.
   O ZIP pode ser usado como alternativa. Não presumir que caminhos de `/tmp`
   ou arquivos fora do Git foram transferidos.
2. Os três arquivos de `tests/` já estão incluídos neste commit. Conferir as
   dependências e executar as suítes na raiz do projeto. Preservar o README
   principal e versionar futuras mudanças nos testes com o código correspondente.
3. Incorporar a migração GRU CSV; executar primeiro o frontend e depois as
   56 verificações do GRU. Manter explícita a falha esperada enquanto a migração
   estiver ausente. Atualizar o pacote/README se os testes evoluírem.
4. Reconciliar `Real` entre saídas e CSV geral. Uma alteração no script não
   atualiza CSVs já gerados: regenerar conforme o escopo autorizado e as
   dependências disponíveis; validar a nova agregação antes de treinar tudo.

### B. Fixar protocolo comum antes de comparar nove algoritmos

5. Definir o alvo mensal, calendário, recorte histórico, cobertura, holdout e
   horizontes. Manter 2026–2031 como contrato atual da UI até mudança deliberada.
   O corte observado atual é dezembro/2025; não usar automaticamente dados mais
   recentes para os novos modelos sem rever também a comparação dos existentes.
6. Separar temporalmente treino, validação e teste antes de ajustar tendências,
   escalas ou sazonalidade. Usar validação rolling-origin; reservar teste final
   independente e comparar com baselines ingênuo e sazonal.
7. Definir previsão recursiva, direta por horizonte ou avaliação com atualização
   por observações. Comparar modelos sob a mesma estratégia e os mesmos valores
   reais; não comparar automaticamente teste de um passo com previsão de 12 passos.
8. Definir tratamento de lacunas, séries curtas, valores negativos previstos e
   arredondamento. Não esconder esses tratamentos no leitor nem alterar métricas
   para favorecer um algoritmo. Registrar configuração, semente e execução.

### C. Implementar os novos produtores, um por vez

9. **XGBoost:** criar features causais de lags/sazonalidade/médias móveis, sempre
   usando informações anteriores ao alvo; escolher treinamento local/global.
   Ajustar hiperparâmetros e early stopping com validação temporal. Na previsão
   recursiva, alimentar os lags futuros com previsões, sem acessar Real futuro.
   Avaliar na escala original e exportar o CSV proposto.
10. **Prophet:** adaptar a série mensal para `ds`/`y`; ajustar apenas no treino,
    calibrar sazonalidade anual/changepoints na validação e prever datas mensais,
    não 72 dias. Exportar `yhat` como `Previsao_PROPHET`, preservando Real.
    Regressoras futuras só podem ser usadas se forem conhecidas ou previstas sob
    protocolo explícito. Não criar curvas de épocas ou AIC/BIC fictícios.
    Não exportar `yhat` ajustado no treino como previsão de teste; após a
    avaliação, distinguir o reajuste para produção do modelo avaliado no holdout.
11. **TCN:** preparar tensores `[amostras, tempo, variáveis]` com alinhamento
    verificado. Definir modelo local/global e backend; uma opção compatível com
    o projeto é Keras 3 com Conv1D causal, dilatações e blocos residuais.
    Dimensionar o campo receptivo e usar validação temporal explícita/early
    stopping, mantendo o teste isolado. Não copiar a montagem incorreta do LSTM.
    Exportar `Previsao_TCN`; registrar perdas reais de treino/validação por época.
12. Para cada produtor, validar em poucas séries e fixtures antes da execução
    completa. AIC/BIC podem ser omitidos ou NA; não são obrigatórios para os
    novos modelos. Intervalos e diagnósticos adicionais exigem contrato próprio.

### D. Integrar os novos modelos ao frontend e aos testes

13. Em `modulos/previsoes_todos.R`, atualizar **todos** os registros relevantes:
    `pt_carregar_previsoes()`/`especificacoes`, `pt_modelos`, `pt_cores`, textos da
    UI e política da linha Real. Em `modulos/previsoes.R` e `modulos/modelagem.R`,
    adicionar os IDs aos dropdowns. Preservar os modelos ausentes como situação
    suportada, sem bloquear os presentes. Avaliar um registro único para reduzir
    listas duplicadas; essa refatoração ainda não existe.
14. Atualizar os scripts de execução do README e as dependências. Só registrar
    um novo modelo como obrigatório nos testes quando seu produtor/arquivo
    estiver disponível. O teste atual exige seis modelos e zero avisos do leitor;
    adicionar três entradas sem arquivos fará essa integração falhar.
15. Atualizar expectativas/loops da integração de seis para sete, oito e nove
    modelos conforme a implantação. Manter fixtures parciais de modelos ausentes,
    futuro sem Real, zeros, constantes e datas sem interseção.
16. Adicionar testes dos novos produtores para: agregação comum e Real idêntico;
    lags/escalas sem vazamento; janelas/targets corretamente alinhados; projeção
    que não lê o futuro; 72 meses futuros quando aplicável; esquema, datas,
    unicidade `(Modelo, Localidade, Data)`, valores válidos e ausência de ajustes
    de treino na coluna usada para métricas. Testes de preparação
    devem ser leves; treinamento completo não deve virar requisito de cada teste.
17. Validar cada novo modelo nas três abas e nos downloads, incluindo filtros,
    totais, nomes e períodos. Conferir visualmente no Windows. Reempacotar os
    testes se necessário; o ZIP atual continua referente aos seis modelos.
18. A inclusão da TCN nas curvas de perda é uma **extensão ainda a decidir**.
    Pode usar `data/historico_perdas_tcn.csv` com Modelo/Localidade/Epoca/Loss/
    Val_Loss e escopo local/global explícito. Separar execuções/folds/sementes
    para evitar épocas duplicadas. Não gravar perdas TCN nos arquivos LSTM/GRU
    nem inventar gradientes TCN; o diagnóstico existente continua sendo do LSTM.

Critério de conclusão: os modelos implementados produzem o contrato mensal
comum, os testes pertinentes passam, as abas exibem corretamente a disponibilidade
e os resultados, e a avaliação estatística tem protocolo/verdade observada
coerentes. Nenhum desses critérios autoriza presumir boa precisão apenas porque
o arquivo foi lido ou o gráfico foi desenhado.
