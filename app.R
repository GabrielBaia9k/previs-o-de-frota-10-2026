#===============================================================================#
#              DETRAN-PA - Departamento de Trânsito do Estado do Pará           #
#===============================================================================#
#==== DESCRIÇÃO:    Dashboard de Frota Veicular - Pará
#==== SITE:         https://www.detran.pa.gov.br/
#==== LICENÇA:      MIT
#==== PROJETO:      https://github.com/MarioDhiego/
#===============================================================================#

source('global.R')

#==============================================================================#
# 1. INTERFACE DO USUÁRIO (UI)
#==============================================================================#

ui <- dashboardPage(
  # --- CABEÇALHO ---
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$strong("Detran - PA"),
      href = "https://www.detran.pa.gov.br/index_.php",
      image = "detran1.jpeg",
      color = "gray"
    )
  ),
  # --- CONFIGURAÇÕES GERAIS ---
  skin = "blue",
  scrollToTop = TRUE,
  fullscreen = TRUE,
  help = TRUE,
  options = list(sidebarExpandOnHover = TRUE),
  
  # --- BARRA LATERAL (SIDEBAR) ---
  sidebar = dashboardSidebar(
    disable = FALSE,
    width = NULL,
    skin = "dark",
    status = "primary",
    collapsed = FALSE,
    minified = TRUE,
    
    sidebarMenu(
      id = "tabs",
      
      # --- Grupo de Análise de Dados ---
      tags$li(class = "nav-header", "ANÁLISE DA FROTA"),
      menuItem(
        "Painel Resumo",
        tabName = "visao_geral",
        icon = icon("display")
      ),
      menuItem(
        "Análise Detalhada",
        icon = icon("magnifying-glass-chart"),
        menuSubItem(
          "Localidade (Estado/RI)",
          tabName = "carac_loc",
          icon = icon("globe")
        ),
        menuSubItem(
          "Município",
          tabName = "carac_muni",
          icon = icon("location-dot")
        ),
        menuSubItem("Tipo de Veículo", tabName = "carac_tipo", icon = icon("car")),
        menuSubItem("Licenciamento/PA", tabName = "pa"),
        menuSubItem("Licenciamento/Município", tabName = "muni")
      ),
      #menuItem(
      #  "Previsão de Frota",
      #  tabName = "previsoes",
      #  icon = icon("chart-line")
      #),
      menuItem(
          "Projeções", icon = icon("project-diagram"), # Titulo pai (sem tabName)
          menuSubItem("Previsão de Frota", tabName = "previsoes", icon = icon("chart-line")),
          menuSubItem("Modelagem", tabName = "modelagem", icon = icon("cogs")), # Exemplo de nova rota
          menuSubItem("Comparação dos Algoritmos",tabName = "previsoes_todos",icon = icon("chart-line")),
          menuSubItem("Análise Multivariada", tabName = "previsoes_multivariada", icon = icon("chart-line"))
      ),
      
      # --- Grupo de Informações Gerais ---
      tags$li(class = "nav-header", "SOBRE O PROJETO"),
      menuItem(
        "Apresentação",
        tabName = "sobre1",
        icon = icon("address-card")
      ),
      #menuItem("Vídeo Institucional", tabName = "video1", icon = icon("video")),
      menuItem(
        "Glossário de Termos",
        tabName = "conceitos",
        icon = icon("book")
      ),
      
      # --- Grupo de Ferramentas ---
      tags$li(class = "nav-header", "FERRAMENTAS"),
      menuItem(
        "Relatório PDF",
        tabName = "relatorio_pdf",
        icon = icon("file-pdf")
      ),
      
      # --- Espaçamento Seguro (Garante distância do menu de PDF) ---
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      div(
        style = "padding-left: 15px;
                 color: #FFFFFF;
                 text-align: left;
                 font-size: 14pt;",
        tags$strong("DADOS ATUALIZADOS"),
        br(),
        "  ( Agosto de 2026 )"
      )
    )
  ),
  
  # --- CORPO DO DASHBOARD ---
  body = dashboardBody(
    tags$head(# Redimensionamento dinâmico de gráficos ao maximizar boxes
      tags$script(
        HTML(
          "
          // Este código 'escuta' por um clique em qualquer botão de maximizar
          $(document).on('click', '[data-card-widget=\"maximize\"]', function(){
            setTimeout(function() {
              $(window).trigger('resize');
            }, 350);
          });
        "
        )
      ), # Código CSS para customização
      tags$style(
        HTML(
          "
          .brand-link { height: 60px !important; }
          .brand-image {
            height: 50px !important;
            width: auto !important;
            margin-top: 5px !important;
          }
        "
        )
      )),
    
    fluidPage(tags$head(
      tags$link(rel = "shortcut icon", href = "icons8-favicon-94.png", type = "image/x-icon"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
    )),
    
    tabItems(
      # --- ABA: APRESENTAÇÃO ---
      tabItem(
        tabName = "sobre1",
        tabBox(
          id = "t1",
          width = 12,
          
          # Sub-Aba: Contexto
          tabPanel("Contexto", icon = icon("address-card"), fluidRow(
            column(
              width = 8,
              position = "left",
              tags$img(
                id = "foto1",
                src = "Frota_gpt1.png",
                controls = "controls",
                width = '100%',
                height = "auto"
              ),
              tags$br(),
              tags$a("Fonte: (Imagem gerada via gemini, 2026)"),
              align = "left"
            ),
            column(
              width = 4,
              tags$br(),
              tags$p(
                style = "text-align: justify; font-size: 11pt;",
                strong(
                  "Em consonância com o compromisso global firmado na Resolução A/RES/74/299 da ONU, que estabeleceu a Segunda Década de Ação pela Segurança Viária (2021-2030), o Brasil busca ativamente reduzir em pelo menos 50% o número de mortes e lesões graves no trânsito. Este esforço está alinhado à meta 3.6 dos Objetivos de Desenvolvimento Sustentável."
                )
              ),
              tags$p(
                style = "text-align: justify; font-size: 11pt;",
                strong(
                  "Neste contexto, o DETRAN-PA, em alinhamento à Política Nacional de Trânsito (Resolução CONTRAN Nº 514/2014), apresenta neste painel uma iniciativa estratégica: o Processo de Integração de Múltiplas Bases de Dados para qualificar as informações sobre vítimas fatais em acidentes de trânsito no estado."
                )
              ),
              tags$p(
                style = "text-align: justify; font-size: 11pt;",
                strong(
                  "O projeto, realizado em colaboração com os 144 municípios que compõem o Estado do Pará, permitiu a consolidação de informações vitais. Como um dos principais resultados, foi estruturada uma base de dados precisa da frota registrada, fundamental para a análise e o planejamento de ações de segurança viária."
                )
              )
            )
          )),
          
          # Sub-Aba: Linha de Base
          tabPanel(
            title = "Linha de Base",
            icon = icon("hospital"),
            fluidRow(column(
              width = 12,
              tags$div(
                style = "background-color: #f8f9fa; border-left: 5px solid #007bff; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                tags$p(
                  style = "text-align:justify;font-size:11pt;line-height:1.6;margin:0;",
                  "Compreender a dinâmica do trânsito exige analisar a ",
                  strong("frota de veículos"),
                  " sob a ótica de sua ",
                  strong("regularidade fiscal e regulatória"),
                  ". Este indicador é essencial para a gestão pública, a segurança viária e a sustentabilidade financeira do sistema de mobilidade."
                )
              )
            )),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("car-side"), " Frota Regular (Adimplente)"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify;font-size:11pt;line-height:1.6;",
                  "Representa os veículos com ",
                  strong("licenciamento ativo"),
                  ", em conformidade com a legislação vigente. Essa frota mantém a circulação legal e contribui diretamente para a arrecadação pública, que financia serviços essenciais e investimentos em infraestrutura."
                )
              ),
              bs4Dash::box(
                title = tagList(
                  icon("exclamation-triangle"),
                  " Frota com Pendências (Inadimplente)"
                ),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify;font-size:11pt;line-height:1.6;",
                  "Inclui os veículos com o ",
                  strong("licenciamento anual pendente"),
                  ", configurando uma situação de irregularidade. Além de restringir a circulação legal, essa condição indica uma ",
                  strong("evasão de recursos públicos"),
                  " que impacta diretamente o orçamento e a manutenção da infraestrutura viária."
                )
              )
            )
          ),
          
          # Sub-Aba: Alinhamento Técnico
          tabPanel(
            "Alinhamento Técnico",
            icon = icon("layer-group"),
            fluidRow(
              column(
                width = 12,
                tags$p(
                  style = "text-align: justify; font-size: 11pt; font-weight: 500;",
                  "Este trabalho é estruturado com base no ",
                  strong(
                    "Plano Nacional de Redução de Mortes e Lesões no Trânsito (PNATRANS)"
                  ),
                  ", instituído pela Lei nº 13.614/2018. Esse marco legal representa um avanço na governança da segurança viária, desafiando o Brasil a aprimorar seus sistemas de monitoramento, planejamento e resposta frente às causas de sinistros de trânsito. O alinhamento técnico que orienta nossas ações se apoia em três eixos complementares: ",
                  strong("Diagnóstico Nacional"),
                  ", ",
                  strong("Framework Técnico-Estatístico"),
                  " e ",
                  strong("Alinhamento Global"),
                  "."
                ),
                tags$a(
                  "Acesse o PNATRANS",
                  href = "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/anexo_i_pnatrans_2.pdf",
                  target = "_blank",
                  style = "font-weight: bold; color: #004085;"
                ),
                tags$br(),
                tags$br()
              ),
              fluidRow(
                bs4Dash::box(
                  title = tagList(icon("search"), "Diagnóstico Nacional"),
                  width = 4,
                  status = "primary",
                  elevation = 2,
                  solidHeader = TRUE,
                  tags$p(
                    style = "text-align: justify; font-size: 11pt;",
                    "Em 2020, foi instituída uma metodologia padronizada de coleta, processamento e validação dos dados de sinistros de trânsito em todo o território nacional. Essa padronização fortaleceu a comparabilidade das informações entre as Unidades Federativas e ampliou a capacidade do poder público em identificar padrões, fatores de risco e pontos críticos da malha viária. O Diagnóstico Nacional constitui a base empírica que orienta a formulação de políticas públicas de segurança viária baseadas em evidências, assegurando integridade, transparência e confiabilidade nas análises estatísticas e operacionais do sistema."
                  )
                ),
                bs4Dash::box(
                  title = tagList(icon("chart-line"), "Framework Técnico-Estatístico"),
                  width = 4,
                  status = "primary",
                  elevation = 2,
                  solidHeader = TRUE,
                  tags$p(
                    style = "text-align: justify; font-size: 11pt;",
                    "Adotando as diretrizes do Manual de Gestão do RENAEST, estabelecido pela Resolução CONTRAN nº 808/2020, este framework consolida um conjunto de normas e protocolos que asseguram a qualidade e consistência das informações sobre acidentes. A partir de técnicas estatísticas robustas, como análises de tendência, modelagem multivariada e indicadores de desempenho, o framework permite o tratamento de grandes volumes de dados e a geração de conhecimento estratégico. Isso garante a padronização nacional dos registros e o alinhamento metodológico com as metas do PNATRANS."
                  )
                ),
                bs4Dash::box(
                  title = tagList(icon("globe"), "Alinhamento Global"),
                  width = 4,
                  status = "primary",
                  elevation = 2,
                  solidHeader = TRUE,
                  tags$p(
                    style = "text-align: justify; font-size: 11pt;",
                    "O Brasil integra a agenda global de segurança viária ao alinhar suas metas com os ",
                    strong("Objetivos de Desenvolvimento Sustentável (ODS)"),
                    " da Organização das Nações Unidas, especialmente o ODS 3.6, que propõe reduzir pela metade o número de mortes e lesões no trânsito até 2030. Essa convergência entre políticas nacionais e compromissos internacionais fortalece a cooperação técnica, estimula o compartilhamento de boas práticas e reafirma o compromisso do país com a construção de um sistema viário mais seguro, humano e sustentável. Trata-se de uma visão integrada que conecta os esforços locais às metas globais de redução de sinistros."
                  )
                )
              )
            )
          ),
          
          # Sub-Aba: Ferramentas Utilizadas
          tabPanel(
            "Ferramentas",
            icon = icon("computer"),
            fluidRow(column(
              width = 12,
              tags$div(
                style = "background-color: #f8f9fa; border-left: 5px solid #6f42c1; padding: 18px; border-radius: 8px; margin-bottom: 25px;",
                tags$p(
                  style = "text-align:justify; font-size: 11pt; line-height:1.6;",
                  "Para o desenvolvimento deste anuário digital com dashboards interativos, foram utilizadas ferramentas de software livre, reconhecidas por sua robustez, escalabilidade e suporte à análise estatística e visualização de dados. O uso de ",
                  strong("R Project"),
                  " e seu Ambiente de Desenvolvimento Integrado ",
                  strong("RStudio"),
                  " garantiu flexibilidade, reprodutibilidade e compatibilidade com múltiplas bibliotecas e pacotes especializados."
                )
              )
            )),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("r-project"), "(R Project)"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$div(
                  style = "text-align:center;",
                  tags$img(
                    src = "R.jpg",
                    width = 180,
                    height = 180,
                    style = "margin-bottom:10px; border-radius:8px; box-shadow: 2px 2px 5px #888;"
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    "O projeto foi desenvolvido em ",
                    strong("R Project, versão 4.5"),
                    ", um ambiente de programação open source, sem custos de licença, permitindo ampla personalização, integração de pacotes e automação de processos analíticos (R Development Core Team, 2025)."
                  ),
                  tags$a(
                    "Download R Project",
                    href = "https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe",
                    target = "_blank",
                    style = "font-weight:bold; color:#004085;"
                  )
                )
              ),
              bs4Dash::box(
                title = tagList(icon("desktop"), "(RStudio)"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$div(
                  style = "text-align:center;",
                  tags$img(
                    src = "RStudio.png",
                    width = 180,
                    height = 180,
                    style = "margin-bottom:10px; border-radius:8px; box-shadow: 2px 2px 5px #888;"
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    "O desenvolvimento foi realizado no ",
                    strong("RStudio IDE, versão 1.4.1.7"),
                    ", que oferece um ambiente integrado de extração, transformação e carga (ETL) de dados, gerenciamento de pacotes e suporte a workflows reprodutíveis no Windows."
                  ),
                  tags$a(
                    "Download RStudio",
                    href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe",
                    target = "_blank",
                    style = "font-weight:bold; color:#004085;"
                  )
                )
              )
            )
          ),
          
          # Sub-Aba: Metodologia
          tabPanel(
            "Metodologia",
            icon = icon("book"),
            fluidRow(column(
              width = 12,
              tags$div(
                style = "background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 18px; border-radius: 8px; margin-bottom: 25px;",
                tags$p(
                  style = "text-align:justify; font-size: 11pt; line-height: 1.6;",
                  "A metodologia utilizada na construção deste painel combina princípios de ",
                  strong(
                    "engenharia de dados, estatística aplicada e visualização interativa"
                  ),
                  ". O processo compreende três etapas principais: ",
                  strong("gerenciamento da base de dados"),
                  ", ",
                  strong("processamento e integração dos dados"),
                  " e ",
                  strong("visualização e comunicação das informações"),
                  ". Essa arquitetura garante desempenho, escalabilidade e reprodutibilidade analítica."
                )
              )
            )),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("database"), "Banco de Dados ORACLE"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "A estrutura de armazenamento foi desenvolvida em um sistema de gerenciamento de banco de dados relacional ",
                  strong("Oracle"),
                  ", reconhecido por seu alto desempenho, escalabilidade e confiabilidade. O modelo de dados contempla múltiplas entidades e atributos referentes à ",
                  strong("frota veicular registrada no Estado do Pará"),
                  ", permitindo a integração de diferentes fontes de informação e a manutenção histórica das observações."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "O repositório central possibilita consultas otimizadas, controle de transações e rastreabilidade dos dados, aspectos essenciais para garantir consistência e governança da informação."
                )
              ),
              bs4Dash::box(
                title = tagList(icon("server"), "Processamento/Integração de Dados"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "A etapa de extração utiliza o pacote ",
                  strong("httr (Wickham, 2023)"),
                  " para realizar requisições à API oficial do DETRAN-PA, obtendo os dados em formato ",
                  strong("JSON"),
                  ". O tratamento e conversão para objetos R são realizados com o pacote ",
                  strong("jsonlite (Ooms et al., 2025)"),
                  ", que garante compatibilidade e performance na leitura de grandes volumes de dados."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "Após a manipulação e validação, os dados são integrados ao banco Oracle por meio do pacote ",
                  strong("DBI (Wickham & Müller, 2024)"),
                  ", assegurando transações consistentes e interoperabilidade entre R e o SGBD. Essa arquitetura de conexão permite atualização automatizada e escalabilidade horizontal do sistema."
                )
              ),
              bs4Dash::box(
                title = tagList(icon("chart-bar"), "Visualização/Comunicação dos Dados"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "As visualizações gráficas foram desenvolvidas com o pacote ",
                  strong("echarts4r (Cone, 2023)"),
                  ", que integra o poder da biblioteca JavaScript ",
                  strong("ECharts"),
                  " à flexibilidade do R, permitindo a criação de gráficos interativos, responsivos e personalizáveis."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "As tabelas dinâmicas utilizam o pacote ",
                  strong("DT (Xie et al., 2025)"),
                  ", oferecendo recursos de ordenação, filtragem, pesquisa e exportação em múltiplos formatos (.csv, .xls, .txt e .pdf)."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "Para análises espaciais, foram criados ",
                  strong("mapas coropléticos interativos"),
                  " com o pacote ",
                  strong("Leaflet (Chang et al., 2024)"),
                  ", representando a distribuição geográfica por município e região de integração do Estado do Pará. Esses mapas incluem filtros temporais e espaciais, permitindo uma exploração analítica detalhada."
                )
              )
            )
          ),
          
          # Sub-Aba: Créditos e Contatos
          tabPanel(
            "Créditos e Contatos",
            icon = icon("phone"),
            fluidRow(
              column(
                width = 4,
                position = "center",
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-size: 11pt",
                  strong(
                    "Este trabalho é uma iniciativa do Departamento de Trânsito do Estado do Pará. Abaixo estão as informações institucionais, diretrizes para citação e contato."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-size: 11pt",
                  strong("Instituição: Departamento de Trânsito do Estado do Pará")
                ),
                tags$p(style = "text-align: justify;font-size: 11pt", strong("Diretora-Geral: Renata Mirela Coelho")),
                tags$p(style = "text-align: justify;font-size: 11pt", strong("Endereço: Av. AUGUSTO Montenegro KM 03 S/N")),
                tags$p(style = "text-align: justify;font-size: 11pt", strong("CEP: 66635-918 - Parque Verde - Belém - Pará")),
                tags$br(),
                tags$p(style = "text-align: justify;font-size: 11pt", strong("Como Citar este Trabalho")),
                tags$p(
                  style = "text-align: justify;font-size: 11pt",
                  strong(
                    "Formato Sugerido: Departamento de Trânsito do Estado do Pará (DETRAN-PA). Anuário Estatístico de Acidentes de Trânsito, 2025 (LC/PUB.2025/1-P). Belém, 2025."
                  )
                ),
                tags$p(
                  style = "text-align: justify;font-size: 11pt",
                  strong(
                    "A reprodução total ou parcial deste material deve ser solicitada à Gerência de Análise e Estatística de Trânsito pelo e-mail: estatisticadetransito@detran.pa.gov.br. Instituições governamentais podem reproduzir o conteúdo sem autorização prévia, contanto que a fonte seja citada e o DETRAN-PA seja notificado."
                  )
                ),
                tags$br()
              ),
              column(
                width = 4,
                status = "primary",
                position = "center",
                tags$br(),
                leafletOutput("mapa")
              )
            )
          ),
          
          # Sub-Aba: Sugestões
          tabPanel(
            "Sugestões",
            icon = icon("lightbulb", class = "far"),
            fluidRow(column(
              width = 6,
              offset = 3,
              wellPanel(
                h3("Deixe sua Sugestão", align = "center"),
                p("Sua opinião é fundamental para a evolução deste trabalho.", align = "center"),
                hr(),
                selectInput(
                  "feedback_type",
                  "Tipo de Feedback:",
                  choices = c(
                    "",
                    "Sugestão de melhoria",
                    "Relatar inconsistência",
                    "Elogio",
                    "Outro"
                  )
                ),
                textAreaInput(
                  "feedback_text",
                  "Sua Mensagem:",
                  height = "150px",
                  placeholder = "Por favor, seja o mais detalhado possível."
                ),
                actionButton(
                  "submit_feedback",
                  "Enviar Feedback",
                  icon = icon("paper-plane"),
                  width = "100%",
                  class = "btn-primary"
                )
              )
            ))
          )
        )
      ),
      
      # --- ABA: GLOSSÁRIO DE TERMOS ---
      tabItem(
        tabName = "conceitos",
        tabBox(
          id = "t3",
          width = 12,
          
          # Sub-Aba: Documentação
          tabPanel(
            "Documentação",
            icon = icon("calendar"),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("file-invoice"), "Registro de Veículos"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                tags$div(
                  style = "text-align:center;",
                  tags$img(
                    src = "registro_veiculo.jfif",
                    width = 80,
                    height = 80,
                    style = "margin-bottom:10px; border-radius:8px; box-shadow:2px 2px 5px #888;"
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    "O registro é o procedimento inicial obrigatório para que um veículo automotor possa circular em vias públicas, vinculando-o a um proprietário e a uma jurisdição específica. "
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Órgão Competente:"),
                    " Realizado no DETRAN do estado ou Distrito Federal, com apoio das CIRETRANs nos municípios do interior."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Documento Expedido:"),
                    " Certificado de Registro de Veículo (CRV), atualmente emitido em formato digital (CRV-e)."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Condições para o Registro:"),
                    " Veículo deve possuir todos os equipamentos obrigatórios em perfeito estado, como cintos de segurança, extintor de incêndio (quando aplicável), retrovisores e sistema de iluminação."
                  )
                )
              ),
              bs4Dash::box(
                title = tagList(icon("redo-alt"), "Licenciamento Anual"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                tags$div(
                  style = "text-align:center;",
                  tags$img(
                    src = "licenciamento.jfif",
                    width = 80,
                    height = 80,
                    style = "margin-bottom:10px; border-radius:8px; box-shadow:2px 2px 5px #888;"
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    "O licenciamento anual renova a autorização de circulação, garantindo conformidade com normas de segurança, ambientais e fiscais."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Periodicidade:"),
                    " Deve ser realizado anualmente, geralmente baseado no final da placa do veículo."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Comprovação:"),
                    " Certificado de Registro e Licenciamento de Veículo (CRLV-e), disponível no aplicativo Carteira Digital de Trânsito (CDT)."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Porte Obrigatório:"),
                    " CRLV digital ou impresso deve estar sempre com o condutor; sua ausência configura infração gravíssima."
                  )
                )
              ),
              bs4Dash::box(
                title = tagList(icon("exchange-alt"), "Transferência de Propriedade"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                tags$div(
                  style = "text-align:center;",
                  tags$img(
                    src = "transferencia.jfif",
                    width = 80,
                    height = 80,
                    style = "margin-bottom:10px; border-radius:8px; box-shadow:2px 2px 5px #888;"
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    "Procedimentos necessários em caso de mudança de propriedade, alteração de características do veículo ou mudança de endereço do proprietário."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Transferência de Propriedade:"),
                    " Obrigatória em até 30 dias após venda, com novo CRV. Vendedor deve comunicar o DETRAN."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Alteração de Características:"),
                    " Modificações no veículo, como cor, motor ou instalação de GNV, devem ser autorizadas e registradas, gerando novos documentos."
                  ),
                  tags$p(
                    style = "text-align:justify; font-size:11pt; line-height:1.6;",
                    strong("Mudança de Domicílio:"),
                    " Proprietário deve atualizar o cadastro junto ao DETRAN em até 30 dias; mudança para outro município pode exigir novos documentos."
                  )
                )
              )
            )
          ),
          
          # Sub-Aba: Aspectos Fiscais e Bases (NOVO E SOLICITADO)
          tabPanel(
            "Aspectos Fiscais e Bases",
            icon = icon("scale-balanced"),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("file-invoice-dollar"), "IPVA vs. Licenciamento"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$div(
                  style = "font-size: 11pt; text-align:justify;",
                  tags$p(tagList(
                    icon("money-bill-wave", class = "text-primary"),
                    strong(" IPVA (Imposto sobre a Propriedade de Veículos Automotores):")
                  )),
                  tags$p(
                    "Tributo estadual de competência da Secretaria da Fazenda (SEFA). O inadimplemento acarreta juros, multas financeiras e inscrição em Dívida Ativa, mas ",
                    strong("não constitui infração de trânsito"),
                    " de forma isolada. Ou seja, não enseja remoção do veículo apenas pelo atraso deste imposto."
                  ),
                  tags$hr(),
                  tags$p(tagList(
                    icon("id-card", class = "text-primary"),
                    strong(" Licenciamento Anual (CRLV-e):")
                  )),
                  tags$p(
                    "Documento obrigatório que atesta que o veículo está apto a circular com segurança e em conformidade ambiental. A sua emissão ",
                    strong("exige a quitação prévia do IPVA e de todas as multas"),
                    " vinculadas ao veículo. Circular com o licenciamento vencido é ",
                    strong("infração gravíssima (Art. 230, V, do CTB)"),
                    ", sujeita a 7 pontos na CNH, multa e remoção do veículo ao pátio."
                  )
                )
              ),
              bs4Dash::box(
                title = tagList(icon("database"), "Ciclo de Vida e Sistemas"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$div(
                  style = "font-size: 11pt; text-align:justify;",
                  tags$p(tagList(
                    icon("calendar-check", class = "text-primary"),
                    strong(" Ano de Fabricação vs. Ano Modelo:")
                  )),
                  tags$p(
                    "O ",
                    strong("Ano de Fabricação"),
                    " reflete a montagem física do chassi, sendo a variável utilizada na estatística para calcular a idade real da frota e seu desgaste. O ",
                    strong("Ano Modelo"),
                    " é uma designação comercial da montadora, muitas vezes referente ao ano subsequente ao da fabricação."
                  ),
                  tags$hr(),
                  tags$p(tagList(
                    icon("server", class = "text-primary"),
                    strong(" RENAVAM (Registro Nacional de Veículos Automotores):")
                  )),
                  tags$p(
                    "Sistema mantido pela SENATRAN que centraliza todas as informações do veículo ao longo da sua vida útil, desde o faturamento na montadora até a sucata (baixa), garantindo a integração dos dados entre todos os DETRANs do país."
                  )
                )
              )
            )
          ),
          
          # Sub-Aba: Registro
          tabPanel(
            "Registro",
            icon = icon("clipboard-check"),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("car"), "Em Circulação"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "Veículo regularmente cadastrado no Estado do Pará, com situação ativa no Registro Nacional de Veículos Automotores (RENAVAM). Possui licenciamento anual válido, CRLV emitido e está autorizado a circular em vias públicas. Esta condição pressupõe que o veículo atenda às exigências de segurança, ambientais e documentais, garantindo circulação legal e segura."
                )
              ),
              bs4Dash::box(
                title = tagList(icon("ban"), "Registro Desativado"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  "Veículo com placa no padrão antigo (pré-Mercosul) sem movimentação no cadastro desde 2003. Essa situação pode decorrer de abandono, falta de licenciamento prolongado ou desinteresse do proprietário. O veículo não pode circular legalmente. Para regularização, é necessário reapresentação à vistoria e atualização cadastral no DETRAN, podendo haver cobrança retroativa de taxas e multas."
                )
              ),
              bs4Dash::box(
                title = tagList(icon("file-alt"), "Baixado / Transferido"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  strong("Baixado:"),
                  " Veículo declarado como sucata ou definitivamente desmontado, registrado oficialmente como inativo. A baixa pode ser voluntária ou compulsória (sinistro, destruição ou determinação administrativa/judicial). O número do chassi é mantido apenas para fins históricos, sem possibilidade de reativação. Circulação com veículo baixado é infração gravíssima (Art. 230/CTB)."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  strong("Transferido para outra UF:"),
                  " Veículo cuja propriedade ou domicílio foi transferido para outro Estado. O registro passa a ser responsabilidade do DETRAN da nova Unidade da Federação. A transferência deve ser concluída em até 30 dias, sob pena de multa (infração média - Art. 233/CTB)."
                )
              )
            )
          ),
          
          # Sub-Aba: Tipos de Veículos
          tabPanel(
            "Tipos",
            icon = icon("truck-pickup"),
            fluidRow(column(
              width = 12,
              tags$br(),
              tags$p(
                style = "text-align:justify; font-size:11pt; line-height:1.6; font-weight:500;",
                "Os tipos de veículos são definidos conforme o Anexo I do Regulamento do Código de Trânsito Brasileiro (CTB), a Convenção de Viena sobre Trânsito Viário e as classificações da Organização Mundial da Saúde. Essa categorização é essencial para a padronização dos registros, estatísticas e políticas públicas de mobilidade e segurança viária."
              )
            )),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("users"), "Veículos de Passageiros"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("car-side", class = "text-primary"),
                  strong(" 1º) Automóvel:"),
                  " destinado ao transporte de passageiros, com capacidade para até oito pessoas, além do condutor."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("van-shuttle", class = "text-primary"),
                  strong(" 2º) Micro-ônibus:"),
                  " veículo automotor de transporte coletivo com capacidade para até 20 passageiros (incluindo vans)."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("bus", class = "text-primary"),
                  strong(" 3º) Ônibus:"),
                  " veículo automotor de transporte coletivo com capacidade para mais de 20 passageiros, podendo ser adaptado para maior conforto."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("car", class = "text-primary"),
                  strong(" 4º) Camioneta:"),
                  " veículo misto destinado ao transporte de passageiros e carga no mesmo compartimento."
                )
              ),
              bs4Dash::box(
                title = tagList(icon("truck"), "Veículos de Carga"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("truck", class = "text-primary"),
                  strong(" 5º) Caminhão:"),
                  " destinado ao transporte de carga com peso bruto total igual ou superior a 3.500 kg, podendo tracionar outro veículo."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("truck-pickup", class = "text-primary"),
                  strong(" 6º) Caminhonete:"),
                  " destinada ao transporte de carga com peso bruto total de até 3.500 kg."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("car-side", class = "text-primary"),
                  strong(" 7º) Quadriciclo:"),
                  " veículo automotor de quatro rodas, com cabine ou não, destinado ao transporte de passageiros (massa até 450 kg) ou cargas (até 600 kg)."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("tractor", class = "text-primary"),
                  strong(" 8º) Veículo Especial:"),
                  " veículo de características diferenciadas, projetado para funções específicas, como ambulâncias, guinchos, tratores ou veículos de coleção."
                )
              ),
              bs4Dash::box(
                title = tagList(icon("motorcycle"), "Veículos (Duas e Três Rodas)"),
                width = 4,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("bicycle", class = "text-primary"),
                  strong(" 9º) Bicicleta:"),
                  " veículo de propulsão humana com duas rodas, dirigido em posição montada, sem motor auxiliar."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("gauge-high", class = "text-primary"),
                  strong(" 10º) Ciclomotor:"),
                  " veículo de duas ou três rodas com motor de combustão até 50 cm³ e velocidade máxima de 50 km/h."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("motorcycle", class = "text-primary"),
                  strong(" 11º) Motocicleta:"),
                  " veículo automotor de duas rodas dirigido em posição montada."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("motorcycle", class = "text-primary"),
                  strong(" 12º) Motoneta:"),
                  " veículo automotor de duas rodas, dirigido em posição sentada."
                ),
                tags$p(
                  style = "text-align:justify; font-size:11pt; line-height:1.6;",
                  icon("car-rear", class = "text-primary"),
                  strong(" 13º) Triciclo:"),
                  " veículo automotor de três rodas com ou sem cabine, dirigido em posição sentada ou montada, sem as características de ciclomotor."
                )
              )
            )
          ),
          
          # Sub-Aba: Espécie dos Veículos
          tabPanel(
            "Espécies",
            icon = icon("tags"),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("car"), "Espécies de Veículos - Parte 1"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify;font-size: 11pt",
                  strong(
                    "Este item relaciona as Espécies de veículos definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde."
                  )
                ),
                tags$div(
                  style = "font-size: 11pt; text-align:justify;",
                  tags$p(
                    tagList(
                      icon("user", class = "text-primary"),
                      strong(" PASSAGEIRO:"),
                      " designa veículo destinado ao transporte de pessoas e suas bagagens;"
                    )
                  ),
                  tags$p(
                    tagList(
                      icon("truck", class = "text-primary"),
                      strong(" CARGA:"),
                      " designa veículos destinado ao transporte de carga, podendo transportar dois passageiros, exclusive o condutor;"
                    )
                  ),
                  tags$p(
                    tagList(
                      icon("truck-pickup", class = "text-primary"),
                      strong(" MISTO:"),
                      " designa veículos automotores destinado ao transporte simultâneo de carga e passageiro;"
                    )
                  ),
                  tags$p(
                    tagList(
                      icon("flag-checkered", class = "text-primary"),
                      strong(" CORRIDA/COMPETIÇÃO:"),
                      " designa veículos destinado ao uso esportivo de acordo com artigo 110 do CTB, somente poderão circular nas vias com autorização da autoridade de trânsito;"
                    )
                  )
                )
              ),
              bs4Dash::box(
                title = tagList(icon("truck-monster"), "Espécies de Veículos - Parte 2"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$div(style = "font-size: 11pt; text-align:justify;", tags$p(
                  tagList(
                    icon("tractor", class = "text-primary"),
                    strong(" TRAÇÃO:"),
                    " designa veículo automotor destinado a tracionar outro veículo;"
                  )
                ), tags$p(
                  tagList(
                    icon("tools", class = "text-primary"),
                    strong(" ESPECIAL:"),
                    " designa veículos cujas características ou finalidade não permitem enquadrá-lo em uma das demais espécies;"
                  )
                ), tags$p(
                  tagList(
                    icon("trophy", class = "text-primary"),
                    strong(" COLEÇÃO:"),
                    " designa veículos que, mesmo tendo sido fabricados há mais de trinta anos, conservam suas características originais de fabricação e possuem valor histórico próprio;"
                  )
                ))
              )
            )
          ),
          
          # Sub-Aba: Categoria de Veículos
          tabPanel(
            "Categorias",
            icon = icon("layer-group"),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("car-side"), "Categorias de Veículos - Parte 1"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify;font-size: 11pt",
                  strong(
                    "Este item apresenta as Categorias de veículos definidas no Anexo I do Regulamento do Código de Trânsito Brasileiro e Convenção de Trânsito Viário de Viena, conforme padrões internacionais da OMS."
                  )
                ),
                tags$div(
                  style = "font-size: 11pt; text-align:justify;",
                  tags$p(
                    tagList(
                      icon("taxi", class = "text-primary"),
                      strong(" ALUGUEL:"),
                      " Veículos destinados ao uso comercial, prestando serviços a terceiros, como táxis e aplicativos de transporte."
                    )
                  ),
                  tags$p(
                    tagList(
                      icon("graduation-cap", class = "text-primary"),
                      strong(" APRENDIZAGEM:"),
                      " Veículos utilizados em centros de formação de condutores para ensino de direção."
                    )
                  ),
                  tags$p(
                    tagList(
                      icon("user-tie", class = "text-primary"),
                      strong(" CHEFE DE MISSÃO DIPLOMÁTICA:"),
                      " Veículos destinados a Chefes de Missão Diplomática e Delegações Especiais."
                    )
                  ),
                  tags$p(
                    tagList(
                      icon("user-shield", class = "text-primary"),
                      strong(" CONSULARES DE CARREIRA:"),
                      " Veículos pertencentes a Repartições Consulares de Carreira e a agentes consulares de carreira."
                    )
                  )
                )
              ),
              bs4Dash::box(
                title = tagList(icon("car"), "Categorias de Veículos - Parte 2"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$div(style = "font-size: 11pt; text-align:justify;", tags$p(
                  tagList(
                    icon("industry", class = "text-primary"),
                    strong(" FABRICANTE:"),
                    " Veículo em fase de desenvolvimento ou teste pelo fabricante."
                  )
                ), tags$p(
                  tagList(
                    icon("university", class = "text-primary"),
                    strong(" OFICIAL:"),
                    " Veículos de propriedade de órgãos públicos federais, estaduais ou municipais."
                  )
                ), tags$p(
                  tagList(
                    icon("user", class = "text-primary"),
                    strong(" PARTICULAR:"),
                    " Veículos de propriedade de pessoas físicas ou jurídicas que não se enquadram nas demais categorias."
                  )
                ))
              )
            )
          ),
          
          # Sub-Aba: Combustível dos Veículos
          tabPanel(
            "Combustível",
            icon = icon("gas-pump"),
            fluidRow(
              bs4Dash::box(
                title = tagList(icon("tint"), "Combustíveis - Parte 1"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$p(
                  style = "text-align:justify;font-size: 11pt",
                  strong(
                    "Este item relaciona os Tipos de Combustível dos veículos definidos no Anexo I do Regulamento do Código de Trânsito Brasileiro e Convenção de Trânsito Viário de Viena, conforme padrões da OMS."
                  )
                ),
                tags$div(style = "font-size: 11pt; text-align:justify;", tags$p(
                  tagList(
                    icon("droplet", class = "text-primary"),
                    strong(" ÁLCOOL/ETANOL:"),
                    " Biocombustível obtido principalmente da cana-de-açúcar. O etanol é utilizado em motores de combustão interna com ignição por centelha, podendo ser anidro (mistura na gasolina) ou hidratado (comercializado como combustível acabado). Especificações segundo Resolução ANP nº 907/2022."
                  )
                ), tags$p(
                  tagList(
                    icon("oil-can", class = "text-primary"),
                    strong(" DIESEL:"),
                    " Combustível derivado do refino do petróleo bruto, em homenagem ao engenheiro Rudolf Diesel. Pouco inflamável, medianamente tóxico e pouco volátil, é usado principalmente em veículos pesados e transporte de carga."
                  )
                ))
              ),
              bs4Dash::box(
                title = tagList(icon("gas-pump"), "Combustíveis - Parte 2"),
                width = 6,
                status = "primary",
                elevation = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                tags$div(style = "font-size: 11pt; text-align:justify;", tags$p(
                  tagList(
                    icon("fire", class = "text-primary"),
                    strong(" GASOLINA:"),
                    " Combustível derivado do petróleo bruto, constituído basicamente por hidrocarbonetos. No Brasil, existem três tipos automotivos (A, B e C), sendo o tipo B de uso exclusivo das Forças Armadas."
                  )
                ), tags$p(
                  tagList(
                    icon("bolt", class = "text-primary"),
                    strong(" ÁLCOOL/GASOLINA (Flex):"),
                    " Veículo equipado com motor de combustão interna capaz de utilizar mais de um tipo de combustível simultaneamente, misturados no mesmo tanque, queimando-os na câmara de combustão."
                  )
                ))
              )
            )
          ),
          
          # Sub-Aba: Convenções
          tabPanel("Convenções", icon = icon("landmark"), fluidRow(
            bs4Dash::box(
              title = tagList(icon("scroll", class = "text-primary"), "Convenções e Notações"),
              width = 12,
              status = "primary",
              elevation = 2,
              solidHeader = TRUE,
              collapsible = TRUE,
              maximizable = TRUE,
              tags$p(
                style = "text-align:justify;font-size: 11pt",
                strong(
                  "O Sistema segue a orientação das Normas da ABNT - NBR 10697, garantindo padronização e consistência dos dados."
                )
              ),
              tags$div(
                style = "font-size: 11pt; text-align:justify;",
                tags$p(
                  strong("(-):"),
                  " Dado numérico igual a zero não resultante de arredondamento."
                ),
                tags$p(
                  strong("NI:"),
                  " Referem-se aos acidentes, vítimas ou atributos não informados."
                ),
                tags$p(strong("…:"), " Dado numérico não disponível.")
              )
            )
          )),
          tabPanel("Previsões", icon = icon("chart-line"), fluidRow(
            bs4Dash::box(
              title = tagList(icon("scroll", class = "text-primary"), "Previsões e Modelagem"),
              width = 12,
              status = "primary",
              elevation = 2,
              solidHeader = TRUE,
              collapsible = TRUE,
              maximizable = TRUE,
              tags$p(
                style = "text-align:justify;font-size: 11pt",
                strong(
                  "O Sistema segue a orientação das Normas da ABNT - NBR 10697, garantindo padronização e consistência dos dados."
                )
              ),
              tags$div(
                style = "font-size: 11pt; text-align:justify;",
                tags$p(
                  strong("(-):"),
                  " Dado numérico igual a zero não resultante de arredondamento."
                ),
                tags$p(
                  strong("NI:"),
                  " Referem-se aos acidentes, vítimas ou atributos não informados."
                ),
                tags$p(strong("…:"), " Dado numérico não disponível.")
              )
            )
          ))
        )
      ),
      
      # --- ABAS MODULARES ---
      tabItem(tabName = "visao_geral", fluidPage(total_ui("geral"))),
      tabItem(tabName = "carac_loc", fluidPage(loc_ui("loc"))),
      tabItem(tabName = "carac_muni", fluidPage(muni_ui("muni"))),
      tabItem(tabName = "carac_tipo", fluidPage(tipo_ui("tipo"))),
      tabItem(tabName = "pa", fluidPage(frotapa_ui("frotapa"))),
      tabItem(tabName = "muni", fluidPage(frotamuni_ui("frotamuni"))),
      tabItem(tabName = "previsoes", fluidPage(previsoes_ui("previsoes"))),
      tabItem(tabName = "modelagem", fluidPage(modelagem_ui("modelagem"))),
      tabItem(tabName = "previsoes_todos",fluidPage(previsoes_todos_ui("previsoes_todos"))),
      tabItem(tabName = "previsoes_multivariada", fluidPage(previsoes_multivariada_ui("previsoes_multivariada"))),
      # --- ABA: RELATÓRIO PDF ---
      tabItem(tabName = "relatorio_pdf", fluidPage(mod_pdf_ui("pdf_ui"))),
      
      # --- ABA: SOBRE ---
      tabItem(tabName = "sobre", fluidPage(
        panel(
          title = "Sobre R, ShinyR e bs4dash",
          width = 6,
          status = "primary",
          elevation = 2,
          solidHeader = TRUE,
          align = "center",
          justify = "justify",
          fluidRow(column(
            4,
            div(
              style = "text-align: justify; text-justify: inter-word;",
              HTML(
                "<h2>Sobre as tecnologias usadas</h2><p>R é uma linguagem de programação estatística amplamente utilizada. ShinyR é um pacote para criar aplicativos web interativos usando R. bs4dash é uma extensão do shinydashboard que utiliza o Bootstrap 4. Ele fornece uma variedade de componentes e funcionalidades para criar dashboards modernos e responsivos em R. O pacote é particularmente útil para a criação de aplicações web interativas e visualmente atraentes, aproveitando as capacidades do Bootstrap 4 para o design de interface do usuário. Além disso, a integração com o Shiny facilita a construção de dashboards dinâmicos e interativos que respondem às interações do usuário em tempo real. Ao combinar R, ShinyR e bs4dash, os desenvolvedores têm à disposição uma poderosa combinação para a criação de aplicações web estatísticas avançadas e dashboards envolventes.</p>"
              )
            )
          ), column(
            8, align = "center", img(src = "Dashboard.gif", height = 350)
          )),
          footer = fluidRow(
            column(4, align = "center", tags$img(src = "R_logo.svg.png", height = 100)),
            column(4, align = "center", tags$img(src = "hex-shiny.png", height = 100)),
            column(4, align = "center", tags$img(src = "bs4dash.png", height = 100))
          )
        )
      ))
    )
  ),
  
  # --- CONTROLBAR (Barra direita oculta) ---
  controlbar = dashboardControlbar(
    id = "controlebar",
    skin = "light",
    pinned = FALSE,
    disable = FALSE,
    width = 250,
    collapsed = TRUE,
    overlay = FALSE,
    controlbarMenu(
      id = "controlebarmenu",
      controlbarItem(
        title = "Instagram",
        a(
          href = "https://www.instagram.com/detranpa_",
          icon("instagram"),
          "Instagram",
          target = "_blank"
        )
      ),
      controlbarItem(
        title = "Facebook",
        a(
          href = "https://www.facebook.com/detranPARA",
          icon("facebook"),
          "Facebook",
          target = "_blank"
        )
      ),
      controlbarItem(
        title = "Twitter",
        a(
          href = "https://twitter.com/DETRAN_PA",
          icon("twitter"),
          "Twitter",
          target = "_blank"
        )
      ),
      controlbarItem(
        title = "Suporte",
        a(href = "https://github.com/MarioDhiego", icon("github"), "Suporte", target = "_blank")
      )
    )
  ),
  
  # --- RODAPÉ ---
  footer = dashboardFooter(
    left = tags$b("DETRAN-PA"),
    right = tags$b("BELÉM, 2026 v.3")
  )
)

#==============================================================================#
# 2. LÓGICA DO SERVIDOR (SERVER)
#==============================================================================#

server <- function(input, output, session) {
  # -------------- Lógica para salvar o feedback --------------------------------#
  # Define o nome do arquivo onde o feedback será salvo
  feedback_file <- here::here("feedback_data.csv")
  
  # Quando o botão for clicado
  observeEvent(input$submit_feedback, {
    # Validação básica para não enviar em branco
    req(input$feedback_type, input$feedback_text)
    
    # Cria um data frame com o novo feedback
    new_feedback <- data.frame(
      timestamp = Sys.time(),
      type = input$feedback_type,
      message = input$feedback_text,
      stringsAsFactors = FALSE
    )
    
    # Tenta ler o arquivo existente ou cria um novo
    if (file.exists(feedback_file)) {
      # Anexa o novo feedback ao arquivo existente
      write.table(
        new_feedback,
        feedback_file,
        append = TRUE,
        sep = ",",
        col.names = FALSE,
        row.names = FALSE
      )
    } else {
      # Cria o arquivo com cabeçalho
      write.csv(new_feedback, feedback_file, row.names = FALSE)
    }
    
    # Mensagem de sucesso para o usuário!
    showModal(
      modalDialog(
        title = "OBRIGADO!",
        "Seu Feedback foi Enviado com Sucesso e nos ajudará a melhorar este Painel.",
        easyClose = TRUE,
        footer = NULL
      )
    )
    
    # Limpa os campos do formulário após o envio
    updateSelectInput(session, "feedback_type", selected = "")
    updateTextAreaInput(session, "feedback_text", value = "")
  })
  
  #-----------------------------------------------------------------------------#
  
  # --- Dados para o Mapa ---
  detran_data <- data.frame(
    lat = -1.37843,
    lon = -48.44034,
    popup_info = paste(
      "<strong>DETRAN/PA - Sede</strong>",
      "Av. Augusto Montenegro, Km 03, S/N",
      "Parque Verde, Belém - PA",
      "CEP: 66635-918",
      sep = "<br/>"
    )
  )
  
  # --- Renderização do Mapa Leaflet ---
  output$mapa <- renderLeaflet({
    leaflet(data = detran_data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(
        lng = ~ lon,
        lat = ~ lat,
        popup = ~ popup_info,
        label = "Departamento de Trânsito do Estado do Pará"
      ) %>%
      setView(lng = detran_data$lon,
              lat = detran_data$lat,
              zoom = 16)
  })
  
  #==============================================================================#
  # -------------- Chamada dos Módulos Funcionais -------------- #
  total_Server("geral")
  loc_Server("loc")
  muni_Server("muni")
  tipo_Server("tipo")
  frotapa_Server("frotapa")
  frotamuni_Server("frotamuni")
  previsoes_server("previsoes")
  modelagem_server("modelagem")
  previsoes_todos_server("previsoes_todos")
  previsoes_multivariada_server("previsoes_multivariada")
  mod_pdf_server("pdf_ui")
}

#==============================================================================#
# 3. CHAMADA DO APLICATIVO
#==============================================================================#
shinyApp(ui, server)