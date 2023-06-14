#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tibble)
library(readr)
library(DT)
library(shinyWidgets)
library(htmltools) 


# Leitura e processamento básioco dos dados

dados <- readRDS(here::here("dados/pkmn.rds"))
dados <- pokemon::pokemon_ptbr

geracoes <- dados %>%
  drop_na(id_geracao)

geracoes <- unique(geracoes$id_geracao)
names(geracoes) <- c("Geração 1", "Geração 2", "Geração 3", "Geração 4", "Geração 5",
                     "Geração 6", "Geração 7")
# Logo

title_logo <- tags$a(href='https://pt.wikipedia.org/wiki/Pok%C3%A9mon',
                     tags$imag(src="logo_pokemon.png", height = '50', width = '180'))

# Ajustes dos dados para a tabela

dados_tab <- dados %>% 
  select(-id, -cor_1, -cor_2, -url_icone) %>% 
  select(pokemon, id_especie, id_geracao, altura, peso, 
         exp_base, tipo_1, tipo_2, hp, ataque, defesa, 
         ataque_especial, velocidade, grupo_ovo_1, grupo_ovo_2, url_imagem) 


# Construção da interface com o usuário (inputs)

ui <- dashboardPage(
  dashboardHeader(title = title_logo), # título geral
  dashboardSidebar( # construção da barra lateral
    sidebarMenu( # construção do menu da barra lateral
      
      menuItem( # primeiro item do menu
        text = "Visão Geral",
        tabName = "visao_geral"
      ),
      
      menuItem( # segundo item do menu
        text = "Dados - Visualizar/Baixar",
        tabName = "dados"
      ),
      
      menuItem(  # terceiro item do menu
        text = "Sobre os Pokemons",
        tabName = "sobre"
      ),
      
      hr(),
      
      # Construindo a caixa de seleção
      
      checkboxGroupInput(inputId = "geracoes",
                         label = h4("Selecionar geração"),
                         choiceNames = c("Geração 1", "Geração 2", "Geração 3",
                                         "Geração 4", "Geração 5", "Geração 6",
                                         "Geração 7"),
                         choiceValues = c(1, 2, 3, 4, 5, 6, 7),
                         selected = 1
      ),
      
      actionButton(inputId = "Incluir", # Botão para incluir gerações
                   label = "Incluir",
                   class = "btn-light"
      )
    ) 
  ),  
  
  
  # Construção dos elementos do corpo da dashboard
  
  dashboardBody(
    
    # Alterando a cor da barra de navegação
    
    tags$head(tags$style(HTML(' 
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #4c51bd;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #4c51bd;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #4c51bd;
                                }
                                '))),
    
    tabItems( 
      tabItem( # Construção da página "Sobre"
        tabName = "sobre",
        titlePanel("Sobre"),
        box( # construção da caixa para o texto sobre os Pokemons
          title = "Saiba mais sobre os Pokemons", width = NULL,
          includeMarkdown("sobre.md")
        )
      ),
      tabItem( # Construção da página "Visão geral"
        tabName = "visao_geral",
        titlePanel("Visão Geral"),
        fluidRow(
          column(
            width = 6,
            box( # construção da caixa para o gráfico
              title = "Quantidade por tipo", width = NULL,
              plotOutput("geracoes", height = "630px")
            )
          ),
          column(
            width = 6,
            fluidRow(
              column(
                width = 8,
                box( # construção da caixa para as caixas de valores
                  title = "Pokemons mais destacados", width = NULL,
                  infoBoxOutput("mais_pesado", width = 12),
                  infoBoxOutput("mais_alto", width = 12),
                  infoBoxOutput("maior_hp", width = 12),
                  infoBoxOutput("maior_ataque", width = 12),
                  infoBoxOutput("maior_defesa", width = 12),
                  infoBoxOutput("mais_rapido", width = 12)
                )
              ),
              column( #Inserção das imagens
                width = 4,
                uiOutput("imagem1"),
                uiOutput("imagem2"),
                uiOutput("imagem3"),
                uiOutput("imagem4"),
                uiOutput("imagem5"),
                uiOutput("imagem6")
              )
            )
          )
        )
      ),
      tabItem( # Construção da página "Dados"
        tabName = "dados",
        titlePanel("Dados"),
        fluidRow(
          width = 12,
          box( # construção da caixa para a tabela de dados
            title = "Tabela geral de dados", width = 12,
            DT::dataTableOutput("dados"))
        )
      )
    )
  )
)




server <- function(input, output, session) {
  
  
  # Plotando o gráfico principal
  
  
  grafico_geral <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        group_by(tipo_1) %>% 
        summarise(count = n())
    )
    
  })
  
  indicadores_gerais <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        summarise(mais_pesado = max(peso),
                  mais_alto = max(altura),
                  maior_hp = max(hp),
                  maior_ataque = max(ataque),
                  maior_defesa = max(defesa),
                  mais_rapido = max(velocidade))
    )
    
  }) 
  
  imagens_mais_pesado <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        top_n(4, peso) 
    )
    
  }) 
  
  imagens_mais_alto <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        top_n(1, altura) 
    )
    
  }) 
  
  imagens_maior_hp <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        top_n(1, hp) 
    )
    
  }) 
  
  imagens_maior_ataque <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        top_n(1, ataque) 
    )
    
  }) 
  
  imagens_maior_defesa <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        top_n(1, defesa) 
    )
    
  }) 
  
  imagens_mais_rapido <- eventReactive(input$Incluir, ignoreNULL = FALSE, {
    tibble::tibble(
      dados %>% 
        filter(id_geracao == input$geracoes) %>%
        top_n(1, velocidade) 
    )
    
  }) 
  
  output$geracoes <- renderPlot({
    
    grafico_geral() %>% 
      
      ggplot(aes(x = fct_reorder(tipo_1, count, .desc = F), y = count, fill = as.factor(tipo_1))) +
      geom_bar(stat="identity") +
      coord_flip() +
      theme_minimal() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
      xlab("Tipo") +
      ylab("Quantidade") 
    
  })
  
  
  # Plotando as caixas de valores 
  
  output$mais_pesado <- renderInfoBox({
    infoBox(
      "Mais Pesado",
      paste0(imagens_mais_pesado()[1, 2]),
      paste0(indicadores_gerais()[1, 1], " kg"),
      icon = icon("scale", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
  
  
  output$mais_alto <- renderInfoBox({
    infoBox(
      "Mais alto",
      paste0(imagens_mais_alto()[1, 2]),
      paste0(indicadores_gerais()[1, 2], " cm"),
      icon = icon("arrow-up", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$maior_hp <- renderInfoBox({
    infoBox(
      "Maior HP",
      paste0(imagens_maior_hp()[1, 2]),
      paste0(indicadores_gerais()[1, 3]),
      icon = icon("heart", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$maior_ataque <- renderInfoBox({
    infoBox(
      "Maior Ataque",
      paste0(imagens_maior_ataque()[1, 2]),
      paste0(indicadores_gerais()[1, 4]),
      icon = icon("fire", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$maior_defesa <- renderInfoBox({
    infoBox(
      "Maior Defesa",
      paste0(imagens_maior_defesa()[1, 2]),
      paste0(indicadores_gerais()[1, 5]),
      icon = icon("flag", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$mais_rapido <- renderInfoBox({
    infoBox(
      "Mais Rápido",
      paste0(imagens_mais_rapido()[1, 2]),
      paste0(indicadores_gerais()[1, 6]),
      icon = icon("forward", lib = "glyphicon"),
      color = "red",
      fill = TRUE
    )
  })
  
  
  # Plotando as imagens dos Pokemons selecionados
  
  output$imagem1 <- renderUI({
    
    url <- imagens_mais_pesado()[1, 21]
    img(
      src = url,
      width = "45%"
    )
    
  })
  
  output$imagem2 <- renderUI({
    
    url <- imagens_mais_alto()[1, 21]
    img(
      src = url,
      width = "45%"
    )
    
  })
  
  output$imagem3 <- renderUI({
    
    url <- imagens_maior_hp()[1, 21]
    img(
      src = url,
      width = "45%"
    )
    
  })
  
  output$imagem4 <- renderUI({
    
    url <- imagens_maior_ataque()[1, 21]
    img(
      src = url,
      width = "45%"
    )
    
  })
  
  output$imagem5 <- renderUI({
    
    url <- imagens_maior_defesa()[1, 21]
    img(
      src = url,
      width = "45%"
    )
    
  })
  
  output$imagem6 <- renderUI({
    
    url <- imagens_mais_rapido()[1, 21]
    img(
      src = url,
      width = "45%"
    )
    
  })
 
  # Plotando a tabela da página de dados 
  
  output$dados  <- DT::renderDataTable({
    datatable(
      dados_tab,
      #caption = 'Tabela 1: Dados gerais sobre os Pokemons.',
      colnames = c('Pokemon', 'Espécie ID', 'Geração ID', 
                   'Altura', 'Peso', 'Expo_base', 'Tipo 1', 
                   'Tipo 2', 'HP', 'Ataque', 'Defesa', 'Ataque Especial', 
                   'Velocidade', 'Grupo Ovo 1', 'Grupo Ovo 2', 'URL_Imagem'),
      rownames = TRUE,
      filter = 'top',
        options = list(
        fixedColumns = TRUE,
        pageLength = 15,
        
        
        autoWidth = TRUE,
        ordering = FALSE,
        scrollX = TRUE,
        #scroller = TRUE,
        
        dom = 'ptB',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      class = "display", #if you want to modify via .css
      extensions = "Buttons"
    )
    
  })
  
  
}


shinyApp(ui = ui, server = server)
