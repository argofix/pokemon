library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyWidgets)


# Leitura dos dados

dados <- readRDS(here::here("dados/pkmn.rds"))
dados <- pokemon::pokemon_ptbr


# Construção da interface com o usuário (inputs)

ui <- dashboardPage(
  dashboardHeader(title = "Teste download button"), # título geral
  dashboardSidebar( # construção da barra lateral
    sidebarMenu( # construção do menu da barra lateral
      
      
      menuItem( # segundo item do menu
        text = "Visualizar dados",
        tabName = "dados",
        menuSubItem(
          text = "Download CSV",
          icon = icon("download"),
          downloadButton("downloadData", "Download Data")
        )
      )
    )
  ),
     
      
     
  # Construção dos elementos do corpo da dashboard
  
  dashboardBody(
    
    
      tabItem( # Construção da página "Dados"
        tabName = "dados",
        titlePanel("Dados"),
        fluidRow(
          width = 12,
          box( # construção da caixa para a tabela de dados
            title = "Tabela geral de dados", width = NULL,
            # dataTableOutput("dados"),
            DT::dataTableOutput("dados"),
            #tableOutput("table")
          )
        )
      )
    )
  )



server <- function(input, output, session) {
  
  
  
  output$dados <- renderDataTable({
    
    DT::datatable(dados, 
                  options = list(
                    scrollX = TRUE,
                    pageLength = 18
                  )
    )
    
  })

  output$dados  <- DT::renderDataTable({
    datatable(
      dados,
      rownames = TRUE,
      options = list(
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = FALSE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      class = "display", #if you want to modify via .css
      extensions = "Buttons"
    )
  
})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dados", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dados, file, row.names = FALSE)
    }  
  )
}


shinyApp(ui = ui, server = server)