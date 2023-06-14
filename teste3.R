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

# Leitura dos dados
dados <- pokemon::pokemon_ptbr

# Construção da interface com o usuário (inputs)
ui <- dashboardPage(
  dashboardHeader(title = "Your App Title"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Visualizar dados",
        tabName = "dados",
        menuSubItem(
          text = "Download CSV",
          icon = icon("download"),
          actionButton("downloadData", "Download Data", class = "btn-primary")
        )
      )
    )
  ),
  dashboardBody(
    tabItem(
      tabName = "dados",
      titlePanel("Dados"),
      fluidRow(
        width = 12,
        box(
          title = "Tabela geral de dados",
          width = NULL,
          DTOutput("dadosTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$dadosTable <- renderDT({
    datatable(dados, 
              options = list(
                scrollX = TRUE,
                pageLength = 18
              )
    )
  })
  
  observeEvent(input$downloadData, {
    data <- isolate(dados)
    write.csv(data, "dados.csv", row.names = FALSE)
    runjs("var link = document.createElement('a');
           link.href = 'dados.csv';
           link.download = 'dados.csv';
           link.click();")
  })
}

shinyApp(ui = ui, server = server)