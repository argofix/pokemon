library(shiny)

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      # selectInput("dataset", "Choose a dataset:",
      #             choices = c("rock", "pressure", "cars")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("table")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  
   dataset <- c(rock)
  
  
  # Table of selected dataset ----
  output$table <- renderTable({
    dataset

  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset, file, row.names = FALSE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)