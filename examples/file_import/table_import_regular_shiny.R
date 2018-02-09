library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    tableImportUI("tableImport1"),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  importedData <- callModule(tableImport, "tableImport1")
  
  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)
