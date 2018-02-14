library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    dataTableImportWidget("tableImport1"),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  # specifying fileLocation as "server" or "both" to provide users the option to import a data table from server file
  importedData <- importDataTable("tableImport1", fileLocation = "both")
  
  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)