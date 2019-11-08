library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    dataTableImportWidget("tableImport1", "Choose a file to import (must be comma or tab separated)"),
    tags$hr(),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  importedData <- importDataTable("tableImport1", C_FILE_LOCATION_LOCAL)

  output$debug <- renderPrint({
    importedData()
  })
}

shinyApp(ui = ui, server = server)
