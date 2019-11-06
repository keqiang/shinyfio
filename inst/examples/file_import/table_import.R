library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    dataTableImportWidget("tableImport1", "Choose a file to import (must be comma or tab separated)"),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  importedData <- importDataTable("tableImport1", serverRootDirectories = c("wd" = "."))
  
  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)
