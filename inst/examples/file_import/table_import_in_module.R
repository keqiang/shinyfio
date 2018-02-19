library(shiny)
library(shinywidgets)

testTableImportInModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableImportWidget(ns("tableImport1")),
    verbatimTextOutput(ns("debug"))
  )
}

testTableImportInModule <- function(input, output, session) {
  importedData <- importDataTable("tableImport1")
  
  output$debug <- renderPrint({
    print(importedData())
  })
}


ui <- fluidPage(
  wellPanel(
    testTableImportInModuleUI("test")
  )
)

server <- function(input, output) {
  callModule(testTableImportInModule, "test")
}

shinyApp(ui = ui, server = server)