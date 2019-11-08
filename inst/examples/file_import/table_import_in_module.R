library(shiny)
library(shinyfio)

testTableImportInModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableImportWidget(ns("tableImport1")),
    tags$hr(),
    verbatimTextOutput(ns("debug"))
  )
}

testTableImportInModule <- function(input, output, session) {
  importedData <- importDataTable("tableImport1", serverRootDirectories = c("wd" = ".."))

  output$debug <- renderPrint({
    importedData()
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
