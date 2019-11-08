library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    dataTableImportWidget("tableImport1"),
    tags$hr(),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  # specifying fileLocation as "server" or "both" to provide users the option to import a data table from server file
  importedData <- importDataTable("tableImport1", fileLocation = C_FILE_LOCATION_BOTH, serverRootDirectories = c("wd" = ".."))

  output$debug <- renderPrint({
    importedData()
  })
}

shinyApp(ui = ui, server = server)
