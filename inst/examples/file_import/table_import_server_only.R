library(shiny)
library(shinyfio)

ui <- fluidPage(
  wellPanel(
    dataTableImportWidget("tableImport1"),
    tags$hr(),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  importedData <- importDataTable("tableImport1", fileLocation = C_FILE_LOCATION_SERVER, serverRootDirectories = c("wd" = ".."))

  output$debug <- renderPrint({
    importedData()
  })
}

shinyApp(ui = ui, server = server)
