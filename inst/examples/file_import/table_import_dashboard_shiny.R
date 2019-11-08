library(shiny)
library(shinydashboard)
library(shinywidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Table Importing Example"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Data Import",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        dataTableImportWidget("tableImport1"),
        tags$hr(),
        verbatimTextOutput("debug")
      )
    )
  )
)

server <- function(input, output) {
  importedData <- importDataTable("tableImport1", fileLocation = C_FILE_LOCATION_LOCAL)

  output$debug <- renderPrint({
    importedData()
  })
}

shinyApp(ui = ui, server = server)
