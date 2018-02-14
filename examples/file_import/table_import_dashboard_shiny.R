library(shiny)
library(shinydashboard)
library(shinywidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Table Importing Example"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(title = "Data Import",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableImportWidget("tableImport1"),
          verbatimTextOutput("debug")
      )
    )
  )
)

server <- function(input, output) {
  importedData <- importDataTable("tableImport1")
  
  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)
