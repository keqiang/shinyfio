# shinywidgets
This package includes some commonly used Shiny customized UI utilities such as a file uploader with data previewing capability.

# Installation
`devtools::install_github("keqiang/shinywidgets")`

# Example: app.R

## Regular Shiny app
```R
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

```

## Dashboard Shiny app
```R
library(shiny)
library(shinydashboard)
library(shinywidgets)

ui <- dashboardPage(
  dashboardHeader(title = "File Importing Module Example"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(title = "Data Import",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          tableImportUI("tableImport1"),
          verbatimTextOutput("debug")
      )
    )
  )
)

server <- function(input, output) {
  importedData <- callModule(tableImport, "tableImport1")
  
  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)
```
