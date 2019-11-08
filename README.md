# shinywidgets
This package includes some commonly used Shiny customized UI widgets such as a file importing widget with data previewing capability.

# A live Shiny app uses modules from this package
https://keqiangli.shinyapps.io/genemap/  
[Source code](https://github.com/keqiang/rshinygenemap)

# Installation
`devtools::install_github("keqiang/shinywidgets")`

# Examples

## Import a data table file (regular Shiny)
```R
# app.R

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
```

## Import a data table file (Shiny module)
```R
# app.R

library(shiny)
library(shinywidgets)

testTableImportInModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableImportWidget(ns("tableImport1")),
    tags$hr(),
    verbatimTextOutput(ns("debug"))
  )
}

testTableImportInModule <- function(input, output, session) {
  importedData <- importDataTable("tableImport1", C_FILE_LOCATION_LOCAL)

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
```

## Import files from either local file system (will upload first) or the server that runs your Shiny application
```R
# app.R

library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    fileImportWidget("fileImport"),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  importedData <- importFile("fileImport", serverRootDirectories = c("wd" = ".."))

  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)
```
## [Other examples](inst/examples)
