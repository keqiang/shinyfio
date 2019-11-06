library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    fileImportWidget("fileImport"),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  importedData <- importFile("fileImport", serverRootDirectories = c("wd" = "."))

  output$debug <- renderPrint({
    print(importedData())
  })
}

shinyApp(ui = ui, server = server)
