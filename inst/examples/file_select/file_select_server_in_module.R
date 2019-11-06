library(shiny)
library(shinywidgets)

testServerFileSelectInModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    serverFileSelectWidget(ns("serverFile")),
    tags$hr(),
    verbatimTextOutput(ns("debug"))
  )
}

testServerFileSelectInModule <- function(input, output, session) {
  selectedFile <- selectServerFile("serverFile", c("wd" = "."))

  output$debug <- renderPrint({
    selectedFile()
  })
}


ui <- fluidPage(
  wellPanel(
    testServerFileSelectInModuleUI("test")
  )
)

server <- function(input, output) {
  callModule(testServerFileSelectInModule, "test")
}

shinyApp(ui = ui, server = server)
