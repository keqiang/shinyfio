library(shiny)
library(shinyfio)

testFileSelectInModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileSelectWidget(ns("fileSelect")),
    verbatimTextOutput(ns("debug"))
  )
}

testFileSelectInModule <- function(input, output, session) {
  selectedFile <- selectFile("fileSelect", fileLocation = C_FILE_LOCATION_BOTH, serverRootDirectories = c("wd" = "."))

  output$debug <- renderPrint({
    selectedFile()
  })
}


ui <- fluidPage(
  wellPanel(
    testFileSelectInModuleUI("test")
  )
)

server <- function(input, output) {
  callModule(testFileSelectInModule, "test")
}

shinyApp(ui = ui, server = server)
