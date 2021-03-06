library(shiny)
library(shinyfio)

ui <- fluidPage(
  wellPanel(
    fileSelectWidget("fileSelect"),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  selectedFile <- selectFile("fileSelect", fileLocation = C_FILE_LOCATION_BOTH, serverRootDirectories = c("wd" = "."))

  output$debug <- renderPrint({
    selectedFile()
  })
}

shinyApp(ui = ui, server = server)
