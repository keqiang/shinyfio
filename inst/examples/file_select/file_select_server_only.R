library(shiny)
library(shinyfio)

ui <- fluidPage(
  wellPanel(
    serverFileSelectWidget("serverFile"),
    tags$br(),
    verbatimTextOutput("debug")
  )
)

server <- function(input, output) {
  selectedFile <- selectServerFile("serverFile", c("wd" = "."))

  output$debug <- renderPrint({
    selectedFile()
  })
}

shinyApp(ui = ui, server = server)
