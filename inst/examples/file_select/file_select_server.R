library(shiny)
library(shinywidgets)

ui <- fluidPage(
  wellPanel(
    serverFileSelectWidget("serverFile"),
    tags$hr(),
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
