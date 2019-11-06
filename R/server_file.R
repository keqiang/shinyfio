serverDirectorySelectUI <- function(id, title = "Please select a directory", label = "Browse...") {
  ns <- NS(id)

  tagList(
    div(
      class = "input-group",
      tags$label(
        class = "input-group-btn",
        shinyFiles::shinyDirButton(
          ns("serverDir"),
          label = label,
          title = title
        )
      ),
      uiOutput(ns("selectedDirLabel"))
    )
  )
}


serverDirectorySelect <- function(input, output, session, serverRoots) {
  # this is required to enable the shinyFiles to work (similar to callModule)
  shinyFiles::shinyDirChoose(input, "serverDir", roots = serverRoots)

  selectedDir <- reactiveVal(value = NULL) # initially selectd directory is null

  observeEvent(input$serverDir, {
    parsedDirPath <- shinyFiles::parseDirPath(serverRoots, input$serverDir)
    if (length(parsedDirPath) > 0) { # the parsed directory string will be a 0-length char vector when the server directory not chosen
      selectedDir(parsedDirPath) # update the value only when it is not null and is different
    }
  })

  output$selectedDirLabel <- renderUI({ # render the displayed string of the current selected directory
    selectedDirVal <- selectedDir()
    tags$input(
      type = "text",
      class = "form-control",
      value = ifelse(is.null(selectedDirVal), "", selectedDirVal),
      placeholder = ifelse(is.null(selectedDirVal), "No directory selected", ""),
      readonly = "readonly"
    )
  })

  return(selectedDir)
}
