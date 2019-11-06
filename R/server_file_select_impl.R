# A Shiny module implementation of the server file selection widget
internalServerFileSelectUI <- function(id, title = "Please select a file", buttonLabel = "Browse...") {
  ns <- NS(id)
  
  tagList(
    div(
      class = "input-group",
      tags$label(
        class = "input-group-btn",
        shinyFiles::shinyFilesButton(
          ns("serverFile"),
          label = buttonLabel,
          title = title,
          multiple = FALSE
        )
      ),
      uiOutput(ns("selectedFileName"))
    )
  )
}

internalServerFileSelect <- function(input, output, session, serverRootDirectories = c("wd" = ".")) {
  # serverRootDirectories are the root directories that you would like to grant the users access and it must be a named vector.
  # this is required to enable the shinyFiles to work (similar to callModule)
  shinyFiles::shinyFileChoose(input, "serverFile", roots = serverRootDirectories)
  
  selectedFile <- reactiveVal(NULL) # initially selectd file path is null
  observeEvent(input$serverFile, {
    parsedFilePaths <- shinyFiles::parseFilePaths(serverRootDirectories, input$serverFile)

    if (nrow(parsedFilePaths) > 0) { # the parsed file paths will be a 0-row tibble when the server file not chosen
      selectedFile(as.list(parsedFilePaths)) # update the value only when it is not null and is different
    }
  })
  
  output$selectedFileName <- renderUI({ # render the displayed string of the current selected file
    selectedFileVal <- selectedFile()
    tags$input(
      type = "text",
      class = "form-control",
      value = ifelse(is.null(selectedFileVal), "", selectedFileVal$name),
      placeholder = ifelse(is.null(selectedFileVal), "No file selected", ""),
      readonly = "readonly"
    )
  })
  
  return(selectedFile)
}

#' Shiny UI widget to select a file on the server
#'
#' This widget provides UI for the user to select a server file
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the server file system.
#'
#' @param id the UI id for this widget
#' @param label the label of this widget
#' @param buttonLabel the label of the button
#' @return A tagList that includes all the UI components
#' @export
serverFileSelectWidget <- function(id, label = "Please select a file", buttonLabel = "Browse...") {
  internalServerFileSelectUI(id, label, buttonLabel)
}

#' Shiny serve logic for \code{serverFileSelectWidget}.
#'
#' Server logic that reacts to user input such as updating the current selected file path.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{serverFileSelectWidget}
#' @param serverRootDirectories the root directories that your app users are allowed to navigate and must it be a named vector.
#' @return the selected server file path as a reactive value
#' @export
selectServerFile <- function(id, serverRootDirectories) {
  callModule(internalServerFileSelect, id, serverRootDirectories)
}
