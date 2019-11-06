#' @import shiny

#' @export
C_FILE_LOCATION_LOCAL <- "Local"
#' @export
C_FILE_LOCATION_SERVER <- "Server"
#' @export
C_FILE_LOCATION_BOTH <- "Both"

# A Shiny module implementation of the file selection widget
internalFileSelectUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden( # hide the radio buttons when only one option is specified by from the server function
      radioButtons( # radio buttons to specify file location
        ns("fileLocation"),
        label = "File Location",
        choices = c(C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER),
        inline = TRUE,
        selected = C_FILE_LOCATION_LOCAL
      )
    ),
    conditionalPanel( # if selecting from a local file
      stringr::str_c(getJavaScriptInputId("fileLocation", ns), " == '", C_FILE_LOCATION_LOCAL, "'"),
      fileInput(
        ns("localFile"),
        label = NULL,
        accept = c(
          "text/csv",
          "text/comma-separated-values",
          "text/plain",
          ".csv",
          ".tsv",
          ".txt",
          ".narrowPeak",
          ".broadPeak"
        )
      )
    ),
    conditionalPanel( # if selecting from the remote server (where Shiny server runs)
      stringr::str_c(getJavaScriptInputId("fileLocation", ns), " == '", C_FILE_LOCATION_SERVER, "'"),
      serverFileSelectWidget(ns("serverFile")),
      tags$br() # line break to make it looks consitent with local file chooser
    )
  )
}

internalFileSelect <- function(input,
                               output,
                               session,
                               fileLocation = c(C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER, C_FILE_LOCATION_BOTH),
                               serverRootDirectories = NULL) {
  fileLocation <- match.arg(fileLocation)
  observe({
    if (fileLocation == C_FILE_LOCATION_SERVER) {
      updateRadioButtons(session, "fileLocation", selected = "server")
    } else if (fileLocation == C_FILE_LOCATION_BOTH) { # only enable the input when both was specified
      shinyjs::show("fileLocation")
    }
  })
  serverFile <- selectServerFile("serverFile", serverRootDirectories)
  
  values <- reactiveValues(serverFile = NULL, localFile = NULL)
  
  observe({
    req(input$localFile)
    values$localFile <- as.list(input$localFile)
  })
  
  observe({
    req(serverFile())
    values$serverFile <- serverFile()
  })
  
  result <- reactive({
    req(input$fileLocation)
    if (input$fileLocation == C_FILE_LOCATION_SERVER) {
      values$serverFile
    } else {
      values$localFile
    }
  })
  
  return(result)
}

#' A Shiny UI widget used to select a file
#'
#' This module provides a UI component for the user to select a file that is from local file system or on the server.
#' When user clicks the button, a modal dialog will show up for user to navigate the file system.
#'
#' @param id ID of this UI component
#' @return Return the UI component
#' @export
fileSelectWidget <- function(id) {
  internalFileSelectUI(id)
}

#' Select a file
#'
#' Server logic that reacts to user input such as updating the current selected file path.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{fileSelectWidget}
#' @param fileLocation a parameter indicates where you want ther users to navigate the files.
#' @param serverRootDirectories the root directories that you allow your app users to navigate under
#' @return the selected file path as a reactive value
#' @export
selectFile <- function(id,
                       fileLocation = c(C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER, C_FILE_LOCATION_BOTH),
                       serverRootDirectories = NULL) {
  fileLocation <- match.arg(fileLocation)
  if (fileLocation != C_FILE_LOCATION_LOCAL) {
    # TODO add more logic to validate the server root directories
    if (is.null(serverRootDirectories)) {
      stop("Must specify server directories when fileLocation is specified other than 'Local'")
    }
  }
  callModule(internalFileSelect, id, fileLocation = fileLocation, serverRootDirectories = serverRootDirectories)
}
