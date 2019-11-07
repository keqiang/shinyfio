# A Shiny module implementation of the server directory selection widget
internalServerDirectorySelectUI <- function(id, title = "Please select a directory", label = "Browse...") {
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
      uiOutput(ns("selectedDirName"))
    )
  )
}

internalServerDirectorySelect <- function(input, output, session, serverRootDirectories = c("wd" = ".")) {
  # serverRootDirectories are the root directories that you would like to grant the users access and it must be a named vector.
  # this is required to enable the shinyFiles to work (similar to callModule)
  shinyFiles::shinyDirChoose(input, "serverDir", roots = serverRootDirectories)
  
  selectedDir <- reactiveVal(value = NULL) # initially selectd directory is null
  
  observeEvent(input$serverDir, {
    parsedDirPath <- shinyFiles::parseDirPath(serverRootDirectories, input$serverDir)
    if (length(parsedDirPath) > 0) { # the parsed directory string will be a 0-length char vector when the server directory not chosen
      selectedDir(parsedDirPath) # update the value only when it is not null and is different
    }
  })
  
  output$selectedDirName <- renderUI({ # render the displayed string of the current selected directory
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

#' Shiny UI widget to select a directory on the server
#'
#' This widget provides UI for the user to select a server directory
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the server file system.
#'
#' @param id the UI id for this widget
#' @param label the label of this widget
#' @param buttonLabel the label of the button
#' @return The server directory selection component
#' @export
serverDirectorySelectWidget <- function(id, label = "Please select a folder", buttonLabel = "Browse...") {
  internalServerDirectorySelectUI(id, label, buttonLabel)
}

#' Shiny serve logic for \code{serverDirectorySelectWidget}.
#'
#' Server logic that reacts to user input such as updating the current selected directory path.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{serverDirectorySelectWidget}
#' @param serverRootDirectories the root directories that your app users are allowed to navigate and it must be a named vector.
#' @return the selected server directory path as a reactive value
#' @export
selectServerDirectory <- function(id, serverRootDirectories) {
  callModule(internalServerDirectorySelect, id, serverRootDirectories)
}