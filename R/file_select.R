#' @import shiny

serverFolderSelectUI <- function(id, title = "Please select a folder", buttonLabel = "Browse...") {
  ns <- NS(id)

  tagList(
    div(
      class = "input-group",
      tags$label(
        class = "input-group-btn",
        shinyFiles::shinyDirButton(ns("serverDir"),
          label = buttonLabel,
          title = title
        )
      ),

      uiOutput(ns("selectedDir"))
    )
  )
}

serverFolderSelect <- function(input,
                                output,
                                session,
                                serverRootFolders) {
  shinyFiles::shinyDirChoose(input, "serverDir", roots = serverRootFolders)

  result <- reactive({
    req(input$serverDir)
    parseDirPath(absoluteServerPaths, input$serverDir)
  })

  output$selectedDir <- renderUI({
    if (is.null(input$serverDir)) {
      tags$input(
        type = "text",
        class = "form-control",
        placeholder = "No directory selected",
        readonly = "readonly"
      )
    } else {
      tags$input(
        type = "text",
        class = "form-control",
        value = result(),
        readonly = "readonly"
      )
    }
  })

  return(result)
}

#' Shiny UI widget to select a folder on the server
#'
#' This widget provides UI for the user to select a server folder.
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the server file system.
#'
#' @param id the UI id for this widget
#' @param label the label of this widget
#' @param buttonLabel the label of the button
#' @return A tagList that includes all the UI components
#' @export
serverFolderSelectWidget <- function(id, label = "Please select a folder", buttonLabel = "Browse...") {
  serverFolderSelectUI(id, label, buttonLabel)
}

#' Shiny serve logic for \code{serverFolderSelectWidget}.
#'
#' Server logic that reacts to user input such as updating the current selected folder path.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{serverFolderSelectWidget}
#' @param serverRootFolders the root folders that you allow your app users to navigate under

#' @return the selected server folder path as a reactive value
#' @export
selectServerFolder <- function(id, serverRootFolders, session = getDefaultReactiveDomain()) {
  callModule(serverFolderSelect, session$ns(id), serverRootFolders)
}

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

      uiOutput(ns("selectedFileLabel"))
    )
  )
}

#' Serve logic of server file select module
#'
#' The module server logic that reacts to user input.
#'
#' @param input Shiny module inputs
#' @param output Shiny module outputs
#' @param session Shiny session
#' @param serverRootDirectories the root folders that you would like to grant the users access and it must be a named vector.
#' @return the selected file path as a reactive value
internalServerFileSelect <- function(input, output, session, serverRootDirectories) {

  # this is required to enable the shinyFiles to work (similar to callModule)
  shinyFiles::shinyFileChoose(input, "serverFile", roots = serverRootDirectories)

  selectedFile <- reactiveVal(value = NULL) # initially selectd file path is null

  observeEvent(input$serverFile, {
    parsedFilePaths <- shinyFiles::parseFilePaths(serverRootDirectories, input$serverFile)
    if (nrow(parsedFilePaths) > 0) { # the parsed file paths will be a 0-row tibble when the server file not chosen
      selectedFile(parsedFilePaths[1, ]) # update the value only when it is not null and is different
    }
  })

  output$selectedFileLabel <- renderUI({ # render the displayed string of the current selected file
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
#' @param serverRootDirectories the root folders that you want user to navigate and must be a named vector.
#' @return the selected server file path as a reactive value
#' @export
selectServerFile <- function(id, serverRootDirectories, session = getDefaultReactiveDomain()) {
  callModule(internalServerFileSelect, session$ns(id), serverRootDirectories)
}

C_FILE_LOCATION_LOCAL <- "Local"
C_FILE_LOCATION_SERVER <- "Server"
C_FILE_LOCATION_BOTH <- "Both"

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
      tags$br(), # two line breaks to make it looks consitent with local file chooser
      tags$br()
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
    values$localFile <- input$localFile
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

#' @return the selected file path as a reactive value
#' @export
selectFile <- function(id,
                       fileLocation = c(C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER, C_FILE_LOCATION_BOTH),
                       serverRootDirectories = NULL,
                       session = getDefaultReactiveDomain()) {
  fileLocation <- match.arg(fileLocation)
  if (fileLocation != C_FILE_LOCATION_LOCAL) {
    if (is.null(serverRootDirectories)) {
      stop("Must specify server directories when fileLocation is specified other than 'Local'")
    }
  }
  callModule(internalFileSelect, session$ns(id), fileLocation = fileLocation, serverRootDirectories = serverRootDirectories)
}
