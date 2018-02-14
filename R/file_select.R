# UI widget for server folder selection -----------------------------------

#' Internal implementation of \code{serverFolderSelectWidget}, a Shiny module to select a folder on the server
#'
#' This module provides UI for the user to select a server folder.
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the server file system.
#'
#' @param id the UI id for this module
#' @param title The heading of the dialog box that appears when the button is pressed
#' @param buttonLabel the label of the button
#' @return A tagList that includes all the UI components
.serverFolderSelectUI <-
  function(id,
           title = "Please select a folder",
           buttonLabel = "Browse...") {
    ns <- NS(id)
    
    tagList(div(
      class = "input-group",
      tags$label(
        class = "input-group-btn",
        shinyFiles::shinyDirButton(ns("serverDir"),
                                   label = buttonLabel,
                                   title = title)
      ),
      
      uiOutput(ns("selectedDir"))
    ))
  }

#' Serve logic of server folder select module
#'
#' The module server logic that reacts to user input.
#'
#' @param input Shiny module inputs
#' @param output Shiny module outputs
#' @param session Shiny session
#' @param serverRootFolders the root folders that you want user to navigate and must be a named vector.
#' #' @return the selected folder path as a reactive value
.serverFolderSelect <-
  function(input,
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
serverFolderSelectWidget <-
  function(id,
           label = "Please select a folder",
           buttonLabel = "Browse...") {
    .serverFolderSelectUI(id, label, buttonLabel)
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
selectServerFolder <-
  function(id,
           serverRootFolders) {
    callModule(.serverFolderSelect, id, serverRootFolders)
  }

# UI widget for server file selection -------------------------------------

#' Internal implementation of \code{serverFileSelectWidget}, a Shiny module to select a file on the server
#'
#' This module provides UI for the user to select a server file.
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the server file system.
#'
#' @param id the UI id for this module
#' @param title The heading of the dialog box that appears when the button is pressed
#' @param buttonLabel the label of the button
#' @return A tagList that includes all the UI components
.serverFileSelectUI <-
  function(id,
           title = "Please select a file",
           buttonLabel = "Browse...") {
    ns <- NS(id)
    
    tagList(div(
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
      
      uiOutput(ns("selectedFile"))
    ))
  }

#' Serve logic of server file select module
#'
#' The module server logic that reacts to user input.
#'
#' @param input Shiny module inputs
#' @param output Shiny module outputs
#' @param session Shiny session
#' @param serverRootFolders the root folders that you want user to navigate and must be a named vector.
#' @return the selected file path as a reactive value
.serverFileSelect <-
  function(input,
           output,
           session,
           serverRootFolders) {
    
    shinyFiles::shinyFileChoose(input, session$ns("serverFile"), roots = serverRootFolders)
    
    result <- reactive({
      req(input$serverFile)
      shinyFiles::parseFilePaths(serverRootFolders, input$serverFile)
    })
    
    output$selectedFile <- renderUI({
      if (is.null(input$serverFile)) {
        tags$input(
          type = "text",
          class = "form-control",
          placeholder = "No file selected",
          readonly = "readonly"
        )
      } else {
        tags$input(
          type = "text",
          class = "form-control",
          value = result()$name,
          readonly = "readonly"
        )
      }
    })
    
    return(result)
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
serverFileSelectWidget <-
  function(id,
           label = "Please select a file",
           buttonLabel = "Browse...") {
    .serverFileSelectUI(id, label, buttonLabel)
  }

#' Shiny serve logic for \code{serverFileSelectWidget}.
#'
#' Server logic that reacts to user input such as updating the current selected file path.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{serverFileSelectWidget}
#' @param serverRootFolders the root folders that you want user to navigate and must be a named vector.
#' @return the selected server file path as a reactive value
#' @export
selectServerFile <-
  function(id,
           serverRootFolders) {
    callModule(.serverFileSelect, id, serverRootFolders)
  }

# UI widget for file selection --------------------------------------------

#' Internal implementation of \code{fileSelectWidget}, a Shiny UI widget to select a file
#'
#' This module provides UI for the user to select a file that is from local machine or on the server.
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the file system.
#'
#' @param id the UI id for this module
#' @return A tagList that includes all the UI components
.fileSelectUI <-
  function(id) {
    ns <- NS(id)
    
    uiOutput(ns("ui"))
  }


#' Serve logic of file select module
#'
#' The module server logic that reacts to user input.
#'
#' @param input Shiny module inputs
#' @param output Shiny module outputs
#' @param session Shiny session
#' @param fileLocation specify from which location the file should be selected from
#' @param serverRootFolders the root folders that you want user to navigate.
#' only used when \code{fileLocation} is specified as 'server' and must be a named vector.
#' #' @return the selected file path as a reactive value
.fileSelect <-
  function(input,
           output,
           session,
           fileLocation = c("local", "server", "both"),
           serverRootFolders = c(".")) {
    fileLocation <- match.arg(fileLocation)
    ns <- session$ns
    output$ui <- renderUI({
      if (fileLocation == "both") {
        tagList(
          radioButtons(
            ns("fileLocation"),
            label = "File Location",
            choices = c("local", "server"),
            inline = TRUE,
            selected = "local"
          ),
          conditionalPanel(
            paste0(.getJavaScriptInput("fileLocation", ns),
                   " == 'local'"),
            fileInput(
              ns("localFile"),
              label = NULL,
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/plain",
                ".csv",
                ".tsv",
                ".txt"
              )
            )
          ),
          conditionalPanel(
            paste0(.getJavaScriptInput("fileLocation", ns),
                   " == 'server'"),
            .serverFileSelectUI(ns("serverFile"))
          )
        )
      } else if (fileLocation == "local") {
        fileInput(
          ns("localFile"),
          label = NULL,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".tsv",
            ".txt"
          )
        )
      } else {
        .serverFileSelectUI(ns("serverFile"))
      }
    })
    
    serverFile <-
      callModule(.serverFileSelect, "serverFile", serverRootFolders)
    
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
      if (fileLocation == "local") {
        values$localFile
      } else if (fileLocation == "server") {
        values$serverFile
      } else {
        req(input$fileLocation)
        if (input$fileLocation == "server") {
          values$serverFile
        } else {
          values$localFile
        }
      }

    })
    
    return(result)
  }

#' Shiny UI widget to select a file
#'
#' This widget provides UI for the user to select a file that is from local machine or on the server.
#' When user clicks the button, a modal dialog will show up for user
#' to navigate the file system.
#'
#' @param id the UI id for this widget
#' @return A tagList that includes all the UI components
#' @export
fileSelectWidget <- function(id) {
  .fileSelectUI(id)
}

#' Shiny serve logic for \code{fileSelectWidget}.
#'
#' Server logic that reacts to user input such as updating the current selected file path.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{fileSelectWidget}
#' @param fileLocation a parameter indicates where you want ther users to navigate the files.

#' @return the selected file path as a reactive value
#' @export
selectFile <-
  function(id,
           fileLocation = c("local", "server", "both")) {
    fileLocation <- match.arg(fileLocation)
    callModule(fileSelect, id, fileLocation)
  }