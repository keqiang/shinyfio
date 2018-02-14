#' Internal implementation of table import widget UI
#'
#' This widget provides UI for user to select a local file
#' and specify options to parse the file to a data table. The preview panel will
#' update accordingly so users will be able know if they are doing
#' things correctly.
#'
#' @param id the UI id for this widget
#' @param label the label of this widget
#' @return A tagList that includes all the UI components
.dataTableImportUI <-
  function(id, label = "Comma or Tab Separated File") {
    ns <- NS(id)
    
    tagList(
      .fileSelectUI(ns("fileSelect")),
      
      # checkbox to indicate if the original file has column headers
      checkboxInput(ns("hasColumnHeaders"), label = "Has column headers", value = TRUE),
      # combobox to specify quote type
      selectInput(
        ns("quoteType"),
        label = "Quote type",
        c(
          "None" = "",
          "Double quote" = "\"",
          "Single quote" = "'"
        )
      ),
      # specify value separator
      selectInput(ns("separator"), label = "Separator type", c("Comma" = ",",
                                                               "Tab" = "\t")),
      
      # a preview table to help user get the right format
      conditionalPanel(
        condition = .getJavaScriptOutput("fileUploaded", ns),
        wellPanel(tags$h3("Data Preview"),
                  .dataTablePreviewUI(ns(
                    "dataTablePreview"
                  )))
      ),
      # the import button
      actionButton(ns("importButton"), "Import")
    )
  }

#' Serve logic of file uploader UI
#'
#' The module server logic that reacts to user input.
#'
#' @param input Shiny module inputs
#' @param ouput Shiny module outputs
#' @param session Shiny session
#' @param fileLocation specify from which location the file should be selected from
#' @param serverRootFolders the root folders that you want user to navigate. only used when \code{fileLocation} is specified as 'server'
#' @return the imported data object as a data frame
.dataTableImport <-
  function(input,
           output,
           session,
           fileLocation = c("local", "server", "both"),
           serverRootFolders = c(".")) {
    fileLocation <- match.arg(fileLocation)
    selectedFile <-
      callModule(.fileSelect, "fileSelect", fileLocation, serverRootFolders)
    
    # user selected file
    verifiedSelectedFile <- reactive({
      req(selectedFile())
      selectedFile()
    })
    
    output$fileUploaded <- reactive({
      req(selectedFile())
      TRUE
    })
    
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    # the current data frame imported regarding to the user specified options
    previewTibble <- reactive({
      read_func <-
        ifelse(input$separator == ",", readr::read_csv, readr::read_tsv)
      read_func(
        verifiedSelectedFile()$datapath,
        col_names = input$hasColumnHeaders,
        quote = input$quoteType,
        n_max = 50
      )
    })
    
    callModule(.dataTablePreview, "dataTablePreview", previewTibble)
    
    # return the imported data only when the import button clicked
    dataToBeImported <- eventReactive(input$importButton, {
      read_func <-
        ifelse(input$separator == ",", readr::read_csv, readr::read_tsv)
      
      read_func(
        verifiedSelectedFile()$datapath,
        col_names = input$hasColumnHeaders,
        quote = input$quoteType
      )
    })
    
    return(dataToBeImported)
  }

.dataTablePreviewUI <- function(id) {
  ns <- NS(id)
  
  tagList(DT::dataTableOutput(ns("previewTable")),
          checkboxInput(ns("showAllData"), label = "Show all", value = FALSE))
}

.dataTablePreview <- function(input, output, session, dataTable) {
  displayData <- reactive({
    if (input$showAllData) {
      dataTable()
    } else {
      head(dataTable())
    }
  })
  
  output$previewTable <- DT::renderDataTable(displayData(),
                                             options = list(scrollX = TRUE),
                                             selection = "none")
}

#' Shiny UI widget to import a data table as data frame from file
#'
#' This widget provides UI for the user to select a local file
#' and specify options to parse the file to a data table. The preview panel will
#' update accordingly so users will be able to know if they are doing
#' things correctly.
#'
#' @param id the UI id for this widget
#' @param label the label of this widget
#' @return A tagList that includes all the UI components
#' @export
dataTableImportWidget <-
  function(id, label = "Comma or Tab Separated File") {
    .dataTableImportUI(id, label)
  }

#' Shiny serve logic for \code{dataTableImportWidget}.
#'
#' Server logic that reacts to user input such as updating the preview table.
#' This function must be called within a Shiny server function
#'
#' @param id The same ID as used in the matching call to \code{dataTableImportWidget}
#' @param fileLocation specify from which location the file should be selected from
#' @param serverRootFolders the root folders that you want your users to navigate.
#' This parameter will only be used when \code{fileLocation} is specified as 'server' or 'both' and must be a named vector 
#' such as c("server-dir1" = "/path/on/server/1/", "server-dir2" = "/path/on/server/2/")
#' @return the imported data object as a reactive data frame
#' @export
importDataTable <-
  function(id,
           fileLocation = c("local", "server", "both"),
           serverRootFolders = c(wd = ".")) {
    fileLocation <- match.arg(fileLocation)
    callModule(.dataTableImport, id, fileLocation, serverRootFolders)
  }