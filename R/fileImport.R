#' UI to import a data table from file
#'
#' This UI component provides UI for user to select a local file
#' and specify options to parse the file to a table. The preview panel will
#' update accordingly so user will be able know if they are doing
#' things correctly or not.
#'
#' @param id the UI id for this module
#' @param label the label of this UI
#' @return A tagList that includes all the UI components
#' @export
tableImportUI <- function(id, label = "Comma or Tab Separated File") {
  ns <- NS(id)
  
  tagList(
    # browse to select file to import
    fileInput(ns("selectedFile"), label, accept = c(
      "text/csv",
      "text/comma-separated-values",
      "text/plain",
      ".csv",
      ".tsv",
      ".txt"
    )),
    # checkbox to indicate if the original file has column headers
    checkboxInput(ns("hasColumnHeaders"), label = "Has column headers", value = TRUE),
    # combobox to specify quote type
    selectInput(ns("quoteType"), label = "Quote type", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )),
    # specify value separator
    selectInput(ns("separator"), label = "Separator type", c(
      "Comma" = ",",
      "Tab" = "\t"
    )),
    
    # a preview table to help user get the right format
    conditionalPanel(
      condition = .getJavaScriptOutput("fileUploaded", ns),
      wellPanel(
        tags$h3("Data Preview"),
        .dataTablePreviewUI(ns("dataTablePreview"))
      )
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
#' @param label Shiny module outputs
#' @param session Shiny session
#' @param stringsAsFactors to indicate if the strings should be imported as factors
#' @return the imported data object as a data frame
#' @export
tableImport <- function(input, output, session, stringsAsFactors = FALSE) {
  
  # user selected file
  verifiedSelectedFile <- reactive({
    req(input$selectedFile)
    input$selectedFile
  })
  
  output$fileUploaded <- reactive({
    req(input$selectedFile)
    TRUE
  })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # the current data frame imported regarding to the user specified options
  previewTibble <- reactive({
    read_func <- ifelse(input$separator == ",", readr::read_csv, readr::read_tsv)
    
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
    read_func <- ifelse(input$separator == ",", readr::read_csv, readr::read_tsv)
    
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
  
  tagList(
    DT::dataTableOutput(ns("previewTable")),
    checkboxInput(ns("showAllData"), label = "Show all", value = FALSE)
  )
}

.dataTablePreview <- function(input, output, session, dataTable) {
  displayData <- reactive({
    if (input$showAllData) {
      dataTable()
    } else {
      head(dataTable())
    }
  })
  
  output$previewTable <- DT::renderDataTable(
    displayData(),
    options = list(scrollX = TRUE),
    selection = "none"
  )
}

.getJavaScriptOutput <- function(id, ns) {
  return(paste0("output['", ns(id), "']"))
}
