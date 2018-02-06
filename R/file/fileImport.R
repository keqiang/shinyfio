#' UI to upload a file
#'
#' This UI component provides UI for user to select a local file
#' and specify options to parse the file. The preview panel will
#' update accordingly so user will be able know if they are doing
#' things correctly or not.
#'
#' @param id the UI id for this module
#' @param label the label of this UI
#' @return A tagList that includes all the UI components
#' @export
fileImportUI <- function(id, label = "Comma or Tab Separated File") {
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
    # server will provide options after parsing the table.
    # this is to specify which column should be used as unique row names for R data frame
    uiOutput(ns("columnAsRowNamesControl")),
    # a preview table to help user get the right format
    conditionalPanel(
      condition = getJavaScriptOutput("fileUploaded", ns),
      box(
        width = 12,
        title = "Data Preview",
        status = "success",
        collapsible = TRUE,
        dataPreviewUI(ns("dataPreview"))
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
fileImport <- function(input, output, session, stringsAsFactors = FALSE) {

  # user selected file
  verifiedSelectedFile <- reactive({
    validate(need(input$selectedFile, message = FALSE))
    input$selectedFile
  })

  output$fileUploaded <- reactive({
    validate(need(input$selectedFile, message = FALSE))
    TRUE
  })

  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  # to give a correct list of column names, need to use NULL for param row.names
  tableColumnsOptions <- reactive({
    dataFrameWithAllColumnsAsData <- head(read.table(verifiedSelectedFile()$datapath,
                                                     header = input$hasColumnHeaders,
                                                     quote = input$quoteType,
                                                     sep = input$separator,
                                                     stringsAsFactors = stringsAsFactors,
                                                     row.names = NULL))
    colNames <- colnames(dataFrameWithAllColumnsAsData)
    tableColumns <- c(0 : length(colNames))
    names(tableColumns) <- c("Use automatic numbering as row names", colNames)
    tableColumns
  })

  # render a combobox for user to select which column to use as row.names param
  output$columnAsRowNamesControl <- renderUI({
    # for server side defined UI element, has to retrieve ns function and wrap the input's id
    ns <- session$ns
    selectInput(ns("columnAsRowNames"),
                label = "Choose a column as unique row names",
                choices = tableColumnsOptions())
  })

  # the current data frame imported regarding to the use specified options
  instantDataFrame <- reactive({
    columnIndex <- input$columnAsRowNames
    # the selectinput ui gives a string rather than integer, so needs to be converted
    if (!is.null(columnIndex)) {
      columnIndex <- as.integer(columnIndex)
      if (columnIndex == 0) {
        columnIndex <- NULL
      }
    }

    read.table(verifiedSelectedFile()$datapath,
               header = input$hasColumnHeaders,
               quote = input$quoteType,
               sep = input$separator,
               stringsAsFactors = stringsAsFactors,
               row.names = columnIndex)
  })

  returnForm <- reactive({
    actualDataFrame <- instantDataFrame()
    list(name = tools::file_path_sans_ext(verifiedSelectedFile()$name),
         type = "data.frame",
         size = dim(actualDataFrame),
         source = "File",
         data = actualDataFrame)
  })

  callModule(dataPreview, "dataPreview", returnForm)

  # return the imported data only when the import button clicked
  dataToBeImported <- eventReactive(input$importButton, {
    returnForm()
  })

  return(dataToBeImported)
}

dataPreviewUI <- function(id) {
  ns <- NS(id)

  tagList(
    DT::dataTableOutput(ns("previewTable")),
    checkboxInput(ns("showAllData"), label = "Show all", value = FALSE)
  )
}

dataPreview <- function(input, output, session, dataObject) {

  dataFrame <- reactive({
    if (input$showAllData == TRUE) {
      dataObject()$data
    } else {
      head(dataObject()$data)
    }
  })

  output$previewTable <- DT::renderDataTable(
    dataFrame(),
    options = list(scrollX = TRUE),
    selection = "none"
  )
}

getJavaScriptOutput <- function(id, ns) {
  return(paste0("output['", ns(id), "']"))
}
