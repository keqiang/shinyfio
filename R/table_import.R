#' Internal implementation of table import widget UI
internalDataTableImportUI <- function(id, label = "Tab or Comma Separated File") {
  ns <- NS(id)

  tagList(
    fileSelectWidget(ns("fileSelect"), label = "Select a File"),

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
    selectInput(ns("separator"), label = "Separator type", c("Tab" = "\t", "Comma" = ",")),
    # a preview table to help user get the right format
    conditionalPanel(
      condition = getJavaScriptOutputId("fileUploaded", ns),
      wellPanel(
        tags$h3("Data Preview"),
        internalDataTablePreviewUI(ns("dataTablePreview"))
      )
    ),
    # the import button
    actionButton(ns("importButton"), "Import")
  )
}

internalDataTableImport <- function(input,
                                    output,
                                    session,
                                    fileLocation = c(C_FILE_LOCATION_BOTH, C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER),
                                    serverRootDirectories = c(".")) {
  fileLocation <- match.arg(fileLocation)
  selectedFile <- selectFile("fileSelect", fileLocation, serverRootDirectories)

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

  callModule(internalDataTablePreview, "dataTablePreview", previewTibble)

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

internalDataTablePreviewUI <- function(id) {
  ns <- NS(id)

  tagList(
    DT::dataTableOutput(ns("previewTable")),
    checkboxInput(ns("showAllData"), label = "Show all", value = FALSE)
  )
}

internalDataTablePreview <- function(input, output, session, dataTable) {
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

#' Shiny UI widget to import a data table as data frame from file
#'
#' This widget provides UI for the user to select a file
#' and specify options to parse the file to a data table. The preview panel will
#' update accordingly so users will be able to know if they are doing
#' things correctly.
#'
#' @param id ID of this UI component
#' @param title Title of the popup dialog
#' @return The widget
#' @export
dataTableImportWidget <- function(id, title = "Tab or Comma Separated File") {
  internalDataTableImportUI(id, title)
}

#' Call this function in Shiny server logic to activate \link{dataTableImportWidget}.
#'
#' @param id The same ID as used in the matching call to \link{dataTableImportWidget}
#' @param fileLocation Specify from which location the file should be selected from
#' @param serverRootDirectories The root directories that your app users are allowed to navigate.
#'     It must it be a named vector such as \code{c("server-dir1" = "/path/on/server/1/", "server-dir2" = "/path/on/server/2/")}.
#'     This parameter will only be used when \code{fileLocation} is specified as
#'     \code{C_FILE_LOCATION_SERVER} or \code{C_FILE_LOCATION_BOTH}
#' @return The imported data object as a reactive data frame
#' @export
importDataTable <- function(id,
                            fileLocation = c(C_FILE_LOCATION_BOTH, C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER),
                            serverRootDirectories = NULL) {
  fileLocation <- match.arg(fileLocation)
  callModule(internalDataTableImport, id, fileLocation, serverRootDirectories)
}
