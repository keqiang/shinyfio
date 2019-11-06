#' @importFrom readr read_csv read_tsv
#' @importFrom magrittr %>%
#'

#' @export
C_DATA_TYPE_TABLE <- "Table"
#' @export
C_DATA_TYPE_MATRIX <- "Matrix"

C_DATA_TYPE_OPTIONS <- c(
  "Table (can contain string values)" = C_DATA_TYPE_TABLE,
  "Matrix (numeric values only)" = C_DATA_TYPE_MATRIX
)

C_SEPARATOR_TYPE_TAB <- "tab"
C_SEPARATOR_TYPE_COMMA <- "comma"

#' Internal implementation of file importing UI
internalFileImportUI <- function(id,
                                 dataType = c(C_DATA_TYPE_TABLE, C_DATA_TYPE_MATRIX),
                                 enableDataTypeSelection = TRUE) {
  ns <- NS(id)

  dataTypeSelectInput <- selectizeInput(
    ns("dataType"),
    "Import as",
    choices = C_DATA_TYPE_OPTIONS,
    selected = dataType
  )

  if (!enableDataTypeSelection) {
    dataTypeSelectInput <- shinyjs::disabled(dataTypeSelectInput)
  }

  tagList(
    fileSelectWidget(ns("fileSelect")),
    dataTypeSelectInput,
    checkboxInput(
      ns("tableHasHeader"),
      label = "Data has column headers",
      value = TRUE
    ),
    # separator type
    selectInput(
      ns("separatorType"),
      label = "File Separator",
      choices = c("Tab" = C_SEPARATOR_TYPE_TAB, "Comma" = C_SEPARATOR_TYPE_COMMA)
    ),
    # a preview table to help the user get the right format
    conditionalPanel(
      condition = getJavaScriptOutputId("fileUploaded", ns),
      wellPanel(DT::dataTableOutput(ns("previewTable")))
    ),

    textOutput(ns("status")),
    actionButton(ns("importButton"), "Import")
  )
}

internalFileImport <- function(input, output, session,
                               fileLocation = c(C_FILE_LOCATION_BOTH, C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER),
                               serverRootDirectories = c(".")) {
  fileLocation <- match.arg(fileLocation)
  selectedFile <- selectFile("fileSelect", fileLocation, serverRootDirectories)

  output$fileUploaded <- reactive({
    req(selectedFile())
    TRUE
  })

  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  fileReadFun <- reactive({
    ifelse(input$separatorType == C_SEPARATOR_TYPE_TAB, read_tsv, read_csv)
  })

  previewData <- reactive({
    req(selectedFile())
    rf <- fileReadFun()
    if (input$tableHasHeader) {
      colNames <-
        rf(
          selectedFile()$datapath,
          trim_ws = TRUE,
          n_max = 1,
          col_names = FALSE, # take the column name as regular data
          col_types = readr::cols() # avoid the verbose logging
        ) %>%
        unlist(., use.names = FALSE) # convert the first row to a vector

      validate(need(!anyDuplicated(colNames), "column names have duplicates"))

      dt <-
        rf(
          selectedFile()$datapath,
          trim_ws = TRUE,
          n_max = 6,
          skip = 1, # skip the column headers
          col_names = FALSE,
          col_types = readr::cols()
        )
      if (length(colNames) == ncol(dt) - 1) {
        colNames <- c("Auto_Added_Header", colNames)
      }
      colnames(dt) <- colNames
    } else {
      dt <-
        rf(
          selectedFile()$datapath,
          trim_ws = TRUE,
          n_max = 6,
          col_names = FALSE,
          col_types = readr::cols()
        )
    }
    validate(
      need(
        nrow(dt) > 0,
        "No data can be read. Check if you have chosen the wrong file or not specified the right file separator or format"
      )
    )

    if (input$dataType == C_DATA_TYPE_MATRIX) {
      rowNames <- dt[[1]]

      numOfCols <- ncol(dt)

      validate(
        need(
          numOfCols >= 2,
          "There are no more data columns. Please check your data and importing configs"
        )
      )

      dataPart <- dt[2:ncol(dt)] # the first column is the row names
      testNumeric <-
        dataPart %>%
        dplyr::summarise_all(is.numeric) %>%
        tidyr::gather()

      validate(
        need(
          all(testNumeric$value),
          "There are non-numeric values in your data. Import as 'Table' if you intended to import a data table with strings"
        )
      )

      dt <- data.matrix(dataPart)

      row.names(dt) <- rowNames
    }

    dt
  })

  output$previewTable <- DT::renderDataTable({
    dt <- previewData()
    req(dt)
    if (input$tableHasHeader) {
      DT::datatable(
        dt,
        options = list(
          scrollX = TRUE,
          dom = "t",
          bSort = FALSE
        ),
        selection = "none"
      )
    } else {
      DT::datatable(
        dt,
        colnames = rep("", ncol(dt)),
        options = list(
          scrollX = TRUE,
          dom = "t",
          bSort = FALSE
        ),
        selection = "none"
      )
    }
  })

  importedData <- eventReactive(input$importButton, {
    shinyjs::disable("importButton")
    tryCatch({
      req(previewData())
      withProgress({
        setProgress(0.1, detail = "validating")
        rf <- fileReadFun()
        if (input$tableHasHeader) {
          setProgress(0.2, detail = "reading column headers")
          colNames <-
            rf(
              selectedFile()$datapath,
              trim_ws = TRUE,
              n_max = 1,
              col_names = FALSE, # treat the column name as regular data
              col_types = readr::cols() # avoid the verbose logging
            ) %>%
            unlist(., use.names = FALSE) # convert the first row to a vector
          setProgress(0.3, detail = "reading data")
          dt <-
            rf(
              selectedFile()$datapath,
              trim_ws = TRUE,
              skip = 1,
              col_names = FALSE,
              col_types = readr::cols()
            )
          if (length(colNames) == ncol(dt) - 1) {
            colNames <- c("Auto_Added_Header", colNames)
          }
          colnames(dt) <- colNames
        } else {
          setProgress(0.3, detail = "reading data")
          dt <-
            rf(
              selectedFile()$datapath,
              trim_ws = TRUE,
              col_names = FALSE,
              col_types = readr::cols()
            )
        }

        if (input$dataType == "data.matrix") {
          rowNames <- dt[[1]]

          dataPart <- dt[2:ncol(dt)]
          testNumeric <-
            dataPart %>%
            summarise_all(is.numeric) %>%
            gather()

          validate(
            need(
              all(testNumeric$value),
              "There are non-numeric values in your data. Import as 'Table' if you intended to import a data table with strings"
            )
          )

          dt <- data.matrix(dataPart)

          row.names(dt) <- rowNames
        }
        setProgress(0.9, detail = "finishing up")
        return(dt)
      }, message = "Importing file")
    }, finally = shinyjs::enable("importButton"))
  })

  observe({
    shinyjs::disable("importButton")
    req(selectedFile())
    req(previewData())
    shinyjs::enable("importButton")
  })

  # return the imported data only when the import button clicked
  result <- eventReactive(importedData(), {
    dt <- importedData()
    req(dt)
    newDataObject(
      data = dt,
      type = input$dataType,
      name = tools::file_path_sans_ext(selectedFile()$name),
      size = dim(dt),
      source = "File"
    )
  })

  return(result)
}

#' Shiny UI widget to import data from a file
#'
#' This widget provides UI for the user to select a file
#' and specify options to parse and import the file. The preview panel will
#' update accordingly so users will be able to know if they are doing
#' things correctly.
#'
#' @param id ID of this UI component
#' @param dataType Data type to parse the file as
#' @param enableDataTypeSelection Whether to let the user specify which data type to import the file as
#' @return The widget
#' @export
fileImportWidget <- function(id,
                             dataType = c(C_DATA_TYPE_TABLE, C_DATA_TYPE_MATRIX),
                             enableDataTypeSelection = TRUE) {
  dataType <- match.arg(dataType)
  internalFileImportUI(id, dataType, enableDataTypeSelection)
}

#' Call this function in Shiny server logic to activate \link{fileImportWidget}.
#'
#' @param id The same ID as used in the matching call to \link{fileImportWidget}
#' @param fileLocation Specify from which location the file should be selected from
#' @param serverRootDirectories The root directories that your app users are allowed to navigate.
#'     It must it be a named vector such as \code{c("server-dir1" = "/path/on/server/1/", "server-dir2" = "/path/on/server/2/")}.
#'     This parameter will only be used when \code{fileLocation} is specified as
#'     \code{C_FILE_LOCATION_SERVER} or \code{C_FILE_LOCATION_BOTH}
#' @return The imported data as the specified type
#' @export
importFile <- function(id,
                       fileLocation = c(C_FILE_LOCATION_BOTH, C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER),
                       serverRootDirectories = NULL) {
  fileLocation <- match.arg(fileLocation)
  if (fileLocation != C_FILE_LOCATION_LOCAL) {
    # TODO add more logic to validate the server root directories
    if (is.null(serverRootDirectories)) {
      stop("Must specify server directories when fileLocation is specified other than 'Local'")
    }
  }
  callModule(internalFileImport, id, fileLocation, serverRootDirectories)
}
