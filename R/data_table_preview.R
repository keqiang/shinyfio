internalDataTablePreviewUI <- function(id, enableOptionToShowAllRows = TRUE) {
  ns <- NS(id)

  showAllCheckbox <- checkboxInput(ns("showAllData"), label = "Show all", value = FALSE)
  if (!enableOptionToShowAllRows) {
    showAllCheckbox <- shinyjs::hidden(showAllCheckbox)
  }

  tagList(
    showAllCheckbox,
    DT::dataTableOutput(ns("previewTable"))
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

  output$previewTable <- DT::renderDataTable({
    options <- list(scrollX = TRUE, dom = "t", bSort = FALSE) # enable x axis scrolling and disable column sorting
    if (isolate(input$showAllData)) { # If displaying all rows, show page length change input
      options$dom <- "litp"
    }
    DT::datatable(
      displayData(),
      options = options,
      selection = "none"
    )
  })
}

#' A data table preview widget
#' 
#' @param id The same ID as used in the matching call to \link{previewDataTable}
#' @param enableOptionToShowAllRows Whether or not to show a checkbox that toggles all rows all only the first few rows
#' @return The widget
#' @examples
#' # In Shiny UI logic, add this widget
#' dataTablePreviewWidget("previewTable")
#' # Then in your Shiny server logic, add following statements
#' dataTable <- reactive({ # generate your data frame here })
#' previewDataTable("previewTable", dataTable)
#' @export
dataTablePreviewWidget <- function(id, enableOptionToShowAllRows = TRUE) {
  internalDataTablePreviewUI(id, enableOptionToShowAllRows)
}

#' Call this function in Shiny server logic to activate \link{dataTablePreviewWidget}
#' 
#' @param id The same ID as used in the matching call to \link{dataTablePreviewWidget}
#' @param dataTable A reactive expression that returns a data table
#' @export
previewDataTable <- function(id, dataTable) {
  callModule(internalDataTablePreview, id, dataTable)
}
