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
  fileImportWidget(id, C_DATA_TYPE_TABLE, FALSE)
}

#' Call this function in Shiny server logic to activate \link{dataTableImportWidget}.
#'
#' @param id The same ID as used in the matching call to \link{dataTableImportWidget}
#' @param fileLocation Specify from which location the file should be selected from
#' @param maxNumberOfLines Import at most this number of lines from the specified file
#' @param serverRootDirectories The root directories that your app users are allowed to navigate.
#'     It must be a named vector such as \code{c("server-dir1" = "/path/on/server/1/", "server-dir2" = "/path/on/server/2/")}.
#'     This parameter will only be used when \code{fileLocation} is specified as
#'     \code{C_FILE_LOCATION_SERVER} or \code{C_FILE_LOCATION_BOTH}
#' @return The imported data object as a reactive data frame
#' @export
importDataTable <- function(id,
                            fileLocation = c(C_FILE_LOCATION_BOTH, C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER),
                            maxNumberOfLines = Inf,
                            serverRootDirectories = NULL) {
  fileLocation <- match.arg(fileLocation)
  importFile(id, fileLocation, maxNumberOfLines, serverRootDirectories)
}
