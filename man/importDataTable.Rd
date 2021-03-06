% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_import.R
\name{importDataTable}
\alias{importDataTable}
\title{Call this function in Shiny server logic to activate \link{dataTableImportWidget}.}
\usage{
importDataTable(id, fileLocation = c(C_FILE_LOCATION_BOTH,
  C_FILE_LOCATION_LOCAL, C_FILE_LOCATION_SERVER), maxNumberOfLines = Inf,
  serverRootDirectories = NULL)
}
\arguments{
\item{id}{The same ID as used in the matching call to \link{dataTableImportWidget}}

\item{fileLocation}{Specify from which location the file should be selected from}

\item{maxNumberOfLines}{Import at most this number of lines from the specified file}

\item{serverRootDirectories}{The root directories that your app users are allowed to navigate.
It must be a named vector such as \code{c("server-dir1" = "/path/on/server/1/", "server-dir2" = "/path/on/server/2/")}.
This parameter will only be used when \code{fileLocation} is specified as
\code{C_FILE_LOCATION_SERVER} or \code{C_FILE_LOCATION_BOTH}}
}
\value{
The imported data object as a reactive data frame
}
\description{
Call this function in Shiny server logic to activate \link{dataTableImportWidget}.
}
