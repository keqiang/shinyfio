#' @importFrom glue glue

getJavaScriptOutputId <- function(id, ns) {
  glue("output['{ns(id)}']")
}

getJavaScriptInputId <- function(id, ns) {
  glue("input['{ns(id)}']")
}

inputLabel <- function(label) {
  if (!is.null(label)) {
    div(
      tags$strong(label),
      style = "padding-bottom: 5px;"
    )
  }
}
