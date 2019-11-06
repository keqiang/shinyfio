#' @importFrom glue glue

getJavaScriptOutputId <- function(id, ns) {
  glue("output['{ns(id)}']")
}

getJavaScriptInputId <- function(id, ns) {
  glue("input['{ns(id)}']")
}
