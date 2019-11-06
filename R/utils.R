#' @importFrom stringr str_c str_interp

getJavaScriptOutputId <- function(id, ns) {
  str_interp("output['${ns(id)}']")
}

getJavaScriptInputId <- function(id, ns) {
  str_interp("input['${ns(id)}']")
}
