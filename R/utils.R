.getJavaScriptOutput <- function(id, ns) {
  return(paste0("output['", ns(id), "']"))
}

.getJavaScriptInput <- function(id, ns) {
  paste0("input['", ns(id), "']")
}