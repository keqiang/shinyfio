.getJavaScriptOutput <- function(id, ns) {
  return(paste0("output['", ns(id), "']"))
}

.getJavaScriptInput <- function(id, ns) {
  paste0("input['", ns(id), "']")
}

.getSession <- function() {
  session <- shiny::getDefaultReactiveDomain()
  
  if (is.null(session)) {
    stop(paste(
      "could not find the Shiny session object. This usually happens when a",
      "shinyjs function is called from a context that wasn't set up by a Shiny session."
    ))
  }
  
  session
}
