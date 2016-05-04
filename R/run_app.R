#' Run event detector app
#'
#' @return
#' @export
#'
event_detector =  function() {
  appDir = system.file("event_detector", package = "eventDetector")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `eventDetector`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
