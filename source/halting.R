#' Throw an error and stop
#'
#' A more elaborate version of stop(). It can do beeping and if inside an interactive
#' session it doesn't stop execution but enters a browsing mode
#' @export
error = function(t, ..., sep="", quit=TRUE, Q, browser=interactive(), nskip1=0) {
  
  if(package_is_installed("beepr")) try(beepr::beep(1))
  
  if(!missing(Q) && missing(quit)) quit = Q
  if(!missing(browser) && isTRUE(browser)) quit = FALSE

  cat0(rep("\n", max(0,nskip1)))
  err = paste(t, ..., sep=sep)
  
  if(quit) {
    if(!interactive()) 
      err = "\nERROR: " %.% err %.% "\n"
    halt(err) 
  } else {
    cat0("\nERROR: ",err,"\n\n")
    utils::flush.console()
    do.call("browser", list(), envir=sys.frame(sys.parent()))
  }
  
  invisible(err)
  
}

#' Stop execution
#'
#' A different way of stopping the execution. Intended to be called by halt but the behavior
#' wasn't as hoped/expected. Abandoned for now. 
#' @export
.halt = function() {
  error = simpleError("")
  class(error) = c("myerror", class(error))
  signalCondition(error)
}

#' Print error and stop execution
#'
#' Prints an error message and stops. The actual way of stopping, however,
#' depends on whether the code is run in interactive or non-interactive
#' modes
halt = function(error="") {
  if(interactive()) {
    if(nchar(error)) cat("\n")
    error = paste0(error,"\nExecution halted.\n")
    #tryCatch(.halt(), myerror=function(...) cat(error))
    stop(error, call.=FALSE)
  } else {
    if(nchar(error)) cat(error)
    cat("\nExecution halted.\n")
    utils::flush.console()
    q("no", status=1, runLast=FALSE)
  }
}

