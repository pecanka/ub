#' @title
#' Throw an error and stop
#'
#' @description
#'
#' A more elaborate version of `base::stop()`. It can do beeping and 
#' if inside an interactive session it does not halt the execution but 
#' enters a browsing mode. It also returns a reliable exit status value 
#' (i.e. 1) in non-interactive mode.
#'
#' Currently disabled, simply calls [`base::stop()`].
#'
#' @export
error = function(..., sep="", quit=TRUE, Q, browser=interactive(), nskip1=0, envir=parent.frame()) {

  msgs = dots_to_nlist()
  msg = collapse0(msgs, sep=sep)

  if(package_is_installed("beepr")) {
    try(beepr::beep(1))
  }
  
  fparent = eval(parse(text='this_fun_name()'), envir=envir)
  
  if(!missing(Q) && missing(quit)) quit = Q
  if(!missing(browser) && isTRUE(browser)) quit = FALSE

  cat0(spaces(nskip1, "\n"))
  err = ifelse(str_is_empty(fparent), "", "occured in function `"%.%
        fparent%.%"()` with message: ") %.%
        ifelse(str_is_empty(msg), "(no message)", msg) %.% '\n'
  
  if(quit) {
    halt(err) 
  } else {
    cat0("\nERROR: ",err,"\n\n")
    utils::flush.console()
    do.call(browser, list(), envir=sys.frame(sys.parent()))
  }
  
  invisible(err)
  
}

error = base::stop

#' @title
#' Stop execution
#'
#' @description
#'
#' A different way of stopping the execution. Intended to be called 
#' by halt but the behavior wasn't as hoped/expected. Abandoned for now.
#'
#' @export
.halt = function() {

  error = simpleError("")
  class(error) = c("myerror", class(error))
  signalCondition(error)
  
}

#' @title
#' Print error and stop execution
#'
#' @description
#'
#' Prints an error message and stops. The actual way of stopping, 
#' however, depends on whether the code is run in interactive or 
#' non-interactive modes
#'
halt = function(error="") {
  
  if(interactive()) {
  
    if(!str_is_empty(error)) catn()
    stop(error %.% "\nExecution halted.\n", call.=FALSE)
    
  } else {
  
    if(!str_is_empty(error)) base::cat(error)
    catn("\nExecution halted.")
    q("no", status=1, runLast=FALSE)
    
  }
}

