#' @title
#' Stop execution without the error message produced by `base::stop`.
#'
#' @description
#'
#' Stops the execution of code without an error message such as the 
#' one produced by `base::stop`. By default, prints a message about 
#' stopping quietly with the calling sequence. To stop completely 
#' silently, use `stop_quietly(NULL)`.
#'
#' @export
stop2 = function(msg='Execution stopped.', show_sequence=TRUE) {

  msg_call = caller_info()[if(show_sequence) 'message' else 'fun']
  
  if(!is.null(msg)) {
    msgf(paste(c(msg, msg_call), collapse = if(missing(msg)) ' ' else '\n'))
  }
  
  opt = options(show.error.messages = FALSE)
  on.exit(options(opt))
  
  stop()

}

#' @name stop2
#' @export
stop_quietly = stop2

#' @title
#' Print an error message and stop the execution
#'
#' @description
#' Prints an error message and stops the execution of code. 
#' The actual way of stopping, however, depends on whether 
#' the code is run in interactive or non-interactive modes.
#'
#' `.halt()` is a different way of stopping the execution. Intended to be
#' called by `halt()` but the behavior was not as hoped/expected. Abandoned 
#' for now.
#'
halt = function(msg="") {
  
  if(interactive()) {
  
    if(!str_is_empty(msg)) 
      base::cat('\n')
      
    stop(msg, "\nExecution halted.\n", call.=FALSE)
    
  } else {
  
    if(!str_is_empty(msg)) 
      base::cat(msg)
      
    msgf("\nExecution halted.")
    
    q("no", status=1, runLast=FALSE)
    
  }
  
}

.halt = function() {

  error = simpleError("")
  class(error) = c("myerror", class(error))
  signalCondition(error)
  
}

