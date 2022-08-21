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
stop2 <- function(msg='Execution stopped.', show_sequence=TRUE) {

  msg_call = if(show_sequence) {
    get_call_info()$message 
  } else {
    get_call_info()$fun
  }
  
  if(!is.null(msg)) {
    msgf(paste(c(msg, msg_call), collapse=if(missing(msg)) ' ' else '\n'))
  }
  
  # Disable printing of error messages
  opt = options(show.error.messages = FALSE)
  
  # But make sure it gets restored to the previous value upon exit
  on.exit(options(opt))
  
  stop()

}

#' @name stop2
#' @export
stop_quietly <- stop2

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

#' Print caller information
#'
#' `caller_info()` is a function that prints the caller information,
#' i.e. the sequence of calling fuctions and the invocation locations.
#'
#' Taken from: https://stackoverflow.com/q/59537482/684229
#'
#' Serves as inspiration for the function `get_call_info()`.
#'
#' @export
caller_info <- function (fmtstring = NULL, level = 1) {
    
  x <- .traceback(x = level + 1)

  i <- 1
  repeat { # loop for subexpressions case; find the first one with source reference
      srcref <- getSrcref(x[[i]])
      if (is.null(srcref)) {
          if (i < length(x)) {
              i <- i + 1
              next;
          } else {
              warning("caller_info(): not found\n")
              return (NULL)
          }
      }
      srcloc <- list(fun = getSrcref(x[[i+1]]), file = getSrcFilename(x[[i]]), line = getSrcLocation(x[[i]]))
      break;
  }

  if (is.null(fmtstring))
      return (srcloc)

  fmtstring <- sub("%f", paste0(srcloc$fun, collapse = ""), fmtstring)
  fmtstring <- sub("%s", srcloc$file, fmtstring)
  fmtstring <- sub("%l", srcloc$line, fmtstring)
  fmtstring
  
}

#' Get the details about where a call came from
#'
#' `get_call_info()` returns a list with information about the calling sequence.
#' Optionally, a message with the information is directly printed.
#'
#' `call_info()` prints and returns the information about the calling sequence
#' (obtained using `get_call_info()`.
#'
#' @returns A list with the following elements:
#' `fun` ... calling function
#' `dir` ... path to the script file
#' `file` ... name of the script file
#' `line` ... line in the script where called
#' `msg` ... message with the sequence
#'
#' @family halting utilities provided by utilbox
#' @export
call_info <- function(print_msg=TRUE, level = 1) {

  info = get_call_info(FALSE, level)
  
  if(print_msg) msgf(info$message2)
 
  return(invisible(info))
 
}

#' @rdname call_info
#' @export
get_call_info <- function(print_msg=FALSE, level = 1) {

  K <- .traceback(x = level + 1)
  
  refs = sapply(K, getSrcref)
  w = which(!sapply(refs, is.null))
  
  if(length(K)==0) {
    warning("caller_info(): No suitable reference found.")
    return(NULL)
  }
  
  K = rev(K[w])
  
  cals = sapply(K, function(x) as.character(getSrcref(x)))
  dirs = sapply(K, getSrcDirectory)
  fils = sapply(K, getSrcFilename)
  lins = sapply(K, getSrcLocation)
  locs = paste0(file.path(dirs, fils),':',lins)

  msg1 = paste0('Invocation sequence: ',paste0(paste0(cals,'@',locs), collapse=' -> '))
  msg2 = paste0('Invocation sequence:\n    -> ',paste0(paste0(cals,'@',locs), collapse='\n    -> '))
  
  srcloc = list(calls=cals, wdir=getwd(), srcdir=dirs, srcfile=fils, srcline=lins, srcloc=locs, message=msg1, message2=msg2)
 
  if(print_msg) msgf(msg2)
 
  return(invisible(srcloc))
 
}

