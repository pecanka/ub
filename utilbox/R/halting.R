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

  msg_call = if(show_sequence) get_call_info()$message else get_call_info()$fun
  
  if(!is.null(msg)) {
    msgf(paste(c(msg, msg_call), collapse=if(missing(msg)) ' ' else '\n'))
  }
  
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()

}

#' @name stop2
#' @export
stop_quietly <- stop2

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
  err = ifelse(str_is_empty(fparent), "", "occured in function `"%p%
        fparent%p%"()` with the message: ") %p%
        ifelse(str_is_empty(msg), "(no message)", msg) %p% '\n'
  
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
#' Print an error message and stop the execution
#'
#' @description
#'
#' Prints an error message and stops the execution of code. 
#' The actual way of stopping, however, depends on whether 
#' the code is run in interactive or non-interactive modes.
halt = function(msg="") {
  
  if(interactive()) {
  
    if(!str_is_empty(msg)) base::cat('\n')
    stop(msg %p% "\nExecution halted.\n", call.=FALSE)
    
  } else {
  
    if(!str_is_empty(msg)) base::cat(msg)
    msgf("\nExecution halted.")
    q("no", status=1, runLast=FALSE)
    
  }
  
}

# returns a list, unless fmtstring is specified
# level: 1 - caller of the caller of this function; 
#        2 - its parent, 
#        3 - its grand-parent etc.
# fmtstring: return format string: %f (function), %s (source file), %l (line)
#
# example: str <- caller_info("Called from %f at %s#%l\n")
# !!! it won't work with e.g. cat(caller_info("Called from %f at %s#%l\n"))
# or cat(paste0(caller_info("Called from %f at %s#%l\n"))) !!!
# https://stackoverflow.com/q/59537482/684229
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

