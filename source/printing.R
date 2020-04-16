#' Print text
#'
#' Prints text to console or file via [base::cat]. Allows for optional
#'
#' `cat0` is basically an alias for [base::cat] that automatically 
#' flushes the console buffer (when `flush=TRUE`). This only affects 
#' buffered output enabled R sessions (e.g. Rgui on Windows)
#'
#' `catn` is an alias for `cat0` which in the default setting adds 
#' a single end-of-line symbol (specified in `eol`, which `\\n` by
#' default) at the end of the strings.
#'
#' `catnn` extends `catn` to also use `eol` as the default value for 
#' `sep`.
#'
#' All of these allow a smart way of flushing the console.
#'
#' @name cat0
#' @export
cat0 = function(..., file="", sep="", fill=FALSE, labels=NULL, append=FALSE, 
  flush=TRUE, flush_cycle=1, envir=utilbox_environment(), fill_using_options=FALSE) {
  
  if(isFALSE(fill) || fill_using_options) {
    
    base::cat(..., file=file, sep=sep, fill=as.numeric(fill)>0, labels=labels, append=append)
    
    x = collapse0(..., sep=sep)
    
  } else {

    x = do.call(collapse0, list(list(...), sep=sep))
    x = str_wrap(x, if(isTRUE(fill)) options()$width else fill)

    base::cat(x, file=file, append=append)
    
  }
  
  flush_console(flush, flush_cycle, envir=envir)
  
  y = nchar(x) - str_last_occurence(x, '\n', 0)
    
  invisible(y)
  
}

#' @rdname cat0
#' @export
catn = function(..., file="", sep="", fill=FALSE, labels=NULL, append=FALSE, eol='\n') {
  cat0(..., eol, file=file, sep=sep, fill=fill, labels=labels, append=append)
}

#' @rdname cat0
#' @export
catnn = hijack(catn, sep='\n', fill=TRUE)

#' Flush buffer 
#'
#' Alias for \code{flush.console()}
#'
#' @export
.fc = utils::flush.console

#' Prints a warning and either stops execution or waits depending on arguments
#' @export
msg = function(t, ..., lead="", sep="", quit=FALSE, wait=FALSE, skip1=0, skip2=0, flush=TRUE) {
  cat0(rep("\n", skip1), lead, flush=FALSE)
  cat0(t, ...,"\n", sep=sep, flush=FALSE)
  cat0(rep("\n", skip2), flush=flush)
  if(quit) halt() 
  if(wait) wait() 
}

#' Print a warning and either stop execution or wait depending on arguments
#' @export
warn = function(..., skip1=1, skip2=1) {
  msg(..., lead="WARNING: ", skip1=skip1, skip2=skip2)
}

#' Print a note and optionally wait
#' @export
note = function(...) {
  msg(..., lead="NOTE: ")
}

#' Flush the console
#'
#' Flushes the console in a smart way.
#'
#' @export
flush_console = function(flush=TRUE, flush_cycle=1, envir=utilbox_environment()) {

  ## Define a counter of prints since the last flush
  if(!exists(".flush_counter", envir=envir)) {
    try(assign(".flush_counter", 0, envir=envir), silent=!TRUE)
  }
  
  ## Increase the flush counter by 1
  assign(".flush_counter", get(".flush_counter", envir=envir) + 1, envir=envir)
  
  ## If either flush is TRUE or the counter has reached the limit, flush the console
  ## and reset the counter to zero
  if(flush || get(".flush_counter", envir=envir)>=flush_cycle) {
    utils::flush.console()
    assign(".flush_counter", 0, envir=envir)
  }
  
}

