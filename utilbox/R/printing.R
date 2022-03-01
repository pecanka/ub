#' @title
#' Return the output of a print call as string
#'
#' @description
#'
#' Returns the output of a print call as string. Useful for saving 
#' the output of a print call into a variable. Simply an alias for 
#' [`utils::capture.output`].
#'
#' @export
print2var = function(fun, file=NULL) {
  utils::capture.output(print(fun))
}
  
#' @title
#' Print text
#'
#' @description
#'
#' `cat0()` prints text to console or file via [`base::cat()`]. It is 
#' basically an alias for [`base::cat()`] that automatically flushes the 
#' console buffer (when `flush=TRUE`). This only affects buffered output 
#' enabled R sessions (e.g. Rgui on Windows)
#'
#' `catn()` is an alias for `cat0()` which in the default setting 
#' adds a single end-of-line symbol (specified in `eol`, which `\\n` by 
#' default) at the end of the strings.
#'
#' `catnn()` extends `catn` to also use `eol` as the default value 
#' for `sep`.
#'
#' All of these allow a smart way of flushing the console.
#'
#' `msg()` prints a message with more control by user. Optionally, it can 
#' stop the execution (when `quit=TRUE`) or wait for user interaction 
#' (when `wait=TRUE`).
#'
#' `warn()` is an alias for `msg()` which appends the string 
#' \"WARNING\" to the printed message.
#'
#' `note()` prints a note and a request to wait (optional).
#'
#' `msgf()` is a wrapper around `base::message()` which flushes the output 
#' by default (for interactive sessions).
#'
#' `.fc()` is an alias for [`utils::flush.console()`].
#'
#' `flush_console()` is a more adjustable way of flushing the console. 
#' It allows to flush only on every n-th print, where n is set by the 
#' value in `flush_cycle`.
#'
#' @family printing function provided by utilbox
#' @export
cat0 = function(..., file="", sep="", fill=FALSE, pad=FALSE, padding=' ', pad_side='right', 
  labels=NULL, append=FALSE, flush=TRUE, flush_cycle=1, envir=utilbox_environment(), 
  fill_using_options=FALSE) {
  
  # define isFALSE which is not found in older versions of package base 
  isFALSE = function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x
  
  if(isFALSE(pad) && (isFALSE(fill) || fill_using_options)) {
    
    base::cat(..., file=file, sep=sep, fill=as.numeric(fill)>0, labels=labels, append=append)
    
    x = collapse0(..., sep=sep)
    
  } else {

    x = do.call(collapse0, list(list(...), sep=sep))
    if(!isFALSE(pad)) {
      x = str_lengthen(x, pad, side=pad_side, padding=padding)
    }
    if(!isFALSE(fill)) {
      x = str_break(x, if(isTRUE(fill)) options()$width else fill)
    }
    
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
catnn = function(..., sep='\n', fill=TRUE) {
  catn(..., sep=sep, fill=fill)
}

#' @rdname cat0
#' @export
msgf = function(..., flush=interactive()) {
  base::message(...)
  if(flush) utils::flush.console()
}

#' @rdname cat0
#' @export
msg = function(t, ..., lead="", sep="", quit=FALSE, wait=FALSE, skip1=0, skip2=0, flush=TRUE) {
  cat0(rep("\n", skip1), lead, flush=FALSE)
  cat0(t, ...,"\n", sep=sep, flush=FALSE)
  cat0(rep("\n", skip2), flush=flush)
  if(quit) halt() 
  if(wait) wait() 
}

#' @rdname cat0
#' @export
warn = function(..., skip1=1, skip2=1) {
  msg(..., lead="WARNING: ", skip1=skip1, skip2=skip2)
}

#' @rdname cat0
#' @export
note = function(...) {
  msg(..., lead="NOTE: ")
}

#' @rdname cat0
#' @export
.fc = utils::flush.console

#' @rdname cat0
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

