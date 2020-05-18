#' @title
#' Format time
#'
#' @description
#'
#' `format_time()` takes time in seconds and format it to natural 
#' language. Useful for printing runtime. The hour information can be 
#' optionally dropped for times under one hour (via `drop_hour=TRUE`). 
#' Analogously for minutes via `drop_minute=TRUE`. The day information 
#' is dropped automatically when it is not relevant (i.e. non-zero).
#'
#'
#' @examples
#' # short runtimes
#' format_time(23.23454)
#' format_time(23.23454, drop_hour=TRUE)
#' # longer
#' format_time(9225.2342)
#' # days long
#' format_time(12329225.2342)
#'
#' @export
format_time = function(tim, drop_hour=TRUE, drop_minute=TRUE, prec=3) {

  td = tim %/% 86400
  tim = tim - td * 86400
  th = tim %/% 3600
  tim = tim - th * 3600
  tm = tim %/% 60
  tim = tim - tm * 60
  ts = round(tim, prec)
  
  if(td==0) {
    td = NULL
  } else {
    drop_hour = drop_minute = FALSE
  }
  
  if(th==0 && drop_hour) {
    th = NULL 
  } else if(th<10) {
    th = "0" %p% th
    drop_minute = FALSE
  }
  
  if(tm==0 && drop_minute) {
    tm = NULL 
  } else if(tm<10) {
    tm = "0" %p% tm
  }
  
  if(!is.null(tm) && ts<10) {
    ts = "0" %p% ts
  }
  
  collapse0(c(td, collapse0(c(th,tm,ts),sep=":")), sep=' days and ') %p% ifelse(is.null(tm)," seconds","")
  
}

#' @title
#' Clock control functions
#'
#' @description
#'
#' Approximate timing of code execution. Useful to announce at what 
#' times different steps in a (long) running script were started and 
#' finished. The times are only approximate because when `announce=TRUE` 
#' there is some time spent on the actual printing of the information 
#' and this time (~0.1s) is not subtracted from the runtimes.
#'
#' `start_clock()` starts the runtime clock.
#'
#' `read_clock()` retrieves the runtime clock.
#'
#' `stop_clock()` stops the runtime clock.
#'
#' `clock()` gets the current system time.
#'
#' @examples
#' clock()
#'
#' # start the clock
#' start_clock()
#' # do something that takes time
#' sleep(1.1)
#' # check how long it took
#' read_clock()
#' # do something else
#' sleep(1.5)
#' # another check how long it took and final stopping
#' read_clock()
#' stop_clock()
#'
#' @export
clock = function(announce=TRUE, lead="", digs=3) {
  
  odigs = options()$digits.secs
  options(digits.secs=digs)
  d1 = paste0(Sys.time())
  options(digits.secs=odigs)
  
  if(announce) catn(lead, d1)
  
  invisible(c("time"=d1))
  
}

#' @rdname clock
#' @export
start_clock = function(announce=TRUE, envir=utilbox_environment(), lead="", what="", out="") {
  
  d1 = clock(FALSE)
  
  if(announce) {
    catn(lead,toupperfirst(what %p% "started at "), paste0(d1),out,".")
  }

  v_start = '._start_time_variable'
  v_last = '._last_check_time_variable'

  assign2(c(v_start, v_last), d1, envir=envir)
  
  invisible(c("starttime"=d1))
  
}

#' @rdname clock
#' @export
read_clock = function(announce=TRUE, envir=utilbox_environment()) {

  v_start = '._start_time_variable'
  v_last = '._last_check_time_variable'

  d2 = clock(FALSE)
  d1 = get2(v_last, envir, get2(v_start, envir, NA))

  # Calculate current runtime
  d12 = difftime(d2, d1, unit="secs")
  
  if(announce) print(d12)
  
  # Assign the current time as the last check time
  assign(v_last, d2, envir=envir)

  invisible(c("time difference"=d12))
  
}

#' @rdname clock
#' @export
stop_clock = function(announce=TRUE, envir=utilbox_environment(), lead="", what="", out="") {
  
  # Read the current clock and recall start
  read_clock(announce)
  d2 = clock(FALSE)
  d1 = get2("._start_time_variable", envir, NA)

  # Announce start and calculate runtime
  d12 = difftime(d2, d1, unit="secs")

  if(announce) {
    catn(lead,toupperfirst(what %p% "started at "), paste0(d1), out, ".")
    catn(lead,toupperfirst(what %p% "finished at "), paste0(d2), out, ".")
    catn(lead,"Total ", tolower(what), "runtime of ", format_time(as.numeric(d12)), out,".")
  }

  invisible(c("runtime"=d12))

}

