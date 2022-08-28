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

  if(is.na(tim)) return(NA)
  
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
    th = paste0("0", th)
    drop_minute = FALSE
  }
  
  if(tm==0 && drop_minute) {
    tm = NULL 
  } else if(tm<10) {
    tm = paste0("0", tm)
  }
  
  if(!is.null(tm) && ts<10) {
    ts = paste0("0", ts)
  }
  
  paste0(paste(c(td, paste(c(th,tm,ts), collapse=":")), collapse=' days and '), 
         ifelse(is.null(tm)," seconds",""))
  
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
  
  if(announce) msgf(lead, d1)
  
  d1
  
}

#' @rdname clock
#' @export
start_clock = function(announce=TRUE, envir=utilbox_environment(), lead="", what="", out="") {
  
  d1 = clock(FALSE)
  
  if(announce) {
    msgf(lead, toupperfirst(paste0(what, "started at ")), paste0(d1), out, ".")
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
  d1 = get0(v_last, envir=envir, ifnotfound=get0(v_start, envir=envir, ifnotfound=NA))

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
  d1 = get0("._start_time_variable", envir, ifnotfound=NA)

  # Announce start and calculate runtime
  d12 = difftime(d2, d1, unit="secs")

  if(announce) {
    msgf(lead, toupperfirst(paste0(what, "started at ")), paste0(d1), out, ".")
    msgf(lead, toupperfirst(paste0(what, "finished at ")), paste0(d2), out, ".")
    msgf(lead, "Total ", tolower(what), "runtime of ", format_time(as.numeric(d12)), out,".")
  }

  invisible(c("runtime"=d12))

}

#' Timing of runtime duration
#'
#' `start_timer()`, `stop_timer()`, `read_timers()`, `stop_all_timers()` can 
#' be used to measure the length of duration of operations. Multiple nested 
#' timers are supported. `start_timer()` starts a timer (the name of the timer
#' can be specified), while `stop_timer()` stops either a specific timer
#' (when its name is supplied via `timer_name`) or the most recently started
#' timer. `stop_all_timers()` stops all existing timers. When a timer is 
#' stopped, the duration information is printed. A current duration by a
#' specific timer can be obtained via `read_timer()`. 
#'
#' @name timners
#' @export
start_timer = function(timer_name = NULL, announce = TRUE, envir = utilbox_environment(), lead = "", what = "", out = "") {
  d1 = clock(FALSE)
  if(announce) {
    msgf(lead, toupperfirst(paste0(what, "started at ")), paste0(d1), out, ".")
  }
  timers = get_timers(envir=envir)
  if(is.null(timer_name)) timer_name = timer_name_n(length(timers) + 1)
  timers = append(timers, setNames(list('start_time'=d1), timer_name))
  save_timers(timers, envir=envir)
  invisible(c(starttime = d1))
}

#' @rdname timners
#' @export
read_timer = function(timer_name = NULL, announce = TRUE, start = FALSE, envir = utilbox_environment()) {
  d2 = clock(FALSE)
  timers = get_timers(envir=envir)
  if(is.null(timer_name)) timer_name = timer_name_n(length(timers))
  d1 = timers[[timer_name]] %|||% NA_real_
  d12 = difftime(d2, d1, unit = "secs")
  if(start) start_timer()
  if (announce) {
    print(d12)
  }
  invisible(list(start_time=d1, stop_time=d2, time_diff = d12))
}

#' @rdname timners
#' @export
stop_timer = function(timer_name = NULL, announce = TRUE, envir = utilbox_environment(), lead = "", what = "", out = "") {
  td = read_timer(timer_name=timer_name, announce=announce, envir=envir)
  timers = get_timers(envir=envir)
  if(is.null(timer_name)) timer_name = timer_name_n(length(timers))
  timers[[timer_name]] = NULL
  save_timers(timers, envir=envir)
  if (announce) {
    msgf(lead, toupperfirst(paste0(what, "started at ")), paste0(td$start_time), out, ".")
    msgf(lead, toupperfirst(paste0(what, "finished at ")), paste0(td$stop_time), out, ".")
    msgf(lead, "Total ", tolower(what), "runtime of ", format_time(as.numeric(td$time_diff)), out, ".")
  }
  invisible(c(runtime = td$time_diff))
}

#' @rdname timners
#' @export
stop_all_timers = function(envir = utilbox_environment()) {
  timers = get_timers(envir=envir)
  sapply(rev(names(timers)), stop_timer, envir=envir)
  return(invisible(NULL))
}

get_timers = function(var_name = '.timers', envir = utilbox_environment()) {
  get0(var_name, envir=envir, ifnotfound=list())
}

save_timers = function(timers, var_name = '.timers', envir = utilbox_environment()) {
  assign(var_name, timers, envir=envir)
}

timer_name_n = function(n) {
  paste0('timer',n)
}
