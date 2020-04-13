#' Format time
#'
#' Take time in seconds and format it to natural language
#' @export
format_time = function(tim, prec=3, drop_hour=FALSE) {
  th = tim %/% 3600
  tim = tim - th * 3600
  tm = tim %/% 60
  tim = tim - tm * 60
  ts = round(tim, prec)
  
  if(th==0 && drop_hour) th = NULL else if(th<10) th = paste0("0",th)
  if(is.null(th)) {
    if(tm==0) tm = NULL else if(tm<10) tm = paste0("0",tm)
  } else {
    if(tm<10) tm = paste0("0",tm)
  }
  if(!is.null(tm) && ts<10) ts = paste0("0",ts)
  
  paste0(paste0(c(th,tm,ts),collapse=":"),ifelse(is.null(tm)," seconds",""))
  
}

#' Clock control functions
#'
#' \code{clock()} gets the current system time and optionally prints it on screen
#' \code{start_clock()} starts the runtime clock
#' \code{stop_clock()} stops the runtime clock
#' \code{read_clock()} retrieves the runtime clock
#'
#' @export
clock = function(announce=TRUE, with_date=TRUE, lead="", digs=3, eol="\n") {
  
  odigs = options()$digits.secs
  options(digits.secs=digs)
  d1 = paste0(Sys.time())
  options(digits.secs=odigs)
  
  if(!with_date) d1 = sub(".*[ ]", "", d1)
  if(announce) cat0(lead, d1, eol)
  
  invisible(c("time"=d1))
  
}

#' @name clock
#' @export
start_clock = function(print=TRUE, envir=parent.frame(), lead="", what="") {
  
  #d1 = Sys.time()
  d1 = clock(announce=print, lead=paste0(lead,toupperfirst(what,"started at ")))
  #if(print) cat(lead,toupperfirst(what,"started at ",paste0(d1),".\n"), sep="")
  
  assign("._start_time_variable", d1, envir=envir)
  assign("._last_check_time_variable", d1, envir=envir)  
  
  invisible(c("starttime"=d1))
  
}

#' @name clock
#' @export
read_clock = function(envir=utilbox_namespace()) {

  #d2 = Sys.time()
  d2 = clock(FALSE)
  d1 = if(exists("._last_check_time_variable", envir=envir))
          get("._last_check_time_variable", envir=envir) else
       if(exists("._start_time_variable", envir=envir))
          get("._start_time_variable", envir=envir) else
       NULL

  # Calculate current runtime
  d12 = if(!is.null(d1)) difftime(d2, d1, unit="secs") else NA
  
  # Assign the current time as the last check time
  assign("._last_check_time_variable", d2, envir=envir)

  return(d12)
}

#' @name clock
#' @export
stop_clock = function(d1=NULL, d12=NULL, start_print=FALSE, end_print=TRUE, 
  runtime_print=TRUE, envir=utilbox_namespace(), lead="", what="") {
  
  # Read the current clock and recall start
  #d2 = Sys.time()
  d2 = clock(FALSE)
  if(is.null(d1) && exists("._start_time_variable", envir=envir))
    d1 = get("._start_time_variable", envir=envir)

  # Announce start and calculate runtime
  if(!is.null(d1)) {
    if(start_print) 
      cat0(lead,toupperfirst(what,"started at "),paste0(d1),".\n")
    if(is.null(d12)) 
      d12 = difftime(d2,d1,unit="secs")
  }
  
  # Announce end
  if(end_print) 
    cat0(lead,toupperfirst(what,"finished at "),paste0(d2),".\n")
  if(!is.null(d12) && runtime_print)
    cat0(lead,"Total ",tolower(what),"runtime of ",format_time(as.numeric(d12)),".\n")

  invisible(c("runtime"=d12))

}

