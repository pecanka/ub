#' @title
#' Pause execution / wait
#'
#' @description
#'
#' `wait()` pauses the execution until either the <ENTER> key (for 
#' continue) or <ESC> (for quit) are pressed.
#'
#' `sleep()` pauses execution for `time` seconds with (optional) 
#' announcement.
#'
#' `ask()` pauses the execution until and waits for the user to 
#' respond with one of the questions. It then evaluates whether the 
#' response was in the affirmative (as specified via `affirmative`). 
#' Only the first nmax characters of the response are compared with
#'
#' `ask_to_confirm()` and `ask_to_confirm_with_timeout()` ask the user for 
#' confirmation with the given message. For the latter, a lack of input within
#' `timeout` seconds is considered non-confirmation, and so are the answers
#' listed in `no`.
#'
#' @examples
#' sleep(3)
#'
#' wait("It's nice weather outside.")
#'
#' wait("It's nice weather outside.")
#'
#' @family system-related functions provided by utilbox
#' @export
wait = function(..., timeout=Inf) {

  if(!is_empty(list(...))) {
    msgf(...)
  }
  
  if(interactive()) {

    msg_timeout = if(is.finite(timeout)) paste0("Waiting for ",timeout," seconds. ") else NULL
    msg = paste0(msg_timeout,"Press <ENTER> to continue, <ESC> to quit, or type 'b' <ENTER> to run browser() ...")

    input = R.utils::withTimeout(ask_to_confirm(msg), timeout=timeout, onTimeout = 'silent')
    
    if(nzchar(input) && any(sapply(c('b','"b"',"'b'"), identical, tolower(input)))) 
      do.call('browser', list(), envir=parent.frame())

  } else {
  
    msgf("`wait()` requires an interactive session.")
    
  }
  
}

#' @rdname wait
#' @export
sleep = function(time, announce=TRUE) {
  
  if(announce) {
    cat0("Sleeping for ",time," seconds ... ")
  }
  
  Sys.sleep(time)
  
  if(announce) {
    msgf("done.")
  }
  
  invisible(NULL)
  
}

#' @rdname wait
#' @export
ask_to_confirm = function (msg="Press <ENTER> to continue or <ESC> to quit ...", ...) {

  if(!interactive()) 
    return(invisible(TRUE))
   
  msgf(msg, ...)
  readLines(n = 1)
 
}

#' @rdname wait
#' @export
ask_to_confirm_with_timeout = function(msg='', timeout=10, no=NULL) {
  answer = R.utils::withTimeout(ask_to_confirm(msg), timeout=timeout, onTimeout = 'silent')
  !is.null(answer) && !isTRUE(answer %in% no)
}


#' @rdname wait
#' @export
ask = function(question, answers=c('Y','N'), affirmative='Y', nmax=1, case_sensitive=FALSE) {

  msgf(question)
  
  if(interactive()) {
    msgf(paste(answers, collapse="/"),":")
    input = readLines(n = 1)
  }
  
  f = if(!case_sensitive) tolower else identity
  g = hijack(substr, start=1, stop=nmax)
  
  agree = g(f(affirmative)) == g(f(input))
  
  invisible(structure(agree, answer=input))
  
}

#' Listen for signals to pause execution
#'
#' When called, `listen_for_pause()` looks for the existence of the specified
#' file (.Rgui_pause.txt in the user's home directory by default) and checks
#' the contents of the file for the presence of the current process ID. If the
#' process ID is found, the specified pause function (via `pause_fun_name`,
#' set to 'browser' by default) is called in the specified environment (`envir`).
#' Otherwise, execution continues as normal. The return value indicates whether
#' the process ID was found (1), not found (0), or the file does not exist (-1).
#'
#' @export
listen_for_pause = function(pid=Sys.getpid(), file=file.path('~','.Rgui_pause.txt'), pause_fun_name='browser', 
    envir=parent.frame(), trace=FALSE) {

  if(!file.exists(file)) return(invisible(-1))
 
  if(trace)
    message("Checking for process ID ",pid," in the file '",file,"' ...")
 
  pid_found = try({
    PIDs = scan(file, what='character', quiet=TRUE)
    pid %in% PIDs
  }, silent=TRUE)
 
  if(isTRUE(pid_found)) {
    message("Process ID ",pid," found in the file '",file,"'. Launching browser ...")
    message("To disable pausing on next check use `remove_pid_from_pause_list()`.")
    do.call(pause_fun_name, list(), envir=envir)
  }
 
  invisible(as.numeric(isTRUE(pid_found)))
 
}

remove_pid_from_pause_list = function(pid=Sys.getpid(), file=file.path('~','.Rgui_pause.txt'), remove_all=TRUE,
  delete_empty=FALSE, trace=TRUE) {

  if(!file.exists(file)) return(invisible())

  if(trace)
    message("Removing ",if(remove_all) "all occurences" else "the first occurence",
            " of process ID ",pid," from the file '",file,"' ... ", appendLF=FALSE)
 
  pid_removed = try({
    PIDs = readLines(file, warn=FALSE)
    if(pid %in% PIDs) {
      PIDs = if(remove_all) PIDs[PIDs %nin% pid] else PIDs[PIDs %nin% pid | duplicated(PIDs)]
      if(length(PIDs)>0) {
        writeLines(PIDs, file)
      } else if(delete_empty) {
        file.remove(file)
      } else {
        cat('', file=file)
      }
    }
  }, silent=!TRUE)
 
  result = is.null(pid_removed) || isTRUE(pid_removed)
 
  if(result) {
    if(trace) message('done.')
  } else {
    message('')
    warning("Something went wrong when removing process ID ",pid," from the file '",file,"'!", immediate.=TRUE)
  }

  invisible(result)
 
}