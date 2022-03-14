#' @title
#' Pause execution / wait
#'
#' @description
#'
#' `wait()` pauses the execution until either the ENTER key (for 
#' continue) or ESC (for quit) are pressed.
#'
#' `sleep()` pauses execution for `time` seconds with (optional) 
#' announcement.
#'
#' `ask()` pauses the execution until and waits for the user to 
#' respond with one of the questions. It then evaluates whether the 
#' response was in the affirmative (as specified via `affirmative`). 
#' Only the first nmax characters of the response are compared with
#'
#' @examples
#' sleep(3)
#'
#' wait("It's nice weather outside.")
#'
#' wait("It's nice weather outside.")
#'
#' @name pause
#' @family system-related functions provided by utilbox
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

#' @rdname pause
#' @export
wait = function(...) {

  if(!is_empty(list(...))) {
    msgf(...)
  }
  
  if(interactive()) {
    msgf("Press ENTER to continue or ESC to quit ...")
    input = scan("", what = "character", nmax=1, quiet=TRUE)
  }
  
}

#' @rdname pause
#' @export
ask = function(question, answers=c('Y','N'), affirmative='Y', nmax=1, case_sensitive=FALSE) {

  msgf(question)
  
  if(interactive()) {
    msgf(paste(answers, collapse="/"),":")
    input = scan("", what = "character", nmax=nmax, quiet=TRUE)
  }
  
  f = if(!case_sensitive) tolower else I
  g = hijack(substr, start=1, stop=nmax)
  agree = g(f(affirmative)) == g(f(input))
  
  invisible(structure(agree, answer=input))
  
}

