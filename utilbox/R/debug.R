#' Kidnap the stop and warning functions
#'
#' `kidnap_stop_function()` kidnaps the function `base::stop`.
#' In this context, kidnapping of a function means that the function 
#' is modified so that it asks the user for input before proceeding 
#' with its code. The original code of the kidnapped function is backed 
#' up in the attribute `original_function` of the kidnapped function
#' so that the original function can be restored. 
#'
#' `restore_stop_function()` restores the original version of the stop
#' function.
#'
#' `kidnap_warning_function()` and `restore_warning_function()` perform
#' the same action for the warning functions `base::warning` and
#' `base::.signalSimpleWarning`.
#'
#' `function_kidnap()` is the workhorse that does the actual kidnapping,
#' which modified the function **in place**.
#'
#' @name kidnap_function
#' @export
kidnap_stop_function = function(quietly=FALSE) {
  result = function_kidnap('stop', 'base', msg_argument='...', 
                           restore_call='restore_stop_function()', 
                           quietly=quietly)
  invisible(result)
}

#' @rdname kidnap_function
#' @export
restore_stop_function = function(quietly=FALSE) {
  result = function_restore('stop', 'base', quietly=quietly)
  invisible(result)
}

#' @rdname kidnap_function
#' @export
kidnap_warning_function = function(quietly=FALSE) {
  res1 = function_kidnap('warning', 'base', msg_argument='...', restore_call=NULL, 
                         quietly=quietly)
  res2 = function_kidnap('.signalSimpleWarning', 'base', msg_argument='msg', 
                         restore_call='restore_warning_function()', 
                         quietly=quietly)
  invisible(res1 && res2)
}

#' @rdname kidnap_function
#' @export
restore_warning_function = function(quietly=FALSE) {
  res1 = function_restore('warning', 'base', quietly=quietly)
  res2 = function_restore('.signalSimpleWarning', 'base', quietly=quietly)
  invisible(res1 && res2)
}

#' @rdname kidnap_function
#' @export
function_kidnap = function(fun_name, envir=parent.frame(), msg_argument, restore_call, remodify=TRUE, 
    quietly=FALSE) {

  calls = c(
    "message('\n*************** KIDNAPPING IN EFFECT ***************');",
    paste0("message('The function `",fun_name,"()` has been invoked'",
          if(!missing(msg_argument)) paste0(", ' (with message \\'', ",msg_argument,",'\\').');")),
           "message('Because the function had been kidnapped, it behaves differently.');",
           "repeat {",
           "  message('You have the following options:');",
    paste0("  message('  - input \\'i\\' and press ENTER to ignore the `",fun_name,
           "          ()` call and continue as if it was never called');"),
    paste0("  message('  - input \\'b\\' and press ENTER to enter the browser inside the `",
                      fun_name,"()` call');"),
    paste0("  message('  - press ENTER to continue the execution of the `",fun_name,
           "          ()` call as normal');"),
           "  input = scan('', what='character', nmax=1, quiet=TRUE);",
           "  input = if(length(input)==0) '' else tolower(substr(input, 1, 1));",
    paste0("if(input == 'i') {
              message('Invokation of `",fun_name,"()` is being ignored, continuing execution ...');
              return(invisible(NULL));
          } else if(input == 'b') {
              message('Launching browser. After continuing via \\'c\\' you will be',
                      ' asked again what to do.\nCalling `browser()`...');
              browser();
          } else {
              message('Continuing with normal execution of `",fun_name,"()`...\n');
              break;
          }"),
    "}"
  )

  function_modify_in_place(fun_name, calls=calls, envir=envir, where='first', 
      restore_call=restore_call, remodify=remodify, quietly=quietly)
  
}

#' @title
#' Toggle dumping on frames on error
#'
#' @description
#' `.roe()` (or it alias `toggle_dump_on_error()`) toggle between dumping of
#' frames when an error is encoutered. This is done by setting `options()$error`
#' to a version of `utils::dump.frames()`. Optionally, the dumping is done to
#' to file (when `to_file=TRUE`) and/or includes the global environment
#' (when `include_GlobalEnv=TRUE`).
#'
#' @export
toggle_dump_on_error = function(dumpto='last.dump', to_file=FALSE, include_GlobalEnv=FALSE) {

  if(is.null(options()$error)) {
    message('Enabling frame dumping on error ...')
    if(!interactive()) {
      to_file = include_GlobalEnv = TRUE
    }
    call_text = paste0("options(error=quote(dump.frames(dumpto='",dumpto,"', to.file=",to_file,
                       ", include.GlobalEnv=",include_GlobalEnv,")))")
    eval(str2lang(call_text))
  } else {
    message('Disabling frame dumping on error ...')
    options(error = NULL)
  }
  
}

#' @rdname toggle_dump_on_error
#' @export
.doe = toggle_dump_on_error

#' Browser with timeout
#'
#' This is a wrapper around `base::browser()` that asks the user for confirmation 
#' before calling `base::browser()`. The confirmation must happen within `timeout` 
#' seconds, otherwise no browser is launched and the execution continues as normal.
#'
#' @export
browser_with_timeout = function(text = "", ..., condition = NULL, expr = TRUE, skipCalls = 0L, timeout = 10, sep=' ') {

  message("*** BROWSER ***")
  
  caller_info(TRUE, level = 2, warn = FALSE)
 
  if(interactive() && is.finite(timeout)) {

    msg = paste0(
      "*** BROWSER WITH TIMEOUT ***\n",
      "Press ENTER in the next ",timeout," seconds to call `base::browser()`.\n",
      "Otherwise the execution will continue as normal.\n",
      "Input 'c' and press ENTER to continue immediately.\n",
      "Pressing ESC will terminate the execution."
    )
    
    confirmed = ask_to_confirm_with_timeout(msg, timeout, no = c('c','C'))
   
    if(confirmed) {
      message("REQUEST CONFIRMED BY USER. LAUNCHING `base::browser()` ...")
    } else {
      message("REQUEST NOT CONFIRMED BY USER. CONTINUING EXECUTION AS NORMAL ...")
      return(invisible(NULL))
    }
   
  }
 
  text = do.call(paste, list(text, ..., sep = sep))

  do.call(.Primitive("browser"), list(text = text, condition = condition, expr = exp,
                                      skipCalls = skipCalls + 2L), envir = parent.frame())
 
}

#' Insert a browser into a pipe
#'
#' A function that is intended to be inserted as a step
#' in a pipe sequence during debugging to explore the state
#' of the input object. It only calls the function `base::browser()`
#' and returns the input object unmodified. Exit the browser
#' with the call `cont` (or `c` for short). See `?browser` for more
#' information.
#'
#' @examples
#' mtcars %>% browse() %>% print()   # use `.`, `cyl`, tidyverse-style
#' 1:10 %>% browse() %>% print()     # use `x` to get the pipe input
#'
#' @export
browse = function(x, ...) {
  UseMethod('browse')
}

browse.data.frame = function(data, ...) {
  x %>% mutate({base::browser()}, ...)
  x
}

browse.default = function(x, ...) {
  base::browser()
  x
}
