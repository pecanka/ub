#' Kidnap the warning function in base
#'
#' `function_kidnap()` modifies the code of the supplied function.
#' Kidnapping in this context means that the function will ask the
#' user for input before proceeding with its code. The original code 
#' of the kidnapped function is backed up in the attribute 'original_function'
#' of the kidnapped function.
#'
#' `function_restore()` restores the original code of a kidnapped function.
#'
#' `stop_function_kidnap()` is a shortcut for kidnapping the function 
#' `base::stop`, while `stop_function_restore()` restores it.
#'
#' `warning_function_kidnap()` and `warning_function_restore()` perform
#' the same action for the warning functions `base::warning` and
#' `base::.signalSimpleWarning`.
#'
#' @export
function_kidnap = function(fun_name, where=parent.frame(), msg_argument, restore_call) {

  if(is.character(where)) {
    where_name = paste0("in the namespace '",where,"'")
    where = asNamespace(where)
  } else if(identical(where, parent.frame())) {
    where_name = "in the parent frame"
  } else {
    where_name = "in the specified namespace"
  }

  if(!is.null(attr(get(fun_name, envir=where), 'original_function'))) {
    message('The function `',fun_name,'` had already been modified.')
    return(invisible())  
  }

  code = c("message('\n*************** KIDNAPPING IN EFFECT ***************');",
           paste0("message('The function `",fun_name,"()` has been invoked'", 
                  if(!missing(msg_argument)) paste0(", ' (with message \\'', ",msg_argument,",'\\').');")),
           "message('Because the function had been kidnapped, it behaves differently.');",
           "repeat {",
           "message('You have the following options:');",
           paste0("message('  - input \\'i\\' and press Enter to ignore the `",fun_name,
                  "()` call and continue execution as if it was never called');"),
           paste0("message('  - input \\'b\\' and press Enter to enter the browser inside the `",
                  fun_name,"()` call');"),
           paste0("message('  - press Enter to continue the execution of the `",fun_name,"()` call as normal');"),
           "  input = scan('', what = 'character', nmax = 1, quiet = TRUE);",
           "  input = if(length(input)==0) '' else tolower(substr(input, 1, 1));",
           paste0("if(input == 'i') {
                      message('Invokation of `",fun_name,"()` is being ignored and the execution is continuing...');
                      return(invisible(NULL));
                  } else if(input == 'b') {
                      message('Browser will be entered. After continuing via \\'c\\' you will be asked again what to do.\nCalling `browser()`...');
                      browser();
                  } else {
                      message('Continuing with normal execution of `",fun_name,"()`...\n');
                      break;
                  }"),
           "}"
          )

  code = paste('{', paste(code, collapse='\n'), '}')
  call = base::str2lang(code)
  fun_polite = utilbox::append_body(get(fun_name, envir=where), call, where='first')

  attr(fun_polite, 'original_function') = get(fun_name, envir=where)

  message('Modifying the function `',fun_name,'` ...')
  unlockBinding(fun_name, env=where)
  assign(fun_name, fun_polite, envir=where)
  lockBinding(fun_name, env=where)
  
  if(identical(get(fun_name, envir=where), fun_polite)) {
    if(missing(restore_call)) restore_call = 'function_restore()'
    message('Function `',fun_name,'` has been modified (',where_name,').')
    if(!is.null(restore_call)) message('You can restore the original version(s) using `',restore_call,'`.')
  } else {
    message('Something went wrong during the modification of `',fun_name,'`!')
  }
  
}

#' @rdname function_kidnap
#' @export
function_restore = function(fun_name, where=parent.frame()) {

  if(is.character(where)) where = asNamespace(where)

  if(is.null(attr(get(fun_name, envir=where), 'original_function'))) {
    message('The function `',fun_name,'` does not appear to have been modified.')
    return(invisible())
  }

  message('Restoring the function `',fun_name,'` ...')
  fun_original = attr(get(fun_name, envir=where), 'original_function')
  unlockBinding(fun_name, env=where)
  assign(fun_name, fun_original, envir=where)
  lockBinding(fun_name, env=where)
  
  if(identical(get(fun_name, envir=where), fun_original)) {
    message('The original version of the function `',fun_name,'` has been restored.')
  } else {
    message('Something went wrong when restoring of `',fun_name,'`!')
  }

}

#' @rdname function_kidnap
#' @export
stop_function_kidnap = function() {
  function_kidnap('stop', 'base', msg_argument='...', restore_call='stop_function_restore()')
}

#' @rdname function_kidnap
#' @export
stop_function_restore = function() {
  function_restore('stop', 'base')
}

#' @rdname function_kidnap
#' @export
warning_function_kidnap = function() {
  function_kidnap('warning', 'base', msg_argument='...', restore_call=NULL)
  function_kidnap('.signalSimpleWarning', 'base', msg_argument='msg', restore_call='warning_function_restore()')
}

#' @rdname function_kidnap
#' @export
warning_function_restore = function() {
  function_restore('warning', 'base')
  function_restore('.signalSimpleWarning', 'base')
}
