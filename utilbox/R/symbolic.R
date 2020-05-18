#' @title
#' Process symbolic call
#
#' @title
#' `process_symbolic_call()` takes either a function, which it leave 
#' alone, or a symbolic
#' @description
#'
#' call (starting with `~`), which it turns into a function. Inspired 
#' by `tidyverse`-style function supply.
#'
#' @export
process_symbolic_call = function(call, narg) {

  if(is.function(call)) {
    return(call)
  }
  
  if(!inherits(call, 'formula')) {
    error('The call is neither function nor formula.')
  }
  
  if(!identical(call[[1]], as.symbol('~'))) 
    error('The call is not a valid formula.')
  if(!is.call(call[[2]])) 
    error('The call is not of class \"call\".')
  
  #f = list(void_x, void_xy, void_xyz, void_general)[[bound_between(narg, 1, 4)]]
  append_body(void_general, call[[2]], where='last')
     
}

#' `symbolic_call_names()` returns the names of arguments that are 
#' needed by a symbolic call.
symbolic_call_names = function(n) {
  h1('.' %p% shift(base::letters, 3), n, stop_on_greedy=TRUE)
}

##' These functions are useful when processing [`tidyverse`]-style 
##' functions.
#void = function() {}
#void_x = function(.x) {}
#void_xy = function(.x, .y) {}
#void_xyz = function(.x, .y, .z) {}

void_general = function(...) {
  args = list(...)
  names(args) = symbolic_call_names(length(args))
  list2env(args, envir=environment())
  lapply(args, function(a) if(is.data.frame(a)) list2env(as.list(a), envir=parent.frame(2)))
}
