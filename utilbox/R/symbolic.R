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
    error('The call is not of class "call".')
  
  #f = list(void_x, void_xy, void_xyz, void_general)[[bound_between(narg, 1, 4)]]
  append_body(void_general, call[[2]], where='last')
     
}

#' Arguments for a symbolic call
#'
#' `symbolic_call_names()` returns the names of arguments that are 
#' needed by a symbolic call. Specifically, it returns the first `n`
#' elements of the sequence
#'
#' @examples
#' symbolic_call_names(10)
#' symbolic_call_names(100)
#'
#' @export
symbolic_call_names = function(n) {
  #h1('.' %p% rotate(base::letters, 3), n, stop_on_greedy=TRUE)
  nl = length(letters)
  h1('.' %p% rotate(base::letters, 3), n) %p% h1(rep(c("",1:ceiling(n/nl)), e=nl), n)
}

#' These functions are useful when processing `tidyverse`-style
#' functions.
void_general = function(...) {
  args = list(...)
  names(args) = symbolic_call_names(length(args))
  list2env(args, envir=environment())
  lapply(args, function(a) if(is.data.frame(a)) list2env(as.list(a), envir=parent.frame(2)))
}
