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

#' Split expressions into terms
#'
#' `split_expression()` takes one or more expressions (optionally as strings)
#' and splits them up into language elements (via `expr2terms()`).
#'
#' @examples
#' split_expression(c("I(A/B/1000/(D*G))", 'I(Wealth - Health)*Age'))
#' lapply(as.list(body(optimize)), FUN=split_expression, assume_language=TRUE) %>% unlist()
#'
#' @export
split_expression = function(..., assume_language=FALSE, only_names=FALSE) {
  x = unlist(list(...), recursive=TRUE)
  x = lapply(x, FUN=expr2terms, assume_language=assume_language, only_names=only_names)
}

#' @rdname split_expression
#' @export
expr2terms = function(x, assume_language=FALSE, only_names=FALSE) {
  if(!assume_language && is.character(x)) x = str2lang(x)
  if(length(x)==1) return(as.character(x))
  x = as.list(x)
  x = unlist(lapply(x, function(y) expr2terms(y, assume_language=assume_language)))
  x = unlist(lapply(unique(append(x, names(x))), as.character))
  if(only_names) {
    is_name = sapply(x, FUN=grepl, pattern='^[a-zA-Z.]')
    x = x[is_name]
  }
  x
}
