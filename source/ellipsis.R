#' @title
#' Processing of ellipsis and call arguments
#'
#' @description
#' `dots_to_nlist()` returns the contents of the ellipsis of a parent function in a 
#' nice form. In calls with unnamed arguments it extracts the names of 
#' variables supplied in the call and uses them to assign names to the 
#' list produced from the ellipsis (`...`). With `keep_symbolic=TRUE`, 
#' `dots_to_nlist()` returns only the names of the supplied arguments
#' in the symbolic form. A call to `as.character()` on the output then
#' gives the names in character form. Using `keep_symbolic=TRUE` is handy
#' for instance for processing symbolic calls (e.g. column names to sort by
#' in `sort_df()` or packages to load in `llib()`).
#'
#' `args_to_nlist()` returns all arguments supplied to or simply defined in
#' the call of the parent function in the form of a `named list`. In order 
#' for it to work properly, the call to `args_to_nlist()` must be the first 
#' call inside the parent function, otherwise the call to `ls` might return 
#' also objects that did not exist upon the call to the function.
#'
#' `argvals_to_nlist()` extracts the values supplied as arguments during
#' the call to the parent function.
#'
#' @examples
#' f = function(...) { dots_to_nlist() }
#' f(name='John', kids=c('Peter','Jake'))
#' name = 'John'; age = 35
#' f(name, age)
#'
#' g = function(name, other, only_called=TRUE) { args_to_nlist(only_called) }
#' g('John', c('Peter','Jake'))
#'
#' @name call_to_list
#' @family coding-related functions provided by utilbox
#' @export
dots_to_nlist = function(keep_symbolic=FALSE, flatten=FALSE, assign_names=TRUE, names, envir=parent.frame()) {

  args = eval(quote(match.call(expand.dots=FALSE)$`...`), envir=envir)
  
  if(keep_symbolic) 
    return(args)
    
  vals = eval(bquote(list(...)), envir=envir)
  
  if(flatten) {
    vals = list_flatten(vals)
  }
  
  if(assign_names) {
    unempty_names(vals, if(missing(names)) as.character(args) else names)
  }
  
}

#' @rdname call_to_list
#' @export
args_to_nlist = function(envir=parent.frame()) {

  nams = ls(all.names=TRUE, envir=envir)
  
  present = sapply(nams, function(n) eval(bquote(!missing(.(n))), envir=envir))
  
  nams = nams[present]
  
  `names<-`(lapply(nams, get, envir=envir), nams)
  
}

#' @rdname call_to_list
#' @export
argvals_to_nlist = function(envir=parent.frame()) {
  argvars = eval(bquote(match.call(expand.dots=FALSE)), envir=envir)
  lapply(as.list(argvars)[-1], as.character)
}
