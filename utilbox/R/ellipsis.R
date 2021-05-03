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
#' `args_to_nlist()` returns all arguments supplied or simply defined 
#' to the call of the parent function in a nice `named list` form. In order 
#' for it to work properly, the call to `args_to_nlist()` should be the first 
#' call inside the parent function, 
#' otherwise the call to `ls` might return also objects that did not 
#' exist upon the call to the function.
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

  args = eval(parse(text="match.call(expand.dots=FALSE)$`...`"), envir=envir)
  
  if(keep_symbolic) return(args)
   
  vals = eval(parse(text="list(...)"), envir=envir)
  
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
  
  present = sapply(nams, function(n) eval(parse(text='!missing('%p%n%p%')'), envir=envir))
  
  nams = filter_by_bool(nams, present)
  
  `names<-`(lapply(nams, get, envir=envir), nams)
  
}

#' @rdname call_to_list
#' @export
argvars_to_nlist = function(envir=parent.frame()) {
  eval(parse(text="lapply(as.list(match.call(expand.dots=FALSE))[-1], as.character)"), envir=envir)
}

#args_to_nlist2 = function(envir=parent.frame()) {
#  lapply(call[-1], eval, envir=envir)
#  #call = as.list(kthr(sys.calls(),2))
#  #h1(as.list(args(get(as.character(call[[1]])))),-1)
#}

