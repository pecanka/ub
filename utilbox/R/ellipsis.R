#' @title
#' Processing of ellipsis and call arguments
#'
#' @description
#' `dots_to_nlist()` returns the ellipsis of a parent function in a 
#' nice form. In calls with unnamed arguments it extracts the names of 
#' variables supplied in the call and uses them to assign names to the 
#' list produced from the ellipsis (`...`).
#'
#' `args_to_nlist()` returns all arguments supplied or simply defined 
#' to the call of the parent function in a nice `named list` form. With 
#' `only_called=TRUE` the function can be called at any point inside the 
#' function for which it is getting the arguments. With 
#' `only_called=FALSE`, in order to work properly, the call to 
#' `args_to_nlist` should be the first call inside the parent function, 
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
dots_to_nlist = function(flatten=FALSE, assign_names=TRUE, names, envir=parent.frame()) {

  args = eval(parse(text="match.call(expand.dots=FALSE)$`...`"), envir=envir)
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
  
  present = sapply(nams, function(n) eval(parse(text='!missing('%.%n%.%')'), envir=envir))
  
  nams = filter_by_bool(nams, present)
  
  `names<-`(lapply(nams, get, envir=envir), nams)
  
}

#args_to_nlist2 = function(envir=parent.frame()) {
#  lapply(call[-1], eval, envir=envir)
#  #call = as.list(nthr(sys.calls(),2))
#  #h1(as.list(args(get(as.character(call[[1]])))),-1)
#}

