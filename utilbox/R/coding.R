#' Get function name
#'
#' Returns the name of the parent function, i.e. that from whose body it is invoked.
#'
#' @examples
#' lll = function() print(this_fun_name()); lll() # should return 'lll'
#'
#' @family coding-related functions provided by utilbox
#' @export
this_fun_name = function() {
  as.character(h1(sys.call(which=sys.parent()))) 
}

#' Get the source code of a function
#'
#' `fun_code_to_text` returns the body of the supplied function
#' as a character string.
#'
#' @param fun function for which the body of the code is desired
#'
#' @details
#' This function takes an R function on input and returns its source code as character vector.
#'
#' @return A character vector with each command of the input function as an element.
#'
#' @examples
#' f <- function(x) { 
#'   print(x)
#'   x = 2
#'   invisible(x)
#' }
#' ff <- fun_code_to_text(f)
#' print(ff)
#'
#' @family coding-related functions provided by utilbox
#' @export
fun_code_to_text = function(fun, file=NULL) {

  stopifnot(is.function(fun))
  
  b = c(deparse(args(fun))[deparse(args(fun))!="NULL"], deparse(body(fun)))
  
  if(!missing(file) && !is.null(file)) {
    cat(paste(b, collapse="\n"), file = file)
    invisible(b)
  } else {  
    b
  }
  
}

fun_code_to_text = function(fun, file=NULL) {
  capture.output(print(fun))
}
  
#' Append the body of a function
#'
#' Takes an existing function (`fun`) and changes its body
#' by appending to it the calls supplied in `calls`.
#'
#' @examples
#' f = function() { x = pi }
#' print(f())                           # returns pi (i.e. 3.141593)
#' f = append_body(f, quote(x^2))
#' print(f())                           # returns pi^2 (i.e. 9.869604)
#' f = append_body(f, c(quote(x <- x^2), quote(x-1)))
#' print(f())                           # returns pi^2 - 1 (i.e. 8.869604)
#'
#' # modify an existing function
#' call = str2lang("cat(\"And the symmetric set difference is...\n\")")
#' f = append_body(setdiffsym, call, at_top=TRUE)
#' f(1:5, 3:10)
#'
#' @family coding-related functions provided by utilbox
#' @export
append_body = function(fun, calls, at_top=FALSE) {
  oldbody = as.expression(as.list(body(fun))[-1])
  newbody = if(at_top) c(calls, oldbody) else c(oldbody, calls)
  body(fun) = as.call(c(as.name("{"), newbody))
  fun
}

#' Processing of ellipsis and call arguments
#'
#' `dots_to_nlist` returns the ellipsis of a parent function in a 
#' nice form. In calls with unnamed arguments it extracts the names
#' of variables supplied in the call and uses them to assign names 
#' to the list produced from the dots (`...`).
#'
#' `args_to_nlist` returns all arguments supplied or simply defined
#' to the call of the parent function in a nice `named list` form. 
#' With `only_called=TRUE` the function can be called at any point
#' inside the function for which it is getting the arguments. 
#' With `only_called=FALSE`, in order to work properly,
#' the call to `args_to_nlist` should be the first call inside 
#' the parent function, otherwise the call to `ls` might return also
#' objects that did not exist upon the call to the function. 
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

#' Argument hijack function
#'
#' A function that allows for changing of default values for arguments of other 
#' functions
#'
#' @examples
#' cat = hijack(cat, sep='')        # equivalent to cat0
#' mean = hijack(mean, na.rm=TRUE)  # equivalent to mean2
#'
#' @family coding-related functions provided by utilbox
#' @export
hijack = function(FUN, ...) {
  .FUN = FUN
  args = list(...)
  lapply(seq_along(args), function(i) formals(.FUN)[[names(args)[i]]] <<- args[[i]])
  .FUN
}

#put_trace = function(..., tracer, at) 
  
#' Compare functions
#'
#' Compares two functions by looking at their arguments (via \code{base::args})
#' and their bodies (via \code{base::body}).
#'
#' @examples
#' f1 = function (x) .Internal(which.max(x))
#' identical(f1, base::which.max)                              # returns FALSE, since they are defined in different environments
#' compare_functions(f1, base::which.max)                      # returns TRUE
#' compare_functions('f1', 'base::which.max', by_name=TRUE)    # returns TRUE
#'
#' @family coding-related functions provided by utilbox
#' @export
compare_functions = function(fun1, fun2, by_name=FALSE, envir1, envir2) {
  
  if(by_name) {
    if(is.character(fun1)) {
      fun1 = if(missing(envir1)) {
        get(fun1, mode='function') 
      } else {
        get(fun1, envir=envir1, mode='function')
      }
    }
    if(is.character(fun2)) {
      fun2 = if(missing(envir2)) {
        get(fun2, mode='function') 
      } else {
        get(fun2, envir=envir2, mode='function')
      }
    }
  }
  
  stopifnot(is.function(fun1), is.function(fun2))
  
  identical(args(fun1), args(fun2)) && identical(body(fun1), body(fun2))
  
}  
  
###
# Example of the usage of the trace function
#
# Let's insert a call to the browser function at line 4 of the function force_as_real"
#
# trace(force_as_real, browser, at=4)
# force_as_real('a1.4')
# untrace(force_as_real, browser)
#
# Ways to change the arguments of a function
#
# formals(cat)$sep <- ""
# trace(base::cat, tracer=quote(if(missing(sep)) sep=''), at=1)
# cat = purrr::partial(cat, sep="")
#
# assign("cat", cat0, envir=.GlobalEnv)
# assign("cat", cat0, envir=.utilbox)

# cat = utils::getFromNamespace("cat", ns="base")
# trace(cat, tracer=quote(if(missing(sep)) sep <- ""), at=1, print=FALSE)
# #utils::assignInNamespace("cat", cat0, ns="base")
###

