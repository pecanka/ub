#' @title
#' Get function name
#'
#' @description
#' Returns the name of the parent function, i.e. that from whose body 
#' it is invoked. If invoked from `.GlobalEnv`, returns an empty string 
#' (i.e. "").
#'
#' @examples
#' example_function = function() print(this_fun_name())
#' example_function()     # should return 'example_function'
#'
#' @family coding-related functions provided by utilbox
#' @export
this_fun_name = function(e=sys.parent()) {
  if(e==0) "" else as.character(h1(sys.call(which=e))) 
}

#' Argument hijack function
#'
#' @description
#' A function that allows for changing of default values for 
#' arguments of other functions. Similar to [`base::formals`] but nicer 
#' syntax.
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

#' @title
#' Number of arguments a function accepts
#'
#' @description
#' Returns the number of arguments that a given function `fun` 
#' accepts. If the arguments of `fun` contain an ellipse (`...`), it 
#' returns `Inf`.
#'
#' @examples
#' nformals(mode)
#' nformals(mean)
#'
#' 
nformals = function(fun, envir=parent.frame()) {

  if(is.character(fun)) {
    fun = get(fun)
  }
  
  fs = formals(fun)
  ifelse('...' %in% names(fs), Inf, length(fs))
  
}

#' @title
#' Get the source code of a function
#'
#' @description
#' `fun_code_to_text()` returns the body of the supplied function as 
#' a character string.
#'
#' @param fun function for which the body of the code is desired
#'
#' @details This function takes an R function on input and returns 
#' its source code as character vector.
#'
#' @returns A character vector with each command of the input 
#' function as an element.
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

#' @title
#' Append the body of a function
#'
#' @description
#' Takes an existing function `fun` and changes its body by appending 
#' to it the calls supplied in `calls`. The calls can either be of class 
#' `calls` or class `character` (converted to `call` via 
#' `as.character`). The way the new cody is appended is governed by 
#' `where`, which allows these options:
#'
#' \"first\": the code in `calls` is placed before the existing code.
#'
#' \"last\": the code in `calls` is placed after the existing code.
#'
#' \"at\": `calls` must be of length 1 and be of class character and 
#' its content is parsed and evaluated. It can reference the selected 
#' old line (which one that is is determined by the value in `at`) as 
#' `.oldline.` (see the example below).
#'
#' @examples
#' f = function() { x = pi }
#' print(f())                           # returns pi (i.e. 3.141593)
#' f = append_body(f, quote(x^2))
#' print(f())                           # returns pi^2 (i.e. 9.869604)
#' f = append_body(f, c(quote(x <- x^2), quote(x-1)))
#' print(f())                           # returns pi^2 - 1 (i.e. 8.869604)
#'
#' # Modify an existing function:
#' call = str2lang("cat(\"And the symmetric set difference is...\n\")")
#' f = append_body(setdiffsym, call, where='first')
#' f(1:5, 3:10)
#'
#' # Above the line is placed at the top so that it does not affect
#' # the return value from the function. A more complex editing is
#' # is possible. We can append the last call in the function
#' # to turn it into an assignment and thus assign the calculated
#' # value into a variable. We can then save add the printing line 
#' # without affecting the returned value of the function. The line 
#' # in the original code that is altered is determined thru 'at'.
#' call = "substitute(y -> x, list(y=.oldline.))"
#' f = append_body(setdiffsym, call, where='at', at=1)
#' f = append_body(f, c("cat(\"And the symmetric set difference is...\n\")", "x"), where='last')
#' f(1:5, 3:10)
#'
#' @family coding-related functions provided by utilbox
#' @export
append_body = function(fun, calls, where=c('first','last','at'), at=NULL) {
  
  where = match.arg(where)
  
  oldbody = as.list(body(fun))
  
  # drop the brackets if there are any
  if(identical(oldbody[[1]], as.name('{'))) {
    oldbody = oldbody[-1]
  }
  
  # append the call at the top ('first') or at the bottom ('last')
  if(where %in% c('first', 'last')) {
    
    if(is.character(calls)) {
      calls = lapply(calls, str2lang)
    }
    
    newbody = if(where=='first') {
      append(calls, oldbody)
    } else {
      append(oldbody, calls)
    }
  
  # alter the original code line number 'at' with the parsed and evaluated
  # content in calls
  } else {
  
    if(length(calls)!=1) 
      error("With where='at' exactly one code line must be supplied.")
    if(missing(at))
      error("With where='at' a value for 'at' must be supplied.")
    
    at = min(at, length(oldbody))
    .oldline. = oldbody[[at]]
    oldbody[[at]] = eval(parse(text=calls[[1]]))
    newbody = oldbody
    
  }
  
  # assign the modified code back into fun and return the function
  body(fun) = as.call(c(as.name("{"), newbody))
  
  fun
  
}
