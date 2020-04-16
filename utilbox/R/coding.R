#' Get function name
#'
#' Returns the name of the parent function, i.e. that from whose body it is invoked.
#' If invoked from `.GlobalEnv`, returns an empty string ("").
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

#' @export
fun_code_to_text2 = function(fun, file=NULL) {
  capture.output(print(fun))
}
  
#' Append the body of a function
#'
#' Takes an existing function (`fun`) and changes its body
#' by appending to it the calls supplied in `calls`. The calls
#' can either be of class `calls` or class `character` (converted
#' to `call` via `as.character`). The way the new cody is
#' appended is governed by `where`, which allows these options:
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

#' Checks for an error
#'
#' Checks whether an object is a result of a failer (error throwing)
#' call by checking whether it is of class `try-error` (which is 
#' produced by [`base::try()`]).
#'
#' @examples
#' is_error(try(x = .this.is.a.missing.variable))
#'
#' @export
is_error = function(x) {
  'try-error' %in% class(x)
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

#' Compare code in R scripts
#'
#' Takes two R scripts and performs a comparison. First, the
#' actual codes are compared as character strings. If they fail
#' this sameness check, the two scripts are sourced into separate
#' environments, which are then compared. In this comparison, the
#' names of all objects are compared (check 1), then the classes 
#' of all objects (check 2), then functions are checked for identical 
#' arguments and bodies (check 3), and finally non-function objects 
#' are compared in terms of values (check 4).
#' 
#' @returns A logical indicating whether the two codes were found to
#'          to be identical together with the number of the first failed
#'          check (attribute \code{check_failed}, where value 0 indicates
#'          'no failure') and the names of the objects that were found 
#'          to be different (attribute \code{objects}).. 
#'
#' @export
compare_code = function(file1, file2, verbose=TRUE) {

  if(verbose) {
    cat0("Comparing files '",file1,"' and '",file2,"' ... ")
    on.exit(catn(if(are_same) 'PASS' else 'DIFFERENCES FOUND!'))
  }
  
  # read the file sources
  C1 = readLines(file1)
  C2 = readLines(file2)
  
  # remove all commented lines
  C1 = C1[!('^\\s*#' %m% C1)]
  C2 = C2[!('^\\s*#' %m% C2)]
  
  # check for identical code
  if(identical(C1, C2)) {
    return(are_same <- structure(TRUE, check_failed = 0))
  }
  
  # if not, execute the code and compare the environments
  writeLines(C1, f1 <- '.~C1.R.tmp')
  writeLines(C2, f2 <- '.~C2.R.tmp')
  sys.source(f1, envir=e1 <- new.env())
  sys.source(f2, envir=e2 <- new.env())
  file.remove(f1, f2)

  # if the object names do not match, return severe difference
  list1 = ls(envir=e1, all=TRUE, sorted=TRUE)
  list2 = ls(envir=e2, all=TRUE, sorted=TRUE)
  if(!identical(list1, list2)) {
    objects = setdiffsym(list1, list2, labels=c(file1, file2))
    return(are_same <- structure(FALSE, check_failed=1, objects=objects))
  }
  
  # compare classes of objects
  class1 = sapply(list1, function(obj) class(get(obj, envir=e1)))
  class2 = sapply(list2, function(obj) class(get(obj, envir=e2)))
  if(!identical(class1, class2)) {
    objects = list(list1[class1!=class2], list2[class1!=class2])
    names(objects) = c(file1, file2)
    return(structure(FALSE, check_failed=2, objects=objects))
  }
  
  # compare all functions by code
  is_fun = sapply(class1, function(cls) identical(cls, 'function'))
  funs_equal = sapply(list1[is_fun], function(obj)
    compare_functions(obj, obj, by_name=TRUE, envir1=e1, envir2=e2))
    
  # compare all non-functions by value
  rest_equal = sapply(list1[!is_fun], function(obj) 
    identical(get(obj, envir=e1), get(obj, envir=e2)))
  
  # final verdict
  if(all(funs_equal) && all(rest_equal)) {
    are_same <- structure(TRUE, check_failed=0)
  } else {
    objects1 = c(list1[is_fun][!funs_equal], list1[!is_fun][!rest_equal]) 
    objects2 = c(list2[is_fun][!funs_equal], list2[!is_fun][!rest_equal])
    objects = structure(list(objects1, objects2), names=c(file1, file2))
    are_same <- structure(FALSE, check_failed=3, objects=objects)
  }
  
  are_same
    
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

# Useful functions:
#
# utils::getAnywhere() retrieves an R Object, including from a Namespace, whether visible 
#   on the search path, registered as an S3 method or in a namespace but not exported.
# utils::argsAnywhere returns the arguments of objects that are functions.
#
# envnames::environment_name() returns the name of the variable that stores the
#   environment (input example: “< environment: 0x00000000147499b0>”)
# envnames::obj_find() looks up objects in all packages and environments including
#   the call's function cascade of frames
#
# base::typeof() returns the type or storage mode of any object (e.g. 'numeric', 'character')
#
# sloop::ftype() returns the type of a function (e.g. "primitive" or c("S3","generic"))
# sloop::s3_dispatch() helps with inspecting how an S3 method is dispatched.
# sloop::s3_get_method() shows the code of the method that gets called regardless
#   whether it is exported by a package or not. Basically, emulated the `:::` call
#   but simpler, since the method name does not have to be looked up first
# sloop::s3_methods_generic() let you find all methods defined for a generic
# sloop::s3_methods_class() does the same for a class
#
# base::NextMethod() delegated work in a method dispatch to the next method in line
#   for an S3 class (see https://adv-r.hadley.nz/s3.html)
#
# The term "S3 method" basically just refers to a scheme of method dispatching,
#   not a specific object type as the name might suggest.
#
# S4 is a stricter version (compared to S3) of the object oriented programming 
# structure in R with many of underlying ideas the same. Two main differences:
#   1. formal class definitions: unlike S3, S4 formally defines the representation 
#      and inheritance for each class
#   2. multiple dispatch: the generic function can be dispatched to a method 
#      based on the class of any number of argument, not just one
# (see http://adv-r.had.co.nz/S4.html)
#
# Pointers in R can be implemented using environments, since environments do not
#   get coppied when used as arguments of a function
#
###

