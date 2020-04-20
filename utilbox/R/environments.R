#' @title
#' Get the original environment of a function or another object
#'
#' @description
#' When an object defined in a namespace is printed, its namespace is 
#' shown as the last line. If a function in a package has been defined 
#' as an alias of a function from another package, this original 
#' information is still present when the function is printed For 
#' instance, `utilbox::head` is just an alias for `utils::head`, as 
#' shown when `print(utilbox::head)` is called. The function 
#' `orig_env()` captures that information and returns it as a character 
#' string. For objects with no such information printed it simply 
#' returns the last element of a print call of that object.
#'
#' @examples
#' orig_env(mean)
#'
#' @export
orig_env = function(fun) {
  t1(print2var(fun))
}

#' @title
#' Check for namespace
#'
#' @description
#' Checks if the given string corresponds to a namespace by 
#' attempting to call [`base::asNamespace`] on it and checking whether 
#' an error occurred.
#'
#' @export
namespace_exists = function(ns) {
  !is_error(try(asNamespace(ns), silent=TRUE))
}

#' @title
#' Transfer objects between environments
#'
#' @description
#' Transfers the specified objects (in `what`) or all objects (when 
#' `what` not supplied) from one environment (`env1`) to another 
#' (`env2'). Optionally, an existence check in the destination 
#' environment is performed (when `check_existence=TRUE`).
#'
#' @examples
#' 
#'
#' @export
transfer_objects = function(from, to, what, check_existence=FALSE, delete=TRUE) {

  if(missing(what)) {
    what = ls(all.names=TRUE, envir=from)
  }

  sapply(what, function(w) transfer_object(from, to, w, check_existence, delete))

}

#' @rdname transfer_objects
#' @export
transfer_object = function(from, to, what, check_existence=FALSE, delete=TRUE) {

  stopifnot(is.environment(from), is.environment(to), length(what)==1)
  
  question = "Object named '" %.% what %.% "' already exists in the" %.%
             " destination environment. Proceed?"

  proceed = !check_existence || !exists(what, envir=to) || ask(question)

  fail1 = !proceed || is_error(try(assign(what, get(what, envir=from), envir=to)))
  
  fail2 = !proceed || !delete || is_error(try(rm(list=what, envir=from)))
  
  data.frame(object=what, transfered=!fail1, deleted=!fail2, 
             row.names=FALSE, stringsAsFactors=FALSE)
  
}

#' @title
#' Get an object
#'
#' @description
#' `get2()` gets an object from the given environment (`envir`). Checks if it exists 
#' first and if the object does not exist it returns the value in 
#' `default_value`. If `envir` is missing, it simply looks for the object
#' names 'what' on the search path.
#'
#' @examples
#' get2('fklasdfjskadfjlsd', envir=.GlobalEnv, NA)
#'
#' @export
get2 = function(what, envir, default_value, mode='any', inherits=TRUE) {
  
  if(missing(envir) && exists(what)) {
    get(what, mode=mode, inherits=inherits)
  } else if(!missing(envir) && exists(what, envir=envir)) {
    get(what, envir=envir, mode=mode, inherits=inherits)
  } else {
    default_value
  }
  
}

#' @title
#' Assign into multiple variables
#'
#' @description
#' Assign a value or multiple values into multiple variables with a 
#' single call. `assign2` takes the name(s) of variables in `what` and 
#' assigns the value in `what` to all of them when `what_as_list=FALSE`. 
#' Otherwise, it takes a list in `what` of the same length as `where` 
#' and assigns into variables named in `where` the corresponding 
#' elements in `what`. The assignment takes place in environment `envir`.
#'
#' @examples
#' assign2(c('x','y'), 1:2)
#' assign2(c('x','y'), list(1:2, 'a'), what_as_list=TRUE)
#'
#' @export
assign2 = function(where, what, envir=.GlobalEnv, what_as_list=FALSE) {
  
  if(!what_as_list && length(what)!=1)
    error("The length of 'what' must be 1 when 'what_as_list=FALSE'.")
    
  if(what_as_list) {
    if(!is.list(what))
      error("'what' must be a list when 'what_as_list=TRUE'.")
    if(length(what)!=length(where))
      error("the lengths of 'where' and 'what' must match when 'what_as_list=TRUE'.")
  }

  for(i in seq_along(where)) {
    assign(where[i], what[[min(length(what),i)]], envir=envir)
  }
  
}

#' @title
#' Comparison of environments
#'
#' @description
#' Compares two environments, which can be supplied either directly 
#' or by name (when `x` and/or `y` are of type `character`). If supplied 
#' by name, the names in `x` and `y` are front-appended by 'package:' 
#' unless `assume_package_x` and `assume_package_y` are FALSE, 
#' respectively.
#'
#' @examples
#' is_same_environment(topenv(ls), 'base')
#' is_same_environment(e1 <- new.env(), e1)
#'
#' assign('xyzXYZ', function() {}, envir=.GlobalEnv)
#' is_same_environment(.GlobalEnv, environment(xyzXYZ))
#'
#' @export
is_same_environment = function(x, y, assume_package_x=TRUE, assume_package_y=TRUE) {

  if(is.character(x) && assume_package_x) x = 'package:' %.% x
  if(is.character(y) && assume_package_y) y = 'package:' %.% y
  
  if(is.character(x)) x = as.environment(x)
  if(is.character(y)) y = as.environment(y)
  
  if(!is.environment(x))
    error("'x' is not an environment.")
  
  if(!is.environment(y))
    error("'y' is not an environment.")
  
  identical(x, y)

}

