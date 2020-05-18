#' @title
#' Original and parent environments
#'
#' @description

#' `orig_env()` get the original environment of a function or another object.
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
#' `all_parent_envs()` returns the cascade of all parent environments
#' of `x`. If no input is specified during a call to `all_parent_envs()`,
#' the default is to take the calling environment.
#'
#' @examples
#' orig_env(mean)
#'
#' all_parent_envs()
#' all_parent_envs(mean)
#'
#' @name environments
#' @export
orig_env = function(obj, na='') {
  last_line = t1(print2var(obj))
  ifelse('<environment:.namespace:' %m% last_line, last_line, na)
}

#' @rdname environments
#' @export
all_parent_envs = function(x=parent.frame()) {
  
  if(!is.environment(x)) x = environment(x)
  
  pe = try(parent.env(x), silent=TRUE)
  #if(is_error(pe)) browser()
  
  if(is_error(pe)) return(NULL)
  
  c(pe, all_parent_env(pe))
  
}

#' @title
#' Check for namespace
#'
#' @description
#'
#' Checks whether a namespace exists. For packages, this basically
#' checks whether a package is installed. The check is done by 
#' calling `base::asNamespace(ns)` and returning a logical which 
#' indicates whether an error occurred on that call.
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
#'
#' `get2()` extracts an object from the given environment (`envir`). 
#' Checks if it exists first and if the object does not exist it returns 
#' the value in `ifnotfound`. If `envir` is missing, it simply looks 
#' for the object names 'what' on the search path. Similar to `base::get0()`
#' except that it does not have any value set for `ifnotfound` by default.
#'
#' `get2m()` extracts multiple objects and returns them in a list.
#'
#' @examples
#' get2('fklasdfjskadfjlsd', envir=.GlobalEnv, NA)
#'
#' @export
get2 = function(what, envir=parent.frame(), ifnotfound, mode='any', inherits=TRUE) {
  get0(what, mode=mode, envir=envir, inherits=inherits) %|||% ifnotfound
}

#' @rdname get2
#' @export
mget2 = function(what, envir=parent.frame(), ...) {
  nlapply(what, get2, envir=envir, ...)
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
assign2 = function(where, what, envir=.GlobalEnv, ns, what_as_list=FALSE, in_namespace=FALSE) {
  
  if(!what_as_list && length(what)!=1)
    error("The length of `what` must be 1 when `what_as_list=FALSE`.")
    
  if(what_as_list) {
    if(!is.list(what))
      error("The value in `what` must be a list when `what_as_list=TRUE`.")
    if(length(what)!=length(where))
      error("The lengths of `where` and `what` must match when `what_as_list=TRUE`.")
  } else {
    what = list(what)
  }
  
  if(in_namespace && missing(ns))
    error("The name of the namespace must be supplied via `ns` when `in_namespace=TRUE`.")

  for(i in seq_along(where)) {
    args = list(x=where[i], value=what[[min(length(what),i)]])
    if(in_namespace) {
      #fun = 'fixInNamespace'
      fun = 'assignInNamespace'
      do.call(fun, args %append% list(ns=ns))
      #()
    } else {
      do.call(assign, args %append% list(envir=envir))
    }
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
