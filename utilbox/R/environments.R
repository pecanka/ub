#' @title
#' Original and parent environments
#'
#' @description

#' `orig_env()` gets the "original" environment (which is more precisely 
#' referred to as the *enclosing environment*) of a function (or another 
#' object). As described in the help page of `base::parent.env()` (see 
#' `?parent.env`), an object's parent environment is the environment where 
#' it was defined. For a function, this is differentiated from the function's 
#' *parent frame*, which is the environment from which the function was invoked.
#' During object look up, the cascade of parent environments is searched
#' for an object of the matching name. Parent frames are not part of this
#' cascade.
#'
#' When an object defined in a namespace (i.e., inside a package) is printed, 
#' its namespace is shown as the last line. If a function in a package has 
#' been defined as an alias of a function from another package, this original 
#' information is still present when the function is printed. For instance,
#' `utilbox::head` is just an alias for `utils::head`. This can be seen by 
#' calling `print(utilbox::head)`. The function `orig_env()` captures that 
#' information and returns it as a character string. For objects with no such 
#' information printed it returns the value in the argument `na`.
#'
#' `parent_envs()` returns the cascade of all parent environments
#' of `x`. If no input is specified during a call to `parent_envs()`,
#' the default is to take the function's calling environment.
#'
#' `parent_frames()` returns the cascade of parent frames to the frame that
#' invoked it (unless `envir` specifies a different base frame). It returns
#' a list with the frames and the invoking calls, with the names of the list
#' frames derived from those calls.
#'
#' @examples
#' orig_env(mean)
#'
#' parent_envs()
#' parent_envs(mean)
#'
#' @name environments
#' @export
orig_env = function(obj, na='') {
  last_line = tail(print2var(obj),1)
  ifelse(last_line %like% '[<]environment[:].namespace[:]', last_line, na)
}

#' @rdname environments
#' @export
parent_envs = function(x = parent.frame()) {
  
  if(!is.environment(x)) 
    x = environment(x)
  
  pe = try(parent.env(x), silent=TRUE)
  
  if(is_error(pe)) 
    return(NULL)
  
  c(pe, parent_envs(pe))
  
}

#' @rdname environments
#' @export
parent_frames = function(n, stop_at_global=FALSE, envir=parent.frame()) {

  frames = sys.frames()
  calls = sys.status()$sys.calls[-1]
  
  if(!missing(n)) {
    frames = head(frames, n)
    calls = head(calls, n)
  }
  
  w_envir = which(sapply(frames, identical, envir))
  
  if(length(w_envir)==0)
    stop('Supplied environment is not among the frames returned by `sys.frames()`.')
    
  keep = seq(1,tail(w_envir,1),1)
  frames = frames[keep]
  calls = calls[keep]
  
  if(!stop_at_global) {
    w_global = which(sapply(frames, identical, .GlobalEnv))
    if(length(w_global)>0) {
      keep = seq(tail(w_global,1), length(frames), 1)
      frames = frames[keep]
      calls = calls[keep]
    }
  }
  
  envs = unlist(lapply(frames, utils::capture.output))

  nams = lapply(calls, utils::capture.output)
  nams = lapply(nams, gsub, pattern="^\\s+|\\s+$", replacement="")
  nams = lapply(nams, paste, collapse=' ')
  nams = paste0(envs,': ',unlist(nams))
  nams = substr(nams, 1, 250)
  
  names(frames) = nams
  
  list(frames=frames, calls=calls)
  
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
#' # create two environment and populate one
#' env1=new.env()
#' env2=new.env()
#' assign('x', rnorm(10), envir=env1)
#' assign('y', "hello world", envir=env1)
#' ls(envir=env1)
#' 
#' # transfer all objects from env1 to env2 (without deletion, so basically "copy")
#' transfer_objects(env1, env2, delete=FALSE)
#' ls(envir=env1)
#' ls(envir=env2)
#'
#' # transfer all objects from env1 to env2 (with deletion, so proper "transfer")
#' transfer_objects(env1, env2)
#' ls(envir=env1)
#' ls(envir=env2)
#'
#'
#' # transfer one object back from env2 to env1
#' transfer_objects(env1, env2, 'x')
#' ls(envir=env1)
#' ls(envir=env2)
#'
#' @export
transfer_objects = function(from, to, what, check_existence=FALSE, delete=TRUE) {

  if(missing(what)) {
    what = ls(all.names=TRUE, envir=from)
  }

  if(length(what)>1) {
    return(sapply(what, function(w) transfer_objects(from, to, w, check_existence, delete)))
  }

  stopifnot(is.environment(from), is.environment(to), length(what)==1)
  
  question = paste0("Object named '", what, "' already exists in the",
                    " destination environment. Proceed?")

  proceed = !check_existence || !exists(what, envir=to) || ask(question)

  ftransfered = !proceed || is_error(try(assign(what, get(what, envir=from), envir=to)))
  
  fdeleted = !proceed || !delete || is_error(try(rm(list=what, envir=from)))
  
  data.frame(object=what, transfered=!ftransfered, deleted=!fdeleted, 
             row.names=FALSE, stringsAsFactors=FALSE)
  
}

#' @title
#' Get an object
#'
#' @description
#'
#' `get2()` is nothing but a wrapper around `base::get0()`, which extracts 
#' an object from the given environment (`envir`) unless the object does not
#' exist at which point the value in `ifnotfound` is returned. It differs from
#' `base::get0()` by the order of arguments and in that it does not have any 
#' value set for `ifnotfound` by default.
#'
#' `get2m()` extracts multiple objects and returns them in a list.
#'
#' @examples
#' get2('fklasdfjskadfjlsd', envir = .GlobalEnv, ifnotfound = NA)
#'
#' @export
get2 = function(what, envir = parent.frame(), ifnotfound, mode = 'any', inherits = TRUE) {
  base::get0(what, mode = mode, envir = envir, inherits = inherits, ifnotfound = ifnotfound)
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
    stop("The length of `what` must be 1 when `what_as_list=FALSE`.")
    
  if(what_as_list) {
  
    if(!is.list(what))
      stop("The value in `what` must be a list when `what_as_list=TRUE`.")
      
    if(length(what) != length(where))
      stop("The lengths of `where` and `what` must match when `what_as_list=TRUE`.")
      
  } else {
    what = list(what)
  }
  
  if(in_namespace && missing(ns))
    stop("The name of the namespace must be supplied via `ns` when `in_namespace=TRUE`.")

  for(i in seq_along(where)) {
    args = list(x=where[i], value=what[[min(length(what),i)]])
    if(in_namespace) {
      fun = 'assignInNamespace'
      do.call(fun, append(args, list(ns=ns)))
      #()
    } else {
      do.call(assign, append(args, list(envir=envir)))
    }
  }
  
}

#' Assign for locked environments
#'
#' `assign_locked()` assigns the supplied value in `value` to an 
#' object named in `x` inside the namespace named in `namespace` 
#' (supplied as character). If the object named in `x` is locked, 
#' the binding is first unlocked and relocked afterwards. In locked 
#' environments only existing objects can be modified by `assign_locked()`.
#'
#' @export
assign_locked = function(x, value, envir) {

  if(!is.character(x))
    stop("Supply name of an object as character.")
  
  if(is.character(envir)) {
    envir = asNamespace(envir)
  }
  
  if(!exists(x, envir=envir))
    stop("Object '",x,"' not found in the specified environment",
         " and therefore cannot be overwritten.")
 
  is_locked = base::environmentIsLocked(envir)
  
  if(is_locked) {
    unlockBinding(sym = x, envir)
  }
  
  assign(x = x, value = value, envir = envir)
  
  if(is_locked) {
    lockBinding(sym = x, envir)
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

  if(is.character(x) && assume_package_x) 
    x = paste0('package:', x)
  if(is.character(y) && assume_package_y) 
    y = paste0('package:', y)
  
  if(is.character(x)) 
    x = as.environment(x)
  if(is.character(y)) 
    y = as.environment(y)
  
  if(!is.environment(x))
    stop("'x' is not an environment.")
  
  if(!is.environment(y))
    stop("'y' is not an environment.")
  
  identical(x, y)

}
