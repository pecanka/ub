#' Get the namespace of the utilbox package
#'
#' Returns the environment which is the namespace of the utilbox package.
#' If it failed for some reason, it returns creates and/or returns 
#' the hidden environment located in `.GlobalEnv`.
#'
#' @family utilbox-internal functions provided by utilbox
#' @export
utilbox_namespace = function() {
  get_package_namespace("utilbox") %ERR% utilbox_environment()
}
  
#' Get the utilbox environment
#'
#' Creates the working namespace called `.utilbox`, which is used by some of the
#' functions of the `utilbox` package (e.g. \code{cat0}) to store private variables.
#'
#' This was used before `utilbox` was an installable package (now basically unnecessary).
#'
#' @family utilbox-internal functions provided by utilbox
#' @export
utilbox_environment = function(envir=.GlobalEnv) {

  if(!exists(".utilbox", mode="environment", envir=envir))
    assign(".utilbox", new.env(), envir=envir)
    
  get(".utilbox", envir=envir)
  
}

#' Get/assign/update/initialize a variable from utilbox environment
#'
#' `get_utilbox` retrieves a value of a variabled named in `x` 
#' from the utilbox's package namespace or the hidden 
#' environment created in `.GlobalEnv` by a call to
#' `utilbox_environment()`.
#'
#' `assign_utilbox` is an analogue which assigns a supplied 
#' value (`val`) to a variable (`x`).
#'
#' `update_utilbox` updates the existing variable by 
#' applying a function (`operation`) with the variable
#' `x` and `val` supplied to it as the first two arguments.
#'
#' `init_utilbox` initializes a given variable to its default
#' value (defined in and obtained via `utilbox_inits`).
#' If not variable name is supplied, all variables defined
#' in `utilbox_inits` are initialized.
#'
#' @name utilbox_namespace_modify
get_utilbox = function(x, envir=utilbox_namespace()) {
  if(exists(x, envir=envir)) {
    get(x, envir=envir) 
  } else {
    structure(NA, class='utilbox:variable_not_found')
  }
}

#' @rdname utilbox_namespace_modify
assign_utilbox = function(x, val, envir=utilbox_namespace()) {
  assign(x, val, envir=envir)
}

#' @rdname utilbox_namespace_modify
update_utilbox = function(x, val, operation=`+`, envir=utilbox_namespace()) {
  oldvalue = get(x, envir=envir)
  newvalue = do.call(operation, list(oldvalue, val))
  assign(x, newvalue, envir=envir)
}

#' @rdname utilbox_namespace_modify
init_utilbox = function(x, envir=utilbox_namespace()) {
  if(missing(x)) x = names(utilbox_inits())
  for(x1 in x) init1_utilbox(x1)
}

#' @rdname utilbox_namespace_modify
init1_utilbox = function(x, envir=utilbox_namespace()) {
  if(!exists(x, envir=envir)) {
    assign_utilbox(x, utilbox_inits(x))
  }
}

#' @rdname utilbox_namespace_modify
utilbox_inits = function(x) {
  inits = list(
    trunc_n_hidden = 0,
    trunc_n_limit = 20,
    abbrev_len_limit = 30
  )
  if(missing(x)) {
    inits 
  } else {
    inits[[x]] %|||% stop(x, " not found in 'inits'.")
  }
}  

#' Source utilbox code files
#'
#' Sources the specified files relative to the path in `path`.
#' If `files` is not specified, it sources all files in the
#' path.
#'
#' By default, this sources files from the utilbox location
#' on my system (the author).
#'
#' @family utilbox-internal functions provided by utilbox
#' @export
source_utilbox = function(..., path) {
  
  files = dots_to_nlist()
  
  if(missing(path)) {
    if(exists('.utilbox_source_path', envir=.GlobalEnv)) {
      path = get('.utilbox_source_path', envir=.GlobalEnv)
    }
  }
  
  # source all if no specific files given via `...`
  if(is_empty(files)) {
  
    catn("Listing files ...")
    files = list.files(path, pattern='[.]R$')
    catn("No input, sourcing all files.")
    
  } else {
    
    files = file_match(files, path)
    files = unlist(lapply(files, first_nonempty))
    
    if(is_empty(files)) return(catn("No files matched the input"))
    
  }
  
  source_files(files, path)
  
}

#' Benchmarking
#'
#' An alias for `microbenchmark::microbenchmark`.
#'
#' @export
mb = function(...) {
  do.call(microbenchmark::microbenchmark, list(...))
}
