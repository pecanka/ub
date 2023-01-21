#' @title
#' Utilbox internals
#'
#' @description
#'
#' `get_in_ub_env` retrieves a value of a variabled named in `x` from 
#' the ub's package namespace or the hidden environment created in 
#' `.GlobalEnv` by a call to `ub_environment()`.
#'
#' `assign_in_ub_env` is an analogue which assigns a supplied value 
#' (`val`) to a variable (`x`).
#'
#' `update_in_ub_env` updates the existing variable by applying a 
#' function (`operation`) with the variable `x` and `val` supplied to it 
#' as the first two arguments.
#'
#' `init_in_ub_env` initializes a given variable to its default value 
#' (defined in and obtained via `get_ub_var_inits`). If not variable name 
#' is supplied, all variables defined in `get_ub_var_inits` are initialized.
#'
#' `ub_namespace()` returns the environment which is the 
#' namespace of the ub package. If it failed for some reason, it 
#' returns creates and/or returns the hidden environment located in 
#' `.GlobalEnv`.
#'
#' `ub_environment()` creates the working environment called 
#' `.ub`, which is used by some of the functions of the `ub` 
#' package (e.g. `cat0`) to store private variables.
#'
#' @name ub_internals
#' @family ub-internal functions provided by ub
#' @export
ub_namespace = function() {
  get_package_namespace("ub") %ERRCLS% ub_environment()
}

#' @export
#' @rdname ub_internals
ub_environment = function(envir=.GlobalEnv) {

  if(!exists(".ub", mode="environment", envir=envir))
    assign(".ub", new.env(), envir=envir)

  get(".ub", envir=envir)

}

#' @export
#' @rdname ub_internals
get_in_ub_env = function(nam, envir=ub_namespace()) {
  if(exists(nam, envir=envir)) {
    get(nam, envir=envir)
  } else {
    structure(NA, class='ub:variable_not_found')
  }
}

#' @export
#' @rdname ub_internals
assign_in_ub_env = function(nam, val, envir=ub_namespace()) {
  assign(nam, val, envir=envir)
}

#' @export
#' @rdname ub_internals
update_in_ub_env = function(nam, val, operation=`+`, envir=ub_namespace()) {
  oldvalue = get(nam, envir=envir)
  newvalue = do.call(operation, list(oldvalue, val))
  assign(nam, newvalue, envir=envir)
}

#' @export
#' @rdname ub_internals
init_in_ub_env = function(nams, envir=ub_namespace()) {
  if(missing(nams)) nams = names(get_ub_var_inits())
  for(nam in nams) {
    #init1_in_ub_env(nam, envir=envir)
    if(!exists(nam, envir=envir)) {
      assign_in_ub_env(nam, get_ub_var_inits(x1))
    }
  }
}

## @export
## @rdname ub_internals
#init1_in_ub_env = function(nam, envir=ub_namespace()) {
#  if(!exists(nam, envir=envir)) {
#    assign_in_ub_env(nam, get_ub_var_inits(x))
#  }
#}

#' @rdname ub_internals
#' @export
get_ub_var_inits = function(nam) {
  
  inits = list(
    trunc_n_hidden = 0,
    trunc_n_limit = 20,
    abbrev_len_limit = 30
  )
  
  if(missing(nam)) {
    inits
  } else {
    inits[[nam]] %|||% stop("Name '",nam,"' not found in 'inits'.")
  }
  
}

#' @title
#' Source ub code files
#'
#' @description
#'
#' Sources the specified files relative to the path in `path`. If 
#' `files` is not specified, it sources all files in the path.
#'
#' By default, this sources files from the ub location on my 
#' system (the author).
#'
#' @family ub-internal functions provided by ub
#' @export
source_ub = function(..., path, normalize=FALSE, envir=.GlobalEnv,
  report_new=TRUE) {
  
  msgf("Sourcing ub source files ...")

  files = dots_to_nlist()

  if(missing(path)) {
    path = get0('.ub_source_path', .GlobalEnv, ifnotfound='.')
  }

  # list all files in the given path when no actual
  # file names were given (i.e. when `...` is empty)
  if(is_empty(files)) {

    msgf("No file list given to `source_ub`, all files will be sourced.")
    msgf("Listing ub files to source ...")
    files = list.files(path, pattern='[.]R$')

  } else {

    files = file_match(files, path)
    files = unlist(lapply(files, first_nonempty))

    if(is_empty(files)) return(catn("No files matched the input"))

  }

  # source the files and return the list of new objects
  new = source_files(files, path, normalize=normalize, envir=envir,
                     report_new=report_new)

  invisible(new)

}

# Create an alias
.sub  = source_ub

