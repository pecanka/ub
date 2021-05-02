#' @title
#' Directory of the current script
#'
#' @description
#'
#' `script_dir` returns the directory of the script file from which 
#' it is called (via `source`).
#'
#' @export
script_dir = function() {
  ifelse("ofile" %in% names(sys.frame(1)), dirname(sys.frame(1)$ofile), '.')
}

#' @title
#' Set working directory
#'
#' @description
#'
#' A version of `setwd` which attempts to create the given path when 
#' it does not exist. Unlike `setwd`, which throws an error when no path 
#' is supplied, `setwd2` with missing path argument sets the working 
#' directory to the output of `script_dir`. Additionally, it also takes 
#' the argument `dir2` whose value is appended to the path being set.
#'
#' @export
setwd2 = function(dir, create=TRUE, ask=TRUE, dir2="") {

  no_input = missing(dir) || is.null(dir)
  
  if(no_input) {
    dir = script_dir()
  }
  
  if(nchar(dir2)>0) {
    dir = file.path(dir, dir2)
  }
  
  if(!dir.exists(dir)) {
    
    if(no_input) {
      return(invisible(getwd()))
    }
  
    if(create) {
      
      dir_create(dir, ask=ask)
      
      #if(ask) {
      #  wait("Directory '",dir,"' does not exist and it will be created ...")
      #}
      #dir.create(dir)
      
    } else {
      error("Cannot change working directory to '",dir,"' since it",
            " does not exist and 'create' is FALSE. Use 'create=TRUE'",
            " to enable the creation of non-existing directories.")
    }
    
  }
  
  setwd(dir)
  
  invisible(getwd())
  
}
# setwd2 = function(dir, create=TRUE, ask=TRUE, dir2="") {
  # if(missing(dir) || is.null(dir)) {
    # dir = script_dir()
  # }
  
  # if(nchar(dir2)>0) {
    # dir = file.path(dir, dir2)
  # }
  
  # if(!dir.exists(dir)) {
    # if(create) {
      # if(ask) {
        # wait("Directory '",dir,"' does not exist and it will be created ...")
      # }
      # dir.create(dir)
    # } else {
      # error("Cannot change working directory to '",dir,"' since it",
            # " does not exist. Use 'create=TRUE' for missing directories",
            # " to be created.")
    # }
  # }
  
  # invisible(setwd(dir))
  
# }

#' @title
#' Sourcing check
#'
#' @description
#'
#' `is_sourced()` checks whether the script from which it is called 
#' is being sourced (via either `source()` or `sys.source()`) "at" (when 
#' \code{exact_level=TRUE}) or "above" (when \code{exact_level=FALSE}) 
#' level `level`. In other words, when this function is called from a 
#' script that is being sourced from the global environment then 
#' `is_sourced(1)` returns `TRUE`, when it is called from a script that 
#' is being sourced by another script from the global environment then 
#' `is_sourced(2)` returns `TRUE`, etc. Returns a logical indicator of 
#' whether the sourcing level is equal or above `level`.
#'
#' `source_depth()` determined the depth of sourcing. In other words, 
#' for nested calls to source script this functions determines how many 
#' times the `source` function has currently been called at the point of 
#' invokation of `source_depth()`.
#'
#' `source_files()` sources all specified files (`files`) relative to 
#' the given path (`path`).
#'
#' `source_pattern()` sources all files that match the supplied 
#' pattern (`pattern`) relative to the supplied path (`path`) or the 
#' current working directory.
#'
#' @name sourcing
#' @export
is_sourced = function(level=1, exact_level = TRUE) {
  if(exact_level) source_level() == level else source_level() >= level
}

#' @rdname sourcing
#' @export
source_depth = function() {

  check_for = list("source" = c("ofile", "keep.source", "deparseCtrl", 
                                "echo", "prompt.echo", "spaced"),
                   "sys.source" = c("i", "exprs", "oop", "file", "envir", 
                                    "chdir", "keep.source", "toplevel.env"))
 
  is_source = function(i) all(check_for$"source" %in% names(sys.frame(i)))
  
  is_sys_source = function(i) all(check_for$"sys.source" %in% names(sys.frame(i)))
  
  which_sourced = sapply(0:sys.nframe(), function(i) is_source(i) | is_sys_source(i))
  
  sum(which_sourced)
  
}

#' @rdname sourcing
#' @export
source_files = function(files, path='.', announce=TRUE, normalize=FALSE, 
  envir=.GlobalEnv, report_new=TRUE) {

  ff = file_path(path, files, normalize=normalize)
  msgs = "Sourcing file '" %p% ff %p% "' ... "
  
  new = list()
  for(i in seq_along(ff)) {
  
    if(announce) cat0(msgs[i])
    
    sys.source(ff[i], envir=env <- new.env())
    
    if(report_new) {
      new[[i]] = ls(envir=env, all.names=TRUE)
    }
    
    transfer_objects(env, envir)
    
    if(announce) message(spaces(max(nchar(msgs)) - nchar(msgs[i])),"done.")
    
  }
  
  invisible(if(report_new) `names<-`(new, ff) else ff)
  
}

#' @rdname sourcing
#' @export
source_pattern = function(pattern, path='.', announce=TRUE, normalize=FALSE, 
  envir=.GlobalEnv, report_new=TRUE) {

  files = list.files(path, pattern)
  
  if(is_empty(files))
    error("No files matched the pattern '",pattern,
          "' relative to path '",path,"'.")
  
  files = source_files(files, path, announce=announce, normalize=normalize, 
                       envir=envir, report_new=report_new)
  
  return(invisible(files))
  
}

#' @title
#' Get the list of objects created by sourcing a file
#'
#' @description
#'
#' Find out what objects would be created if a file was sourced (via 
#' [`base::sys.source`]) and return the list.
#'
#' @examples
#' tmp = '.~temp.R'
#' cat('test_function = function() NULL\n', file=tmp_file)
#' list_created_by_sourcing(tmp)
#' unlink(tmp)
#'
#' @export
list_created_by_sourcing = function(file, ..., all.names=TRUE) {

  env = new.env()
  sys.source(file, ..., envir=env)
  objs = ls(all.names=all.names, envir=env)
  
  as_object_table(objs, env)

}
