#' Directory of the current script
#'
#' \code{script_dir} returns the directory of the script file from 
#' which it is called (via \code{source}).
#'
#' @export
script_dir = function() {
  if("ofile" %in% names(sys.frame(1))) {
    dirname(sys.frame(1)$ofile)
  } else {
    "."
  }
}

#' Set working directory
#'
#' A version of \code{setwd} which attempts to create the given path when it 
#' does not exist. Unlike \code{setwd}, which throws an error when no path is 
#' supplied, \code{setwd2} with missing path argument sets the working directory 
#' to the output of \code{script_dir}. Additionally, it also takes the argument 
#' \code{dir2} whose value is appended to the path being set.
#'
#' @export
setwd2 = function(dir, create=TRUE, ask=TRUE, dir2="") {
  if(missing(dir) || is.null(dir)) {
    dir = script_dir()
  }
  
  if(nchar(dir2)>0) {
    dir = file.path(dir, dir2)
  }
  
  if(!dir.exists(dir)) {
    if(create) {
      if(ask) {
        wait("Directory '",dir,"' does not exist and it will be created ...")
      }
      dir.create(dir)
    } else {
      error("Cannot change working directory to '",dir,"' since it",
            " does not exist. Use 'create=TRUE' for missing directories",
            " to be created.")
    }
  }
  
  invisible(setwd(dir))
  
}

#' Sourcing check
#'
#' Check whether the script from which it is called is being sourced (via either
#' \code{source()} or \code{sys.source()}) "at" (when \code{exact_level=TRUE}) or 
#' "above" (when \code{exact_level=FALSE}) level \code{level}. In other words, when
#' this function is called from a script that is being sourced from the global environment
#' then \code{is_sourced(1)} returns \code{TRUE}, when it is called from a script that
#' is being sourced by another script from the global environment
#' then \code{is_sourced(2)} returns \code{TRUE}, etc.
#' @return
#' A logical indicator of whether the sourcing level is equal or above \code{level}.
#' @export
is_sourced = function(level=1, exact_level = TRUE) {
  if(exact_level) source_level() == level else source_level() >= level
}

#' Determine the depth of sourcing
#'
#' When sourcing files, it might be of interest to know how many times the \code{source}
#' function has been called. This function returns that number.
#'
#' @name is_sourced
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

#' Source multiple files
#'
#' Sources all specified files (`files`) relative to 
#' the given path (`path`).
#'
#' @export
source_files = function(files, path='.', announce=TRUE) {

  ff = file_path(path, files, normalize=TRUE)
  msgs = "Sourcing file '" %.% ff %.% "' ... "
  
  for(i in seq_along(ff)) {
    if(announce) cat0(msgs[i])
    source(ff[i])
    if(announce) catn(spaces(max(nchar(msgs)) - nchar(msgs[i])),"done.")
  }
  
}

#' Source all files matching a pattern
#'
#' Sources all files that match the supplied pattern (`pattern`)
#' relative to the supplied path (`path`) or the current working
#' directory.
#'
#' @export
source_pattern = function(pattern, path='.') {

  files = list.files(path, pattern)
  
  if(is_empty(files))
    error("No files matched the pattern '",pattern,
          "' relative to path '",path,"'.")
  
  source_files(files, path)
  
}
