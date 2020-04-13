#' Get the utilbox namespace/environment
#'
#' Creates the working namespace called `.utilbox`, which is used by some of the
#' functions of the `utilbox` package (e.g. \code{cat0}) to store private variables.
#'
#' This was used before `utilbox` was an installable package (now basically unnecessary).
#'
#' @family utilbox-internal functions provided by utilbox
#' @export
utilbox_namespace = function(envir=.GlobalEnv) {
  if(!exists(".utilbox", mode="environment", envir=envir))
    assign(".utilbox", new.env(), envir=envir)
  get(".utilbox", envir=envir)
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
    files = list.files(path, pattern='[.]R$')
    catn("No input, sourcing all files.")
  } else {
    
    files = file_match(files, path)
    files = unlist(lapply(files, first_nonempty))
    
    if(is_empty(files)) return(catn("No files matched the input"))
    
  }
  
  for(f in files) {
    ff = file_path(path, f, normalize=TRUE)
    cat0("Sourcing file '",ff,"' ... ")
    source(ff)
    catn("done.")
  }
  
}

#' Benchmarking
#'
#' An alias for `microbenchmark::microbenchmark`.
#'
#' @export
mb = function(...) {
  do.call(microbenchmark::microbenchmark, list(...))
}
