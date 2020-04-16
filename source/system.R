#' Determine the kind of session and the system.
#'
#' \code{is_win} returns \code{TRUE} when the current system is Windows.
#'
#' \code{is_linux} returns \code{TRUE} when the current system is Linux/Unix.
#'
#' \code{is_term} returns \code{TRUE} when the current session runs inside a
#' terminal (as opposed to inside an interactive GUI).
#'
#' \code{is_rgui} returns \code{TRUE} when the current session runs inside
#' an interactive GUI (as opposed to inside a terminal).
#'
#' \code{is_rstudio} returns \code{TRUE} when the current session runs inside
#' RStudio.
#'
#' @name is_win
#' @family system-related functions provided by utilbox
#' @export
is_win = function() {
  .Platform$OS.type=="windows"
}

#' @name is_win
#' @family system-related functions provided by utilbox
#' @export
is_linux = function() {
  any(.Platform$OS.type==c("linux","unix"))
}

#' @name is_win
#' @family system-related functions provided by utilbox
#' @export
is_term = function() {
  .Platform$GUI=="RTerm"
}

#' @name is_win
#' @family system-related functions provided by utilbox
#' @export
is_rgui = function() {
  .Platform$GUI=="Rgui"
}

#' @name is_win
#' @family system-related functions provided by utilbox
#' @export
is_rstudio = function() {
  .Platform$GUI=="RStudio"
}

#' Get and set environment variables
#'
#' Just wrappers for the built-in R functions so that it
#' is easier to find them.
#'
#' `set_envir_var` sets a system environment variable,
#' `get_envir_var` reads a value of a system
#' environment variable.
#' 
#' @name environment_variables
#' @family system-related functions provided by utilbox
#' @export
get_envir_var = function(name) {
  Sys.getenv(name)
}

#' @rdname environment_variables
#' @export
set_envir_var = function(name) {
  Sys.setenv(name)
}

#' Get the system search paths
#'
#' Returns the system search paths.
#'
#' @examples
#' get_system_PATH()
#'
#' @family system-related functions provided by utilbox
#' @export
get_system_PATH = function(path) {
  data.frame(PATH=unlist(strsplit(Sys.getenv("PATH"), ';')))
}

#' Add to the system search paths
#'
#' Adds the supplied path among the system search paths.
#'
#' @family system-related functions provided by utilbox
#' @export
append_system_PATH = function(path) {

  PATH = Sys.getenv("PATH")
  
  if(missing(path)) return(PATH)
  
  if(regexpr(str_patternize(path), PATH)>0) {
    
    catn("Path '",path,"' already present in the system PATH.")
    
  } else {
    
    Sys.setenv('PATH'=paste0(PATH,";",path))
    
    if(regexpr(str_patternize(path), PATH)>0) {
      catn("Path '",path,"' appended to the system PATH.")
    }
    
  }
  
  invisible(get_PATH())
  
}
