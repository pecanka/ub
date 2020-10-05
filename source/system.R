#' @title
#' Determine the kind of session and the system.
#'
#' @description
#'
#' `is_win` returns `TRUE` when the current system is Windows.
#'
#' `is_linux` returns `TRUE` when the current system is Linux/Unix.
#'
#' `is_term` returns `TRUE` when the current session runs inside a 
#' terminal (as opposed to inside an interactive GUI).
#'
#' `is_rgui` returns `TRUE` when the current session runs inside an 
#' interactive GUI (as opposed to inside a terminal).
#'
#' `is_rstudio` returns `TRUE` when the current session runs inside 
#' RStudio.
#'
#' @name is_win
#' @family system-related functions provided by utilbox
#' @export
is_win = function() {
  .Platform$OS.type=="windows"
}

#' @rdname is_win
#' @export
is_linux = function() {
  any(.Platform$OS.type==c("linux","unix"))
}

#' @rdname is_win
#' @export
is_term = function() {
  .Platform$GUI=="RTerm"
}

#' @rdname is_win
#' @export
is_rgui = function() {
  .Platform$GUI=="Rgui"
}

#' @rdname is_win
#' @export
is_rstudio = function() {
  .Platform$GUI=="RStudio"
}

#' @title
#' Get and set environment variables
#'
#' @description
#'
#' `set_envir_var` sets a system environment variable, 
#' `get_envir_var` reads a value of a system environment variable.
#'
#' These are simply wrappers for the built-in R functions so that it 
#' is easier to find them.
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

#' @title
#' Modify system search path
#'
#' @description
#'
#' `get_system_path()` reads and returns the system search path variables.
#'
#' `append_system_path()` adds the supplied path into the system 
#' search path variables.
#'
#' @examples
#' get_system_path()
#'
#' @name system_path
#' @family system-related functions provided by utilbox
#' @export
get_system_path = function(path) {
  data.frame(PATH=unlist(strsplit(Sys.getenv("PATH"), ';')))
}

#' @rdname system_path
#' @export
append_system_path = function(path) {

  PATH = Sys.getenv("PATH")
  
  if(missing(path)) return(PATH)
  
  if(grepl(str_patternize(path), PATH)) {
    
    catn("Path '",path,"' already present in the system PATH.")
    
  } else {
    
    Sys.setenv('PATH'=paste0(PATH,";",path))
    
    if(grepl(str_patternize(path), PATH)) {
      catn("Success. The path '",path,"' has been appended to the system PATH.")
    } else {
      catn("Something went wrong. The path '",path,"' does not seem",
           " to have been appended to the system PATH.")
    }
    
  }
  
  invisible(get_PATH())
  
}
