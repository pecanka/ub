#' @title
#' R session info
#'
#' @description
#'
#' `R_system_call_args()` returns the trailing arguments of the call 
#' that invoked the current R session. Useful when calling Rscript on a 
#' script and modifying its behavior via flags
#'
#' `R_session_info()` prints information about the current R session 
#' including the version of R and the trailing arguments of the system 
#' call that started this session.
#'
#' `R_user_home()` returns the default path to the .Rprofile file in 
#' the user's home directory.
#'
#' `R_default_Rprofile_file()` returns the default path to the 
#' .Rprofile file in the user's home directory.
#'
#' @examples
#' R_system_call_args(trailingOnly=FALSE)
#'
#' @name R_session_info
#' @family system-related functions provided by ub
#' @export
R_system_call_args = function(trailingOnly=TRUE) {
  args = commandArgs(trailingOnly=trailingOnly)
  args = unname(gsub("[\r\n]","",args))
  args[nchar(args)>0]
}

#' @rdname R_session_info
#' @export
R_session_info = function() {

  msgf("R version information:")
  print(R.version)
  msgf("\nProgram call: '",paste(commandArgs(trailingOnly=FALSE), collapse=" "),"'")
  msgf("User home path: '",getwd(),"'")
  msgf("Working path: '",Sys.getenv('R_USER'),"'")
  
}

#' @rdname R_session_info
#' @export
R_user_home = function(fsep=.Platform$file.sep) {
  gsub('\\\\',fsep,Sys.getenv("HOME"))
}

#' @rdname R_session_info
#' @export
R_default_Rprofile_file = function() {
  file.path(R_user_home(), ".Rprofile")
}

