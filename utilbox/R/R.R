#' Get trailing arguments
#'
#' Returns the trailing arguments of the call that invoked the current R session.
#' Useful when calling Rscript on a script and modifying its behavior via flags
#'
#' @examples
#' R_system_call_args(trailingOnly=FALSE)
#'
#' @family system-related functions provided by utilbox
#' @export
R_system_call_args = function(trailingOnly=TRUE) {
  args = commandArgs(trailingOnly=trailingOnly)
  args = unname(gsub("[\r\n]","",args))
  args[nchar(args)>0]
}

#' R session info
#'
#' Prints information about the current R session including the version of R
#' and the trailing arguments of the system call that started this session.
#'
#' @family system-related functions provided by utilbox
#' @export
R_session_info = function() {
  catn("R version information:")
  print(R.version)
  catn()
  catn("Program call: '",paste(commandArgs(trailingOnly=FALSE), collapse=" "),"'")
  catn("User home path: '",getwd(),"'")
  catn("Working path: '",Sys.getenv('R_USER'),"'")
}

#'
#'
#' @export
R_get_default_pckgs = function() {
  list("option(): defaultPackages"=getOption('defaultPackages'),
       "R_DEFAULT_PACKAGES"=Sys.getenv('R_DEFAULT_PACKAGES'))
}

#' Append a package to R's defaultly loaded packages
#'
#' This appends the specified package names to the system
#' variable which is consulted by R for what to load on
#' startup. 
#'
#' Note: On Windows, this does not seem to do anything.
#' Best use the '.Rprofile' file in the user's home directory
#' for instance via [R_add_lib_startup].
#'
#' @export
R_append_default_pckgs = function(..., system_var_name='R_DEFAULT_PACKAGES') {

  dots = match.call(expand.dots = FALSE)$`...`
  pckgs = as.character(dots)
  
  def_pckg = Sys.getenv(system_var_name) %|||||% NULL
  
  PCKGS = collapse0(c(def_pckg, pckg), sep=',')
  
  do.call(Sys.setenv, `names<-`(list(PCKGS), system_var_name))
  
}

#' Adjust R GUI
#'
#' Change the way the GUI looks (e.g. font size, font style). Without 
#' changing the default arguments, a call to \code{Rgui_adjust} makes 
#' the font smaller so that more info fits the screen, while \code{Rgui_reset}
#' resets the settings to their defaults.
#'
#' @name R_gui_adjust
#' @family system-related functions provided by utilbox
#' @export
R_gui_adjust = function(cmd = c("points = 7", "rows = 43", "columns = 177")) {
  
  if(is_rgui()) {
    
    temp = tempfile()
    catn(paste(cmd, collapse="\n"), file=temp)
    loadRconsole(file = temp)
    invisible(TRUE)
    
  } else invisible(FALSE)
  
}

#' @rdname R_gui_adjust
#' @family system-related functions provided by utilbox
#' @export
R_gui_reset = function() {

  loadRconsole(file = file.path(R.home(), "etc/Rconsole"))
  
  invisible(TRUE)
  
}

#' Default location for the .Rprofile file
#'
#' Returns the default path to the .Rprofile file in the
#' user's home directory.
#'
#' @family package-related functions provided by utilbox
#' @export
R_user_home = function(fsep=.Platform$file.sep) {
  gsub('\\\\',fsep,Sys.getenv("HOME"))
}

#' Default location for the .Rprofile file
#'
#' Returns the default path to the .Rprofile file in the
#' user's home directory.
#'
#' @family package-related functions provided by utilbox
#' @export
R_default_Rprofile_file = function() {
  file.path(R_user_home(), ".Rprofile")
}

#' Add/remove code to/from the .Rprofile file
#'
#' `R_add_code_startup` adds code (supplied as character 
#' string vector `code`) and adds lines to the .Rprofile 
#' file for the code to evaluated at R's start up.
#'
#' `R_del_code_startup` removes all lines that match the
#' supplied code.
#'
#'
#' Do not forget to double-backslash the supplied code.
#'
#' @examples
#' R_add_code_startup("print('Hi!')")   # check .Rprofile
#' R_del_code_startup("print('Hi!')")   # delete what was added
#'
#' @name code_startup
#' @family system-related functions provided by utilbox
#' @export
R_add_code_startup = function(code, Rprof_file) {
  
  if(missing(Rprof_file)) {
    Rprof_file = R_default_Rprofile_file()
  }
  
  catnf = hijack(catn, file=Rprof_file, append=TRUE)

  add_line = function(line) {
    added = "eval(parse(text=\"" %.% line %.% "\"))"
    catnf(added)
    added
  }
  
  catn('Appending code to the file ',Rprof_file,' ...')
  added = sapply(code, add_line)
  catn('Total of ',length(code),' line(s) of code appended to the file.')
  
  invisible(list(added_lines=added))

}

#' @rdname code_startup
#' @export
R_del_code_startup = function(code, Rprof_file, exact_match=FALSE, remove_all_matches=TRUE) {
  
  if(missing(Rprof_file)) {
    Rprof_file = R_default_Rprofile_file()
  }
  
  catnf = hijack(catn, file=Rprof_file, append=TRUE)

  matched_by_line = if(exact_match) {
    function(code) which(code==lines)
  } else {
    function(code) which(`%m%`(code, lines, fixed=TRUE))
  }

  catn("Reading file '",Rprof_file,"' ...")
  lines = readLines(Rprof_file)
  
  catn("Matching the lines in the file against the supplied code (",
       if(remove_all_matches) "all matches" else "only the first match for each supplied line",
       " will be removed) ...")
  is_match = lapply(code, matched_by_line)
  to_drop = unique(unlist(if(remove_all_matches) is_match else lapply(is_match, h1)))
  
  if(is_empty(to_drop)) {
    catn("No lines matched the supplied code. The file has not been modified.")
  } else {
    catn("Removing the matching code from the file ...")
    writeLines(lines[-to_drop], Rprof_file)
    catn('Total of ',length(to_drop),' lines of matching code were removed from the file.')
  }
  
  invisible(list(lines_dropped=lines[to_drop]))

}

#' An alias for the quit function
#'
#' This does essentially the same as \code{base::q} except it quits without saving
#' or asking to save the current workspace.
#'
#' @family system-related functions provided by utilbox
#' @export
.q = function(save="no") {
  base::q(save=save)
}

#' Toggle stopping on warning
#'
#' Sets `options()$warn` to either 0 or 2, which makes R either stop when a warning
#' is issued (>=2) or not (0). If argument `turn_on` is missing, the function acts
#' as a toggle turning stopping on warnings on and off on alternative calls.
#'
#' @export
.sow = function(turn_on, announce=TRUE) {
  
  if(missing(turn_on)) turn_on = options()$warn<2
  options(warn=ifelse(turn_on, 2, 0))
  
  status = ifelse(turn_on,"ENABLED","DISABLED")
  
  if(announce) note("Stopping on warnings has been ",status,".")
  
  invisible(status)
  
}

#' Toggle recovery on error
#'
#' Toggle the recovering on error option (same logic as \code{.sow()})
#'
#' @export
.roe = function(turn_on, announce=TRUE) {
  
  if(missing(turn_on)) turn_on = is.null(options()$error)
  options(error=if(turn_on) recover else NULL)
  
  status = ifelse(turn_on,"ENABLED","DISABLED")
  
  if(announce) note("Recovery on error has been ",status,".")
  
  invisible(status)
}

#' Pause execution
#'
#' Pauses execution for \code{time} seconds with announcement.
#'
#' @export
sleep = function(time) {
  cat0("Sleeping for ",time," seconds ... ")
  Sys.sleep(time)
  catn("done.")
  invisible(NULL)
}

#' Pause
#'
#' Pauses the execution until the user presses ENTER (for continue) or ESC (for quit)
#' @export
wait = function(...) {
  if(!is_empty(list(...))) do.call("catn", list(...))
  if(interactive()) {
    cat0("Press ENTER to continue or ESC to quit ...", fill = TRUE)
    input = scan("", what = "character", nmax=1, quiet=TRUE)
  }
}

redirect_print = function(...) {
  capture.output(...)
}