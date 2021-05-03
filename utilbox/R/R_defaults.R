#' @title
#' Default setting for R
#'
#' @description
#'
#' `R_get_default_pckgs()` lists the packages that R loads on startup.
#'
#' `R_add_lib_startup()` adds a library to the list of libraries 
#' loaded automatically at startup of R.
#'
#' `R_append_default_pckgs()` appends the specified package names to 
#' the system variable 'R_DEFAULT_PACKAGES', which is supposed to be 
#' consulted by R for what to load on startup. However, on Windows, this 
#' does not seem to do anything. It is better to use the '.Rprofile' 
#' file in the user's home directory for instance.
#'
#' `R_add_code_startup()` adds code (supplied as character string 
#' vector `code`) and adds lines to the .Rprofile file for the code to 
#' evaluated at R's start up.
#'
#' `R_del_code_startup()` removes all lines that match the supplied 
#' code.
#'
#' Do not forget to double-backslash the supplied code.
#'
#' @examples
#' #R_add_lib_startup('utilbox')   # run this example only if you want to set 'utilbox' to load on startup
#'
#' R_add_code_startup("print('Hi!')")   # check .Rprofile
#' R_del_code_startup("print('Hi!')")   # delete what was added
#'
#' @name R_defaults
#' @family system-related functions provided by utilbox
#' @export
R_get_default_pckgs = function() {

  list("Loaded via option('defaultPackages')"=getOption('defaultPackages'),
       "Loaded via R_DEFAULT_PACKAGES"=Sys.getenv('R_DEFAULT_PACKAGES'))
       
}

#' @rdname R_defaults
#' @export
R_append_default_pckgs = function(..., system_var_name='R_DEFAULT_PACKAGES') {

  dots = match.call(expand.dots = FALSE)$`...`
  pckgs = as.character(dots)
  
  def_pckg = Sys.getenv(system_var_name) %|||||% NULL
  
  PCKGS = collapse0(c(def_pckg, pckg), sep=',')
  
  do.call(Sys.setenv, `names<-`(list(PCKGS), system_var_name))
  
}

#' @rdname R_defaults
#' @export
R_add_lib_startup = function(pckg, Rprof_file, check_duplicate_load=TRUE) {
  
  if(missing(pckg) || is_empty(pckg)) {
    message('Specify a library to add to startup.')
    return(invisible(NULL))
  }
  
  if(missing(Rprof_file)) {
    Rprof_file = R_default_Rprofile_file()
  }
  
  message('Modifying the file ',Rprof_file,' ...')
  message('Packages to be added among those that load at startup: ',collapse0(pckg, sep=", "))
  
  all_pckgs = union(getOption('defaultPackages'), pckg)

  wrt2fil = hijack(catn, file=Rprof_file, append=TRUE)
  wrt2fil(".inst = utils::installed.packages()[,'Package']")
  wrt2fil(".defs = c('",collapse0(all_pckgs, sep="','"),"')")
  wrt2fil(".defs_avail = intersect(.defs, .inst)")
  wrt2fil(".defs_miss = setdiff(.defs, .inst)")
  wrt2fil("options(defaultPackages = .defs_avail)")
  #wrt2fil("cat('Packages loaded at startup:', " \",sQuote('" %p% all_pckgs %p% "'),\"", "\\n\")")
  wrt2fil("cat('Packages loaded at startup: ',paste(sapply(.defs_avail, sQuote), collapse=' '),'\\n')")
  wrt2fil("if(length(.defs_miss)>0) cat('Packages not loaded at startup (not installed): ',paste(sapply(.defs_miss, sQuote), collapse=' '),'\\n')")
  wrt2fil("rm(.defs, .inst, .defs_avail, .defs_miss)")

  
  message("File ",Rprof_file," modified.")
  message("You must reload the R session for any changes to take effect.")
  
  return(invisible(NULL))
  
}

#' @rdname R_defaults
#' @export
R_add_code_startup = function(code, Rprof_file) {
  
  if(missing(Rprof_file)) {
    Rprof_file = R_default_Rprofile_file()
  }
  
  wrt2fil = hijack(catn, file=Rprof_file, append=TRUE)

  add_line = function(line) {
    added = "eval(parse(text=\"" %p% line %p% "\"))"
    wrt2fil(added)
    added
  }
  
  catn('Appending code to the file ',Rprof_file,' ...')
  added = sapply(code, add_line)
  catn('Total of ',length(code),' line(s) of code appended to the file.')
  
  invisible(list(added_lines=added))

}

#' @rdname R_defaults
#' @export
R_del_code_startup = function(code, Rprof_file, exact_match=FALSE, remove_all_matches=TRUE) {
  
  if(missing(Rprof_file)) {
    Rprof_file = R_default_Rprofile_file()
  }
  
  matched_by_line = if(exact_match) {
    function(code) which(code==lines)
  } else {
    function(code) which(`%m%`(code, lines, fixed=TRUE))
  }

  catn("Reading file '",Rprof_file,"' ...")
  lines = readLines(Rprof_file)
  
  message("Matching the lines in the file against the supplied code (",
       if(remove_all_matches) "all matches" else "only the first match for each supplied line",
       " will be removed) ...")
       
  is_match = lapply(code, matched_by_line)
  to_drop = unique(unlist(if(remove_all_matches) is_match else lapply(is_match, h1)))
  
  if(is_empty(to_drop)) {
    message("No lines matched the supplied code. The file has not been modified.")
  } else {
    message("Removing the matching code from the file ...")
    writeLines(lines[-to_drop], Rprof_file)
    message('Total of ',length(to_drop),' lines of matching code were removed from the file.')
  }
  
  invisible(list(lines_dropped=lines[to_drop]))

}
