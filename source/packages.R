#' Library operations
#'
#' \code{llibrary} loads specified libraries. On input, \code{pckgs} must either 
#' be a character vector of package names or a list with each of its elements 
#' having the structure of \code{list(name=..., src=...)} where name is the package 
#' name and src is the package source (repository or local) from which the package 
#' is to be loaded. 
#'
#' \code{llib} is an alias for \code{llibrary} which allows unquoted input.
#' 
#' \code{unload_library} unloads/detaches a loaded library.
#'
#'
#' \code{list_installed_packages} lists all installed packages.
#'
#' \code{list_loaded_packages}) list all loaded packages.
#'
#' (\code{package_is_installed}) checks if a package is installed.
#'
#' @examples
#' list_installed_packages()
#' list_loaded_packages()
#' package_is_installed('utilbox')
#'
#' @name llibrary
#'
#' @family package-related functions provided by utilbox
#' @export
llibrary = function(pckgs=NULL, quietly=TRUE, character.only=FALSE, fail=warn, 
  detach=FALSE, default_src="CRAN", url_CRAN="https://cloud.r-project.org/") {

  ## If symbol names expected, make them into strings
  if(!character.only) pckgs = as.character(substitute(pckgs))
    
  ## If no packages supplied, silently return
  if(length(pckgs)==0) return()
   
  ## Get the currently loaded libraries
  loaded_packages = list_loaded_packages()
  
  ## Make sure pckgs is a list (use CRAN as default source)
  if(is.vector(pckgs)) {
    pckgs = lapply(pckgs, function(x) list(name=x, src=default_src))
  }

  ## Load necessary packages
  for(lib in pckgs) {
  
    # Check if package already loaded
    if(any(lib$name==loaded_packages)) {
      if(!detach) next
      catn("Detaching package ",lib$name," ...")
      detach('package:'%.%lib$name, character.only=TRUE)
    }
    
    # Announce loading of the current package
    if(!quietly) catn("Loading package ",lib$name," ...")

    # Check if current package is installed
    if(all(lib$name!=installed.packages()[,"Package"])) {
    
      catn("Library '",lib$name,"' is not installed. Installing it from '",lib$src,"' ...")
      if(!is.null(lib$note)) catn(lib$note)

      # Check for location information
      if(is.null(lib$src) || is.na(lib$src)) {
        fail("Library ",lib$name," cannot be installed due to missing",
             " source information. Try installing it manually.")
        next
      }

      # Try to install it
      if(lib$src=="CRAN" || regexpr("^http[s]?://.*",lib$src)>0) {
        type = ifelse(is.null(lib$type), getOption("pkgType"), lib$type)
        install.packages(lib$name, type=type, repos=url_CRAN)
      } else {
        if(file.exists(lib$src)) install.packages(lib$src, repos=NULL)
      }
    }
    
    # Check if installed now
    if(all(lib$name!=installed.packages()[,"Package"])) {
      fail("Package '",lib$name,"' was not available at '",
           if(lib$src=="CRAN") url_CRAN else lib$src,"'.", 
           " Please install it manually and try again.")
      next
    }

    # If this point reached without errors, it is installed, so the package is loaded
    if(!quietly) catn("Loading package '",lib$name,"' ...")
    require(lib$name, character.only=TRUE, quietly=quietly)
  
  } # for(lib in pckgs)

} # llibrary

#' @rdname llibrary
#' @family package-related functions provided by utilbox
#' @export
llib = function(..., detach=FALSE) {
  dots = match.call(expand.dots = FALSE)$`...`
  pckgs = as.character(dots)
  llibrary(pckgs, character.only=TRUE, detach=detach)
}

#' @rdname llibrary
#' @family package-related functions provided by utilbox
#' @export
unload_library = function(pckgs=NULL, character.only=FALSE) {
  if(!character.only) pckgs = as.character(substitute(pckgs))
  for(pckg in pckgs) detach(paste0("package:",pckg), character.only=TRUE, unload=TRUE)
  return(invisible(TRUE))
  
}

#' @rdname llibrary
#' @family package-related functions provided by utilbox
#' @export
list_installed_packages = function() {
  x = try(installed.packages()[,"Package"])
  if(class(x)=="try-error") "" else x
}
  
#' @rdname llibrary
#' @family package-related functions provided by utilbox
#' @export
list_loaded_packages = function() {
  c(sessionInfo()$basePkgs, names(sessionInfo()$otherPkgs))
}

#' @rdname llibrary
#' @family package-related functions provided by utilbox
#' @export
package_is_installed = function(pckgs, character.only=FALSE) {
  if(!character.only) pckg = as.character(substitute(pckg))
  `names<-`(sapply(pckgs, requireNamespace, quietly=TRUE), pckgs)
}

#' List objects in a package
#'
#' Lists all/exported objects in a package.
#'
#' \code{list_package_exported} lists all exported objects by a package.
#'
#' \code{list_package_all} lists all objects defined in a package (including
#' non-exported objects).
#'
#' `package_table` puts the objects into a table together 
#'
#' @examples
#' list_package_exported(utilbox)
#' list_package_all(utilbox)
#'
#' # lsf.str("package:dplyr")   # see also
#' # ls("package:dplyr")        # see also (works when the package is loaded)
#' help(package = dplyr)
#' # see https://stackoverflow.com/questions/30392542/is-there-a-command-in-r-to-view-all-the-functions-present-in-a-package
#'
#' @name list_package
#' @family package-related functions provided by utilbox
#' @export
list_package_exported = function(pckg, character.only=FALSE) {

  if(!character.only) pckg = as.character(substitute(pckg))
  objs = getNamespaceExports(pckg)
  package_table(objs, pckg)
  
}

#' @rdname list_package
#' @export
list_package_all = function(pckg, character.only=FALSE, all.names=TRUE) {

  if(!character.only) pckg = as.character(substitute(pckg))
  objs = ls(envir=getNamespace(pckg), all.names=all.names)
  package_table(objs, pckg)
  
}

#' @rdname list_package
#' @export
package_table = function(objs, pckg) {

  stopifnot(is.character(pckg))

  is_exported = objs %in% getNamespaceExports(pckg) 
  clas1 = apply_pckg(objs, pckg, function(x, nam) h1(class(x)))
  classes = apply_pckg(objs, pckg, function(x, nam) collapse0(class(x),','))
  namespace = apply_pckg(objs, pckg, function(x, nam) t1(fun_code_to_text(x)))
  #self_reference = apply_pckg(objs, pckg, function(x, nam) any(('[,(/%! ]'%.%patternize(nam)%.%'[(, ]') %m% fun_code_to_text(x)))
  
  `rownames<-`(data.frame(object_name=objs, 
                          exported=ifelse(is_exported, 'YES', 'no'), 
                          object_primary_class=clas1, 
                          object_all_classes=classes, 
                          original_namespace=namespace), NULL)
  
}

#' Apply function to an object in a package
#'
#' Applies function \code{f} to the object(s) whose names are in 
#' \code{objs} and that are found in the package \code{pckg}. Useful
#' for applying a function (e.g. \code{base::class}) to non-exported 
#' objects inside a package.
#'
#' @examples
#' apply_pckg('rev_cols', 'utilbox', class)    # returns 'function'
#'
#' @family package-related functions provided by utilbox
#' @export
apply_pckg = function(objs, pckg, f, ..., workhorse=sapply) {
  workhorse(objs, function(n) do.call(f, list(get(n, envir=getNamespace(pckg)), n, ...)))
}

#' Add path to the package path
#'
#' Adds a supplied path to the list of paths for searching packages
#'
#' @param libpath path to be added
#'
#' @details
#' Adds a supplied path to the list of paths for searching packages. This is useful
#' for instance when the default path is in a location which is write-protected and
#' packages need to be installed into an alternative location.
#'
#' @return A character vector with each command of the input function as an element.
#'
#' @name set_pkglib
#'
#' @family package-related functions provided by utilbox
#' @export
set_pkglib = function(libpath) {
  if(is.null(libpath)) return(invisible(0))
  if(.libPaths()[1]!=libpath) return(invisible(1))
  cat("Setting primary local R library to '",libpath,"' ...\n", sep="")
  dir_exist_check(libpath)
  .libPaths(libpath)
  libpath %in% .libPaths()
}

#' Set a library to load automatically
#'
#' Add a library to the list of libraries loaded automatically at startup of R.
#'
#' @examples
#' #R_add_lib_startup('utilbox')   # run this example only if you want to set 'utilbox' to load on startup
#'
#' @family package-related functions provided by utilbox
#' @export
R_add_lib_startup = function(pckg, Rprof_file, check_duplicate_load=TRUE) {
  
  if(missing(pckg) || is_empty(pckg)) {
    return(invisible(NULL))
  }
  
  if(missing(Rprof_file)) {
    Rprof_file = R_default_Rprofile_file()
  }
  
  catn('Modifying the file ',Rprof_file,' ...')
  catn('Packages to be added among those that load at startup: ',collapse0(pckg, sep=", "))
  
  all_pckgs = union(getOption('defaultPackages'), pckg)

  catnf = hijack(catn, file=Rprof_file, append=TRUE)
  catnf("options(defaultPackages=c('",collapse0(all_pckgs, sep="','"),"'))")
  catnf("cat(\"Packages loaded at startup:", " \",sQuote('" %.% all_pckgs %.% "'),\"", "\\n\")")
  
  catn("File ",Rprof_file," modified.")
  catn("You must reload the R session for any changes to take effect.")
  return(invisible(NULL))
}

