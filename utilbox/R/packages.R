#' @title
#' Get the namespace of a package
#'
#' @description
#'
#' `get_package_namespace()` returns the namespace of the package 
#' with name in `pckg`.
#'
#' `check_namespace()` checks whether the package `pckg` is installed 
#' and available and throws an error if not.
#'
#' @examples
#' get_package_namespace('base')
#'
#' check_namespace('base')          # no error
#' check_namespace('XYZABC12323')   # error
#'
#' @name namespaces
#' @family coding-related functions provided by utilbox
#' @export
get_package_namespace = function(pckg, character.only=FALSE, stop_on_error=FALSE) {

  if(!character.only) pckg = as.character(substitute(pckg))
  
  ns = try(as.environment("package:"%.%pckg), silent=TRUE)
  
  if(is_error(ns) && stop_on_error)
    error("Namespace for package '",pckg,"' not found.")
    
  ns
  
}

#' @rdname namespaces
#' @export
check_namespace = function(pckg, envir=parent.frame()) {
  
  fun_name = eval(parse(text='this_fun_name()'), envir=envir)
  
  if(!requireNamespace(pckg)) 
    stop("Package '",pckg,"' needs to be installed to use function '",fun_name,"'.")
    
}

#' @title
#' Library operations
#'
#' @description
#'
#' `llibrary()` loads specified libraries. On input, `pckgs` must 
#' either be a character vector of package names or a list with each of 
#' its elements having the structure of \code{list(name=..., src=...)} 
#' where name is the package name and src is the package source 
#' (repository or local) from which the package is to be loaded.
#'
#' `llib()` is an alias for `llibrary()` which allows unquoted input.
#'
#' `unload_library()` unloads/detaches a loaded library.
#'
#' `list_installed_packages()` lists all installed packages.
#'
#' `list_loaded_packages()`) list all loaded packages.
#'
#' `package_is_installed()` checks if a package is installed.
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
  detach_first=FALSE, default_src="CRAN", url_CRAN="https://cloud.r-project.org/") {

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
      if(!detach_first) next
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
      if(lib$src=="CRAN" || grepl("^http[s]?://.*", lib$src)) {
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
#' @export
llib = function(..., detach_first=FALSE) {
  pckgs = as.character(dots_to_nlist(keep_symbolic=TRUE))
  llibrary(pckgs, character.only=TRUE, detach_first=detach_first)
}

#' @rdname llibrary
#' @export
unload_library = function(pckgs=NULL, character.only=FALSE) {
  if(!character.only) pckgs = as.character(substitute(pckgs))
  for(pckg in pckgs) {
    detach(paste0("package:",pckg), character.only=TRUE, unload=TRUE)
  }
  return(invisible(TRUE))
  
}

#' @rdname llibrary
#' @export
list_installed_packages = function() {
  x = try(installed.packages()[,"Package"])
  if(class(x)=="try-error") "" else x
}
  
#' @rdname llibrary
#' @export
list_loaded_packages = function() {
  c(sessionInfo()$basePkgs, names(sessionInfo()$otherPkgs))
}

#' @rdname llibrary
#' @export
package_is_installed = function(pckgs, character.only=FALSE) {
  
  if(!character.only) pckg = as.character(substitute(pckg))
  
  `names<-`(sapply(pckgs, requireNamespace, quietly=TRUE), pckgs)
  
}

#' @title
#' List objects in a package
#'
#' @description
#'
#' List all/exported objects in a package.
#'
#' `list_package_exported()` lists all exported objects by a package.
#'
#' `list_package_all()` lists all objects defined in a package 
#' (including non-exported objects).
#'
#' `object_table()` puts the objects into a table together
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
list_package_exported = function(pckg, pattern, character.only=FALSE, mode) {

  if(!character.only) pckg = as.character(substitute(pckg))
  objs = getNamespaceExports(pckg)
  
  object_table(objs, pckg, pattern, mode)
  
}

#' @rdname list_package
#' @export
list_package_all = function(pckg, pattern, character.only=FALSE, all.names=TRUE, mode) {

  if(!character.only) pckg = as.character(substitute(pckg))
  objs = ls(envir=getNamespace(pckg), all.names=all.names)
  
  object_table(objs, pckg, pattern, mode)
  
}

#' @rdname list_package
#' @export
object_table = function(objs, pckg, pattern, mode) {

  if(!is.character(pckg) && !is.environment(pckg))
    error("Supply either a package name (character) or an environment.")
    
  if(is.character(pckg) && !namespace_exists(pckg))
    error("Namespace '",pckg,"' not found.")

  if(namespace_exists(pckg)) {  
    is_exported = objs %in% getNamespaceExports(pckg)
    env_pckg = orig_env(asNamespace(pckg))
  } else {
    is_exported = NULL
    env_pckg = orig_env(pckg)
  }
  
  class1 = apply_pckg(objs, pckg, function(x) h1(class(x)))
  classes = apply_pckg(objs, pckg, function(x) collapse0(class(x), sep=','))
  namespace = apply_pckg(objs, pckg, function(x) orig_env(x))
  namespace = ifelse(namespace==env_pckg, '', namespace)

  #self_reference = apply_pckg(objs, pckg, function(x, nam) any(('[,(/%! ]'%.%str_patternize(nam)%.%'[(, ]') %m% fun_code_to_text(x)))
  
  tbl = list(name=objs, 
             exported=ifelse(is_exported, 'YES', 'no'), 
             primary_class=class1, 
             all_classes=classes, 
             original_namespace=namespace)
  
  tbl = as.data.frame(list_clean(tbl), stringsAsFactors=FALSE)
  rownames(tbl) = NULL
  
  if(!missing(pattern)) {
    tbl = tbl[`%m%`(pattern, tbl$name),]
  }
  
  if(!missing(mode)) {
    mode_fits = apply_pckg(tbl$name, pckg, function(x, nam) mode(x) == mode, value_error=FALSE)
    tbl = tbl[mode_fits,]
  }
  
  sort_df(tbl, primary_class, name)

}

#' @title
#' Apply function to an object in a package or environment
#'
#' @description
#'
#' Applies function `f` to the object(s) whose names are in `objs` 
#' and that are found in `pckg`, which is primarily intended to be a 
#' package name (assume to be one when of class `character`), but can 
#' also be an environment. The elipsis (`...`) is supplied to the 
#' function `f`. `f` can have any number of arguments and only as many 
#' as it accepts are supplied to it (as determined via [`nformals`]).
#'
#' This function is useful for applying a function `f` (e.g. 
#' [`base::class`]) to exported and/or non-exported objects inside a 
#' package as done for example by [`object_table`].
#'
#' @examples
#' apply_pckg('rev_cols', 'utilbox', class)    # returns 'function'
#'
#' @family package-related functions provided by utilbox
#' @export
apply_pckg = function(objs, pckg, f, ..., workhorse=sapply, value_error=NA) {

  if(is.character(pckg)) pckg = getNamespace(pckg)
  
  fun_to_apply = function(n) {
    args = h1(list(get(n, envir=pckg), n, ...), nformals(f))
    try(do.call(f, args), silent=TRUE) %ERR% value_error
  }

  workhorse(objs, fun_to_apply)
  
}

#' @title
#' Add path to the package path
#'
#' @description
#'
#' Adds a supplied path to the list of paths for searching packages.
#'
#' @param libpath path to be added
#'
#' @details Adds a supplied path to the list of paths for searching 
#' packages. This is useful for instance when the default path is in a 
#' location which is write-protected and packages need to be installed 
#' into an alternative location.
#'
#' @return A character vector with each command of the input function 
#' as an element.
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

