#' @title
#' Get the namespace of a package
#'
#' @description
#'
#' `get_package_namespace()` returns the namespace of the package 
#' with name in `pckg`. It is basically the same as `base::asNamespace()`
#' except it allows for package names to be supplied as symbols (e.g.,
#' like `base::library()` or `base::require()`). Stopping on error
#' is controlled via `stop_on_error`.
#'
#' `check_namespace()` checks whether the package `pckg` is installed 
#' and available and if not it throws an error (when `severity=2`, the 
#' default), or warning (when `severity=1`) or simply returns `FALSE` 
#' (when `severity=0`).
#'
#' @examples
#' get_package_namespace('base')
#' get_package_namespace(base)
#'
#' check_namespace('base')                      # TRUE (no error)
#' check_namespace('XYZABC12323')               # error
#' check_namespace('XYZABC12323', severity=1)   # warning
#' check_namespace('XYZABC12323', severity=0)   # FALSE (no error)
#'
#' @name namespaces
#' @family coding-related functions provided by utilbox
#' @export
get_package_namespace = function(pckg, character.only=FALSE, stop_on_error=FALSE) {

  if(!character.only) 
    pckg = as.character(substitute(pckg))
  
  res = try(base::asNamespace(pckg), silent=TRUE)
  
  if('try-error' %in% class(res) && stop_on_error)
    stop("Namespace '",pckg,"' not found.")
    
  res
  
}

#' @rdname namespaces
#' @export
check_namespace = function(pckg, envir=parent.frame(), severity=2) {
  
  res = requireNamespace(pckg, quietly=severity==0)
  
  if(res || severity==0) 
    return(res)
  
  parent_fun = parent_fun_name()
  
  error_msg = paste0("Package '", pckg, "' not found.")
  
  if(nzchar(parent_fun)) 
    error_msg = paste0(error_msg, " It must be installed to use the function `", parent_fun, "()`.")
  
  fun = if(severity==1) warning else stop
  
  fun(error_msg)
    
}

#' @title
#' Library loading and unloading operations
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
#' `package_is_installed()` checks if a package is installed.
#'
#' `is_lib_installed()` is an alias for `package_is_installed()`.
#'
#' `unload_all_libraries()` unloads all non-base libraries in the 
#' search path (i.e. those returned by `base::search()`).
#'
#' `reload_unloaded_libraries()` attempts to reverse the action by
#' `unload_all_libraries()`.
#'
#' @examples
#' package_is_installed('utilbox')
#' package_is_installed(utilbox)
#' lib = 'utilbox'; package_is_installed(lib)
#' lib = 'utilbox'; package_is_installed(lib, character.only=TRUE)
#'
#' @name llibrary
#'
#' @family package-related functions provided by utilbox
#' @export
llibrary = function(pckgs=NULL, quietly=TRUE, character.only=FALSE, fail=warn, 
    detach_first=FALSE, remove_first=FALSE, default_src="CRAN", lib_fun=base::library,
    url_CRAN="https://cloud.r-project.org/", suppress_startup_msgs=FALSE, ...) {

  echo = if(quietly) null else msgf

  ## If symbol names expected, make them into strings
  if(!character.only) 
    pckgs = as.character(substitute(pckgs))
    
  ## If no packages supplied, silently return
  if(length(pckgs)==0) 
    return()
   
  ## Get the currently loaded libraries
  loaded_packages = list_loaded_packages()
  
  ## Make sure pckgs is a list (use the default source)
  if(is.vector(pckgs)) {
    pckgs = lapply(pckgs, function(x) list(name=x, src=default_src))
  }

  ## Load necessary packages
  for(lib in pckgs) {
  
    # Check if package already loaded
    if(any(lib$name==loaded_packages)) {
      if(!detach_first && !remove_first) next
      echo("Detaching package ",lib$name," ...")
      detach(paste0('package:', lib$name), character.only=TRUE)
    }
    
    # Remove the package prior to loading
    if(remove_first) {
      echo("Uninstalling package ",lib$name," ...")
      remove.packages(lib$name)
    }
    
    # Announce loading of the current package
    echo("Loading package ",lib$name," ...")

    # Check if current package is installed
    if(all(lib$name!=installed.packages()[,"Package"])) {
    
      echo("Library '",lib$name,"' is not installed. Installing it from '",lib$src,"' ...")
      if(!is.null(lib$note)) echo(lib$note)

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
      src = if(lib$src=="CRAN") url_CRAN else lib$src
      fail("Package '",lib$name,"' was not found at '",src,"'. Please install it manually and try again.")
      next
    }

    # If this point reached without errors, it is installed, so the package is loaded
    echo("Loading package '",lib$name,"' ...")
    wrapper = if(suppress_startup_msgs) base::suppressPackageStartupMessages else base::identity
    wrapper(lib_fun(lib$name, character.only=TRUE, quietly=quietly, ...))
  
  } # for(lib in pckgs)

} # llibrary

#' @rdname llibrary
#' @export
llib = function(..., detach_first=FALSE, remove_first=FALSE, suppress_startup_msgs=FALSE) {

  pckgs = as.character(dots_to_nlist(keep_symbolic=TRUE))
  
  llibrary(pckgs, character.only=TRUE, detach_first=detach_first, 
           remove_first=remove_first, suppress_startup_msgs=suppress_startup_msgs)
           
}

#' @rdname llibrary
#' @export
unload_library = function(pckgs=NULL, character.only=FALSE, warn=TRUE) {

  if(!character.only) 
    pckgs = as.character(substitute(pckgs))
  
  unloaded = structure(rep(FALSE, length(pckgs)), names=pckgs)
  
  for(pckg in pckgs) {
  
    ps = ifelse(base::startsWith(pckg, "package:"), pckg, paste0("package:",pckg))
    res = try(detach(ps, character.only=TRUE, unload=TRUE), silent=TRUE)

    if('try-error' %in% class(res) && warn) {
      warning("Unloading of package '", pckg,"' failed.")
    } else {
      unloaded[pckg] = TRUE
    }
    
  }
  
  return(invisible(unloaded))
  
}

#' @rdname llibrary
#' @export
unload_attached_library = function(pckgs) {

  if(length(pckgs)>0) {
    detach(pckgs, character.only=TRUE, unload=TRUE)
  }

}

#' @rdname llibrary
#' @export
unload_all_libraries = function() {

  list_pckg_loaded = list_attached_packages(only_name=FALSE)
  list_pckg_base = list_loaded_packages(other=FALSE)
  
  list_pckg_loaded = setdiff(list_pckg_loaded, paste0('package:',list_pckg_base))
  
  unloaded = unload_attached_library(list_pckg_loaded)
  
  assign('.utilbox_unloaded_libraries', unloaded, envir=.GlobalEnv)

  invisible(unloaded)

}

#' @rdname llibrary
#' @export
reload_unloaded_libraries = function(pckgs) {

  if(missing(pckgs)) {
    pckgs = if(exists('.utilbox_unloaded_libraries', envir=.GlobalEnv)) {
      rev(get('.utilbox_unloaded_libraries', envir=.GlobalEnv))
    } else {
      NULL
    }
  }
  
  if(is.null(pckgs)) 
    return(invisible())

  
  reloaded = structure(rep(NA, length(pckgs)), names=pckgs)
  
  for(pckg in names(pckgs)) {
    if(pckgs[pckg]) {
      library(sub('package[:]','',pckg), character.only=TRUE)
      reloaded[pckg] = TRUE
    }
  }
  
  return(invisible(reloaded))

}

#' @rdname llibrary
#' @export
package_is_installed = function(pckgs, character.only=FALSE) {

  if(!character.only) 
    pckgs = as.character(substitute(pckgs))
  
  structure(sapply(pckgs, requireNamespace, quietly=TRUE), names=pckgs)
  
}

#' @rdname llibrary
#' @export
is_lib_installed = package_is_installed

#' @title
#' Listing of installed and/or loaded packages
#'
#' @description
#'
#' `list_installed_packages()` lists all installed packages.
#'
#' `list_loaded_packages()` and `list_attached_packages()` 
#' both list all loaded packages. The former using 
#' `utils::sessionInfo()`, the latter based on `base::search()`.
#'
#' @examples
#' list_installed_packages()
#' list_loaded_packages()
#' list_attached_packages()
#'
#' @family package-related functions provided by utilbox
#' @export
list_installed_packages = function() {

  utils::installed.packages()[,"Package"]
  
}
  
#' @rdname list_installed_packages
#' @export
list_loaded_packages = function(base=TRUE, other=TRUE) {

  c(if(base) sessionInfo()$basePkgs, if(other) names(sessionInfo()$otherPkgs))
  
}

#' @rdname list_installed_packages
#' @export
list_attached_packages = function(only_name=TRUE, list_ignore=c('.GlobalEnv','Autoloads')) {

  plist = setdiff(base::search(), list_ignore)
  
  if(only_name) 
    plist = sub('^package[:]', '', plist)  
  
  plist

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
#' `as_object_table()` puts the objects and their main characteristics into 
#' a single table (class `data.frame`).
#'
#' @examples
#' list_package_exported('utilbox')
#' list_package_all('utilbox')
#'
#' # lsf.str("package:dplyr")   # see also
#' # ls("package:dplyr")        # see also (works when the package is loaded)
#' help(package = dplyr)
#' # see https://stackoverflow.com/questions/30392542/is-there-a-command-in-r-to-view-all-the-functions-present-in-a-package
#'
#' @export
list_package_objects = function(pckg, pattern, all.names=TRUE, exclude=FALSE, what=c('all','exported'), mode=NULL, warn=TRUE, quietly=FALSE) {

  what = match.arg(what)
  
  if(!is.character(pckg))
    stop("Supply package name as character.")
    
  if(!namespace_exists(pckg)) {
    if(warn) warning("Namespace '",pckg,"' does not exist and thus its objects cannot be obtained.")
    return(NULL)
  }

  detail = ifelse(what=='all', ifelse(all.names, "visible","existing"), 'exported')
  if(!quietly) msgf("Listing all ",detail," objects in the package '",pckg,"' ...")
  
  objs = if(what=='all') {
    ls(envir=getNamespace(pckg), all.names=all.names)
  } else {
    filter_out(getNamespaceExports(pckg), ifelse(all.names, '^$', '^[.]'))
  }
  
  as_object_table(objs, pckg, pattern, exclude, mode)
  
}

#' @rdname list_package_objects
#' @export
list_package_exported = function(pckg, pattern, all.names=TRUE, exclude=FALSE, mode=NULL, warn=TRUE, quietly=FALSE) {
  nlapply(pckg, list_package_objects, pattern=pattern, all.names=all.names, 
          exclude=exclude, what='exported', mode=mode, warn=warn, quietly=quietly)
}

#' @rdname list_package_objects
#' @export
list_package_all = function(pckg, pattern, all.names=TRUE, exclude=FALSE, mode=NULL, warn=TRUE, quietly=FALSE) {
  nlapply(pckg, list_package_objects, pattern=pattern, all.names=all.names, 
          exclude=exclude, what='all', mode=mode, warn=warn, quietly=quietly)
}

#' @rdname list_package_objects
#' @export
list_package_duplicates = function(pckgs, quietly=FALSE, sep='|') {

  if(!quietly) {
    msgf('Identifying functions found in multiple packages ...')
  }

  if(missing(pckgs)) {
    pckgs = list_installed_packages()
  }
  
  x = do.call(base::rbind, list_package_exported(pckgs, warn=FALSE, quietly=quietly))
  rownames(x) = NULL
  z = x['function' %in% z$all_classes,c('package','object_name')]
  cnts = tapply(z$package, z$object_name, length)
  pckg_nams = tapply(z$package, z$object_name, paste, collapse=sep)
  
  res = data.frame(object_name=names(cnts), count=cnts, packages=pckg_nams)[cnts>1]
  
  if(!quietly) {
    msgf('List of functions exported by multiple packages:')
    print(res)
  }
  
  invisible(res)

}

#' @rdname list_package_objects
#' @export
as_object_table = function(objs, pckg, pattern, exclude=FALSE, mode) {

  if(!is.character(pckg) && !is.environment(pckg))
    stop("Supply either a package name (character) or an environment.")
    
  if(is.character(pckg) && !namespace_exists(pckg))
    stop("Namespace '",pckg,"' not found.")

  if(namespace_exists(pckg)) {  
    is_exported = objs %in% getNamespaceExports(pckg)
    env_pckg = orig_env(asNamespace(pckg))
  } else {
    is_exported = NULL
    env_pckg = orig_env(pckg)
  }
  
  class1 = apply_pckg(objs, pckg, function(x) head(class(x)),1)
  classes = apply_pckg(objs, pckg, function(x) paste(class(x), collapse=','))
  namespace = apply_pckg(objs, pckg, function(x) orig_env(x))
  namespace = ifelse(namespace==env_pckg, '', namespace)

  # put the information into a list
  tbl = list(location=if(is.environment(pckg)) print2var(pckg) else pckg,
             object_name=objs %|||% NA_character_, 
             exported=ifelse(is_exported, 'YES', 'no') %|||% NA_character_, 
             primary_class=class1 %|||% NA_character_, 
             all_classes=classes, 
             original_namespace=namespace)
  
  # make sure the list is not completely empty and convert it to a data frame
  tbl = lapply(tbl, `%||||%`, NA_character_)
  tbl = as.data.frame(list_clean(tbl), stringsAsFactors=FALSE)
  
  # drop the rows that were artificially added
  tbl = tbl[!is.na(tbl$object_name),]

  if('object_name' %nin% colnames(tbl)) 
    return(tbl) 
  
  if(!missing(pattern) && !str_is_empty(pattern)) {
    #tbl = tbl[`%m_any%`(pattern, tbl$object_name, exclude=exclude),]
    tbl = tbl[tbl$object_name %notlikeany% pattern,]
  }
  
  if(!missing(mode) && is.character(mode)) {
    mode_fits = apply_pckg(tbl$object_name, pckg, function(x, nam) mode(x) == mode, value_error=FALSE)
    tbl = tbl[mode_fits,]
  }
  
  if(nrow(tbl)>0) {
    tbl = `rownames<-`(sort_df(tbl, primary_class, object_name), 1:nrow(tbl))
  }
  
  tbl

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
#' package as done for example by [`as_object_table`].
#'
#' @examples
#' apply_pckg('rev_cols', 'utilbox', class)    # returns 'function'
#'
#' @family package-related functions provided by utilbox
#' @export
apply_pckg = function(objs, pckg, f, ..., workhorse=sapply, value_error=NA) {

  if(is.character(pckg)) pckg = getNamespace(pckg)
  
  fun_to_apply = function(n) {
    args = head(list(get(n, envir=pckg), n, ...), nformals(f),1)
    try(do.call(f, args), silent=TRUE) %ERRCLS% value_error
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

#' Package dependencies
#'
#' `lib_dependencies` lists dependencies for all packages supplied in `pckg`. 
#' It is nothing but a wrapper for `tools::package_dependencies`.
#'
#' `install_package_dependencies` (re)installs all dependencies of the 
#' supplied packages. Useful when an error occurred during a previous
#' package installation and some packages were not installed.
#'
#' @family package-related functions provided by utilbox
#' @export
lib_dependencies = function(pckg, ...) {
  tools::package_dependencies(pckg, ...)
}

#' @rdname lib_dependencies
#' @export
install_package_dependencies = function(pckg, ..., skip_installed=TRUE, quiet=FALSE) {
  
  msgf('Listing all package dependencies ...')
  pckgs = unlist(lib_dependencies(pckg))
  
  if(length(pckgs)==0) {
    msgf('The supplied packages have no dependencies.')
    return(invisible(NULL))
  }
  
  if(skip_installed) {
    pckgs = setdiff(pckgs, installed.packages()[,'Package'])
  }
  
  if(length(pckgs)==0) {
    msgf('All dependencies are already installed.')
  }
  
  for(p in pckgs) {
    install.packages(p, ...)
  }
  
}

#' Identify object's package
#'
#' `which_package` tries to identity the package in which the
#' supplied object is located or from which it came from.
#'
#' @family package-related functions provided by utilbox
#' @export
which_package = function(object_name) {
  sub('>','',sub('.*[:][ ]?','',print2var(environment(object_name))))
}

#' Enable/disable auto-installation of packages
#'
#' `enable_auto_install()` enables auto-installation of packages by placing
#' functions `library()` and/or `require()` into the global environment
#' (i.e., `.GlobalEnv`), which then supperseed the calls to `base::library()`
#' and/or `base::require()`.
#'
#' `disable_auto_install()` deletes any functions by those names in the global 
#' environment, thereby disabling the auto-installation.
#'
#' `.eai()` and `.dai()` are aliases for `enable_auto_install()` and 
#' `disable_auto_install()`, respectively.
#'
#' @family package-related functions provided by utilbox
#' @name auto_install_packages
#' @export
.eai = function(call_type=c('library','require')) {
  message('Enabling auto-installation of packages on load via "bare" (i.e., without an explicit',
          ' reference to a package) calls to `',paste(call_type, collapse='()`, `'),'()` ...')
  for(f in call_type) {
    if(exists(f, envir=.GlobalEnv, inherits=FALSE)) next    
    assign(f, utilbox::llib, envir=.GlobalEnv)
  }
 
  return(invisible(NULL))
}

#' @rdname auto_install_packages
#' @export
.dai = function(call_type=c('library','require')) {
  message('Disabling auto-installation of packages on load via "bare" (i.e., without an explicit',
          ' reference to a package) calls to `',paste(call_type, collapse='()`, `'),'()` ...')
  for(f in call_type) {
    if(!exists(f, envir=.GlobalEnv, inherits=FALSE)) next
    rm(list=f, envir=.GlobalEnv)
  }
  return(invisible(NULL))
}

#' @rdname auto_install_packages
#' @export
enable_auto_install = .eai

#' @rdname auto_install_packages
#' @export
disable_auto_install = .dai

#' Finding functions by substrings
#'
#' `str_find_in_funtion()` searches the function body of the specified 
#' function (via `fun`) for the given `string`. If the string is found,
#' it returns the corresponding line(s) (as obtained by printing the 
#' result of a call to `base::body()`), otherwise `NULL` is returned. 
#'
#' `str_find_in_package()` searches for the string in functions within
#' a package. Either all functions when (`what='all'`) or only exported
#' functions (when `what='exported'`) are searched.
#'
#' @examples
#' str_find_in_function('contrasts', 'lm', envir=asNamespace('stats')) # returns lines 3, 49, 59
#' str_find_in_package('contrasts', 'stats') # returns 28 functions from the package `stats`
#'
#' @name str_find_in
#' @export 
str_find_in_function = function(string, fun, envir=parent.frame()) {

  if(is.character(fun))
    fun = get(fun, envir=envir)
    
  grep(string, print2var(fun))
  
}

#' @rdname str_find_in
#' @export 
str_find_in_package = function(string, pckg, what='all') {

  funs = list_package_objects(pckg, mode='function', what=what, quiet=TRUE)$object_name
  names(funs) = funs
  
  locs = lapply(funs, str_find_in_funtion, string=string, envir=asNamespace(pckg))
  
  list_clean(locs)
  
}
