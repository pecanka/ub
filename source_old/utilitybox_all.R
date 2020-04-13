################################################################################
########################### DEFINITION OF FUNCTIONS ############################
################################################################################

###
# Example of the usage of the trace function
#
# Let's insert a call to the browser function at line 4 of the function force_as_real"
#
# trace(force_as_real, browser, at=4)
# force_as_real('a1.4')
# untrace(force_as_real, browser)
#
# Example of how to change the arguments of a function
#
# formals(cat)$sep <- ""
# trace(base::cat, tracer=quote(if(missing(sep)) sep=''), at=1)
# cat = purrr::partial(cat, sep="")
#
###

################################################################################

#' Set a library to load automatically
#'
#' Add a library to the list of libraries loaded automatically at startup of R
#'
#' @examples
#' #set_lib_to_load_at_startup('utilbox')   # run this example only if you want to set 'utilbox' to load on startup
#'
#' @export
set_lib_to_load_at_startup = function(lib, Rprofile_file, check_duplicate_load=TRUE) {
  
  if(missing(lib) || length(lib)==0) return(invisible(NULL))
  
  if(missing(Rprofile_file)) Rprofile_file = file.path(Sys.getenv("HOME"), ".Rprofile")
  
  for(l in lib) {
    
    if(check_duplicate_load && file.exists(Rprofile_file)) {
      x = scan(Rprofile_file, what='character', quiet=TRUE, sep='\n')
      pattern = paste0(c('library','require'),"[(][\"']?",l,"[\"']?[)]")
      if(length(x)>0 && any(sapply(pattern, regexpr, x)>0)) {
        catn("Library "%.%l%.%" appears to already have been set to load on startup by this function.")
        next
      }
    }
    
    catn("Adding the loading of library '",l,"' to the file '",Rprofile_file,"' ...")
    catn("\n################################################", file=Rprofile_file, append=TRUE)
    catn("# The following lines were added by utilbox::set_lib_to_load_at_startup", file=Rprofile_file, append=TRUE)
    catn("cat('Loading library "%.%l%.%" ...\\n')", file=Rprofile_file, append=TRUE)
    catn("try(library("%.%l%.%"))", file=Rprofile_file, append=TRUE)
    catn("cat('Library "%.%l%.%" ',ifelse('"%.%l%.%"'%in%names(utils::sessionInfo()$otherPkgs)",
         ",'successfully','could not be'),' loaded.\\n', sep='')", file=Rprofile_file, append=TRUE)
    catn("################################################\n", file=Rprofile_file, append=TRUE)
         
  }
  
  catn("You must reload the R session for any changes to take effect.")
  return(invisible(NULL))
}

#' Directory of the current script
#'
#' \code{script_dir} returns the directory of the script file from 
#' which it is called (via \code{source}).
#'
#' @export
script_dir = function() {
  if(all(names(sys.frame(1))!="ofile")) "." else dirname(sys.frame(1)$ofile)
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
  if(missing(dir) || is.null(dir)) dir = script_dir()
  if(nchar(dir2)>0) dir = dir %.% '/' %.% dir2
  if(!dir.exists(dir)) {
    if(create) {
      if(ask) wait("Directory '",dir,"' does not exist and it will be created ...")
      dir.create(dir)
    } else {
      error("Cannot change working directory to '",dir,"' since it does not exist.",
            " Set create to TRUE for the missing directories to be created.")
    }
  }
  setwd(dir)
}

################################################################################

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
#' @name is_sourced
#' @export
source_depth = function() {
  check_for = list("source" = c("ofile", "keep.source", "deparseCtrl", "echo", "prompt.echo", "spaced"),
                   "sys.source" = c("i", "exprs", "oop", "file", "envir", "chdir", "keep.source", "toplevel.env"))
  is_source = function(i) all(check_for$"source" %in% names(sys.frame(i)))
  is_sys_source = function(i) all(check_for$"sys.source" %in% names(sys.frame(i)))
  which_sourced = sapply(0:sys.nframe(), function(i) is_source(i) | is_sys_source(i))
  sum(which_sourced)
}

################################################################################

#' An alias for the quit function
#'
#' This does essentially the same as \code{base::q} except it quits without saving 
#' or asking to save the current workspace.
#' @export
.q = function(save="no") 
  base::q(save=save)

################################################################################

#' Not-in-set
#'
#' Logical negation of %in%, i.e. returns a logical vector TRUE for elements of 
#' first argument that are not in the second argument and vice-versa
#' @export
`%notin%` = `%nin%` = 
  Negate('%in%')

######################################################################################

#' Concatenation operators
#'
#' Concatenation operators, which are aliases for the pasting functions. 
#' \code{grapes.grapes} is an operator versions of \code{base::paste0}, 
#' \code{grapes..grapes} is an operator versions of \code{base::paste} 
#' and \code{grapes_grapes} is an operator versions of \code{base::paste}, 
#' which takes symbols as arguments (see the examples below).
#'
#' @name concatenate
#' @export
#' @examples
#' "multi" %.% "tasking"
#' "multi" %..% "tasking"
#' multi %_% tasking
`%.%` = function(...) {
  do.call("paste0", list(...))
}

#' @name concatenate
#' @export
`%..%` = function(...) 
  do.call("paste", list(...))

#' @name concatenate
#' @export
`%_%` = function(..., sep="_") {
  strings = as.list(as.character(match.call(expand.dots = FALSE)$`...`))
  do.call("paste", append(strings, list(sep=sep)))
}
  
######################################################################################

#' Regular expression match operator
#'
#' An operator version of \code{base::regexpr}. Uses the left-hand side argument as 
#' the \code{pattern} and the right-hand side argument as the \code{text} arguments
#' of a call to \code{regexpr}. The call to \code{regexpr} is vectorized, which means
#' that the operator also takes vector arguments (on either side).
#'
#' @export
#' @examples
#' "ay" %match% "daylight"
#' "ai" %match% "daylight"
`%match%` = function(pattern, x) {
  print(pattern)
  Vectorize(regexpr)(pattern, x) > 0
}

######################################################################################

#' Default value for NULL
#'
#' This is just an alias for the equivalent "\code{grapes||grapes}" operator in the 
#' package \code{rlang}.
#' @export
#' @examples
#' 1 %|||% 2
#' NULL %|||% 2
`%|||%` = function (x, y) {
  if(is_null(x)) y else x
}

######################################################################################

#' Non-NA value check
#' 
#' Check whether a row of \code{x} contains at least one non-NA value. Works for vectors
#' and 2-arrays.
#'
#' @export
#' @examples
#' x = rbind(c(1,NA),c(NA,NA)); anyNonNA(x)
anyNonNA = function(x, rowwise=TRUE) {
  if(rowwise) {
    stopifnot(is.null(dim(x)) || length(dim(x))<=2)
    if(NCOL(x)<=1) !is.na(t(t(x))[,1]) else !apply(is.na(x),1,all)
  } else {
    Recall(t(x), rowwise=TRUE)
  }
}

######################################################################################

#' Get the utilbox namespace/environment
#'
#' Creates the working namespace called '.utilbox', which is used by some of the 
#' functions of the utilbox package (e.g. \code{cat0}).
#'
#' @export
utilbox_namespace = function(envir=.GlobalEnv) {
  if(!exists(".utilbox", mode="environment", envir=envir)) 
    assign(".utilbox", new.env(), envir=envir)
  get(".utilbox", envir=envir)
}


######################################################################################

#' Version of cat that automatically flushes the console if \code{flush} is \code{TRUE}.
#' This only affects buffered output enabled R sessions (e.g. Rgui on Windows)
#' @export
cat0 = function(..., sep="", flush=TRUE, flush_cycle=1, envir=utilbox_namespace()) {
  
  ## Print the text
  base::cat(..., sep=sep)
  
  ## Define a counter of prints since the last flush
  if(!exists(".flush_counter", envir=envir)) 
    assign(".flush_counter", 0, envir=envir)
  
  ## Increase the flush counter by 1
  assign(".flush_counter", get(".flush_counter", envir=envir) + 1, envir=envir)
  
  ## If either flush is TRUE or the counter has reached the limit, flush the console
  ## and reset the counter to zero
  if(flush || get(".flush_counter", envir=envir)>=flush_cycle) {
    utils::flush.console()
    assign(".flush_counter", 0, envir=envir)
  }
  
}

################################################################################

#' Arithmetric mean 
#'
#' A version of \code{base::mean} which removes \code{NA} values by default.
#' @export
#' @examples
#' x = c(1,NA); mean(x); mean2(x)
mean2 = function(..., na.rm=TRUE) {
  base::mean(..., na.rm=na.rm)
}

################################################################################

#' Version of \code{cat0} which prints the end-of-line symbol by default
#' @export
catn = function(..., newline=TRUE) 
  cat0(...,if(newline) "\n")

################################################################################

#' Alias for \code{flush.console()}
#' @export
.fc = utils::flush.console

################################################################################

#' An "empty" function which returns zero.
#' @export
void = function(x) 
  return(invisible(0))

################################################################################

#' Remove all objects
#'
#' Shorthand for removal of all objects (including hidden ones whose names start
#' with a dot) from the environment in \code{envir} (equals \code{.GlobalEnv} by 
#' default). This function is basically an alias for \code{rm(list=ls(envir=envir))}. 
#' Optionally, names of objects that should not be removed can be supplied via \code{keep}.
#'
#' @export
.rma = function(all.names=FALSE, keep=".utilbox", envir=.GlobalEnv)
  rm(list=setdiff(ls(envir=envir, all.names=all.names), keep), envir=envir)

################################################################################

#' Delete objects
#'
#' Deletes all supplied objects in the environment in \code{envir}. Unlike the 
#' basic \code{base::rm} function, the current function checks if the supplied 
#' objects exist in order to avoid errors. Objects can be supplied as character 
#' strings or as symbols.
#'
#' @export
rme = function(..., envir=parent.frame()) {
  dots = match.call(expand.dots = FALSE)$`...`
  suppressWarnings(do.call(rm, append(dots, list(envir=envir))))
  #suppressWarnings(rm(..., envir=envir))
}

#rme = function(..., envir=parent.frame()) {
#  dots = match.call(expand.dots = FALSE)$`...`
#  objs = as.character(dots)
#  invisible(sapply(objs, .rme, envir=envir))
#}

.rme = function(y, envir=parent.frame(), treat_as_symbol=FALSE) {
  nam = if(treat_as_symbol) as.character(substitute(y)) else y
  if(exists(nam, envir=envir)) rm(list=nam, envir=envir)
  #if(length(ls(envir=envir, pattern=paste0("^",y,"$")))>0) rm(list=y, envir=envir)
}

################################################################################

#' Argument hijack function
#'
#' A function that allows for changing of default values for arguments of other 
#' functions
#' @export
#' @examples
#' cat = hijack(cat, sep='')        # equivalent to cat0
#' mean = hijack(mean, na.rm=TRUE)  # equivalent to mean2
hijack = function (FUN, ...) {
  .FUN = FUN
  args = list(...)
  lapply(seq_along(args), function(i) formals(.FUN)[[names(args)[i]]] <<- args[[i]])
  .FUN
}

################################################################################

#' Same object check
#'
#' Checks whether all arguments correspond to the same object in memory. Arguments
#' can be given quoted or unquoted and the unquoted with the same result.
#'
#' @export
#' @examples
#' a = 1; b = a; c = 2
#' is_same_object(a, b)     # returns TRUE
#' is_same_object(a, b, c)  # returns FALSE
#' is_same_object(a, "a")   # returns TRUE
is_same_object = function(..., envir = parent.frame()) {
  args = as.character(match.call(expand.dots = FALSE)$`...`)
  mems = sapply(args, function(arg) tracemem(get(arg, envir=envir)))
  length(unique(mems)) == 1
}

################################################################################

#' Assign function
#'
#' (Not exported) A function that changes name of a variable by reassigning 
#' \code{from} to \code{to} and removing the object \code{from}. Effectively,
#' it is a rename function.
#' 
.var_rename_right = function(from, to, envir = parent.frame()) {
  from = as.character(substitute(from))
  to = as.character(substitute(to))
  if(identical(from, to)) return(invisible(-1))
  assign(to, get(from, envir=envir), envir=envir)
  rm(list=from, envir=envir)
  return(invisible(0))
}

#' Assign function
#'
#' Create an alias for \code{.var_rename_left}, which takes input in the reverse order.
.var_rename_left = function(to, from, envir = parent.frame()) { }
body(.var_rename_left) = body(.var_rename_right)

#' Operator that change name of a variable by reassigning the value from 
#' its left-hand side (for \code{grapes->grapes}) or right-hand side (for 
#' \code{grapes->grapes}) and assigning to the variable on the other side.
#' Effectively, rename functions.
#' @name var_rename
#' @export
#' @examples
#' a = 100; a %->% b; b; exists('a')  # after the call b is 100 and a does not exist
#' a = 100; b %<-% b; b; exists('a')  # same as above, except opposite direction
"%->%" = .var_rename_right

#' @name var_rename
#' @export
"%<-%" = .var_rename_left

################################################################################

#' Pause
#'
#' Pauses the execution until the user presses ENTER (for continue) or ESC (for quit)
#' @export
wait = function(...) {
  do.call("cat", list(...))
  if(!interactive()) {
    note("Function wait() works only in an interactive mode of R. Continuing execution ...")
  } else {
    cat("\nPress ENTER to continue or ESC to quit ...", fill = TRUE)
    input = scan("", what = "character", nmax=1, quiet=TRUE)
  }
}

################################################################################

#' Toggle stopping on warning
#'
#' Sets options()$warn to either 0 or 2, which makes R either stop when a warning
#' is issued (>=2) or not (0). If argument 'turn_on' is missing, the function acts
#' as a toggle turning stopping on warnings on and off on alternative calls.
#' @export
.sow = function(turn_on, announce=TRUE) {
  if(missing(turn_on)) turn_on = options()$warn<2
  options(warn=ifelse(turn_on, 2, 0))
  status = ifelse(turn_on,"ENABLED","DISABLED")
  if(announce) note("Stopping on warnings has been ",status,".")
  status
}

################################################################################

#' Toggle recovery on error
#'
#' Toggle the recovering on error option (same logic as \code{.sow()})
#' @export
.roe = function(turn_on, announce=TRUE) {
  if(missing(turn_on)) turn_on = is.null(options()$error)
  options(error=if(turn_on) recover else NULL)
  status = ifelse(turn_on,"ENABLED","DISABLED")
  if(announce) note("Recovery on error has been ",status,".")
  status
}


################################################################################

#' Pause execution
#'
#' Pauses execution for \code{time} seconds with announcement.
#' @export
sleep = function(time) {
  catn("Sleeping for ",time," seconds ...")
  Sys.sleep(time)
  return(invisible(NULL))
}

################################################################################

#' Stop execution
#'
#' A different way of stopping the execution. Intended to be called by halt but the behavior
#' wasn't as hoped/expected. Abandoned for now. 
#' @export
.halt = function() {
  error = simpleError("")
  class(error) = c("myerror", class(error))
  signalCondition(error)
}

################################################################################

## Prints an error message and stops differently for interactive/non-interactive modes
halt = function(error="") {
  if(interactive()) {
    if(nchar(error)) cat("\n")
    error = paste0(error,"\nExecution halted.\n")
    #tryCatch(.halt(), myerror=function(...) cat(error))
    stop(error, call.=FALSE)
  } else {
    if(nchar(error)) cat(error)
    cat("\nExecution halted.\n")
    utils::flush.console()
    q("no", status=1, runLast=FALSE)
  }
}

################################################################################

#' Throw an error and stop
#'
#' A more elaborate version of stop(). It can do beeping and if inside an interactive
#' session it doesn't stop execution but enters a browsing mode
#' @export
error = function(t, ..., sep="", quit=TRUE, Q, browser=interactive(), nskip1=0) {
  
  if(pckg_is_installed("beepr")) try(beepr::beep(1))
  
  if(!missing(Q) && missing(quit)) quit = Q
  if(!missing(browser) && isTRUE(browser)) quit = FALSE

  cat(rep("\n", max(0,nskip1)))
  error = paste(t, ..., sep=sep)
  if(quit) {
    if(!interactive()) error = paste0("\nERROR: ",error,"\n")
    halt(error) 
  } else {
    cat0("\nERROR: ",error,"\n\n")
    utils::flush.console()
    do.call("browser", list(), envir=sys.frame(sys.parent()))
  }
  
}

################################################################################

#' List all functions exported by a package
#'
#' Possible alternatives in the examples
#'
#' @examples
#' list_exported("dplyr")
#'
#' lsf.str("package:dplyr")
#' ls("package:dplyr")  # when the package is loaded
#' help(package = dplyr)
#' # see https://stackoverflow.com/questions/30392542/is-there-a-command-in-r-to-view-all-the-functions-present-in-a-package
#' @export
list_exported = function(pckg, character.only=FALSE) {
  if(!character.only) pckg = as.character(substitute(pckg))
  getNamespaceExports(pckg)
}


################################################################################

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
#' \code{list_installed_packages} lists all installed packages.
#'
#' \code{list_loaded_packages}) list all loaded packages.
#'
#' (\code{pckg_is_installed}) checks if a package is installed.
#'
#' @name llibrary
#' @export
#' @examples
#' list_installed_packages()
#' list_loaded_packages()
#' pckg_is_installed('utilbox')
llibrary = function(pckgs=NULL, quietly=TRUE, character.only=FALSE, fail=warn, 
  url_CRAN="https://cloud.r-project.org/") {

  ## If symbol names expected, make them into strings
  if(!character.only) pckgs = as.character(substitute(pckgs))
    
  ## If no packages supplied, silently return
  if(length(pckgs)==0) return()
   
  ## Get the currently loaded libraries
  loaded_packages = list_loaded_packages()
  
  ## Make sure pckgs is a list (use CRAN as default source)
  if(is.vector(pckgs)) 
    pckgs = lapply(pckgs, function(x) list(name=x, src="CRAN"))

  ## Load necessary packages
  for(lib in pckgs) {
  
    # Check if package already loaded
    if(any(lib$name==loaded_packages)) next
    
    # Announce loading of the current package
    if(!quietly) cat("Loading package ",lib$name," ...\n", sep=""); .fc()

    # Check if current package is installed
    if(all(lib$name!=installed.packages()[,"Package"])) {
    
      cat("Library '",lib$name,"' seems to be missing, trying to install it from '",lib$src,"' ...\n", sep="")
      if(!is.null(lib$note)) cat(lib$note,"\n",sep="")

      # Check for location information
      if(is.null(lib$src) || is.na(lib$src)) {
        fail("Library ",lib$name," cannot be installed due to missing",
             " source information! You must install it manually.")
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
    if(!quietly) cat("Loading package '",lib$name,"' ...\n", sep=""); .fc()
    require(lib$name, character.only=TRUE, quietly=quietly)
  
  } # for(lib in pckgs)

} # llibrary

#' @name llibrary
#' @export
load_libraries = llibrary

#' @name llibrary
#' @export
rrequire = function(...) {
  x = try(require(...))
  if(x) return(x) else llibrary(...)
}

#' @name llibrary
#' @export
llib = function(...) {
  pckgs = as.character(as.list(match.call())[-1])
  llibrary(pckgs, character.only=TRUE)
}

#' List packages
#' @export
list_installed_packages = function() {
  x = try(installed.packages()[,"Package"])
  if(class(x)=="try-error") "" else x
}
  
#' @name llibrary
#' @export
list_loaded_packages = function()
  c(sessionInfo()$basePkgs, names(sessionInfo()$otherPkgs))

#' @name llibrary
#' @export
pckg_is_installed = function(pckgs) 
  structure(sapply(pckgs, requireNamespace, quietly=TRUE), names=pckgs)
  
#' @name llibrary
#' @export
unload_library = function(pckgs=NULL, character.only=FALSE) {
  if(!character.only) pckgs = as.character(substitute(pckgs))
  for(pckg in pckgs) detach(paste0("package:",pckg), character.only=TRUE, unload=TRUE)
  return(invisible(TRUE))
  
}

################################################################################

#' Public IP address
#'
#' Gets ones public IP address using the ipify.com API. The default server can be changed.
#'
#' The function probably needs a rewrite for when the call to the server fails for some reason.
#'
#' @export
get_my_ip = function(server = "https://api.ipify.org", trace=1) {
  if(trace>0) catn("Checking your IP address via ",server," ...")
  ip = scan(con <- url(server), what = 'character', quiet = TRUE)
  if(exists("con", envir=environment())) close(con)
  if(trace>0) catn("Your IP address is ",ip,".")
  invisible(ip)
}

################################################################################

#' Get function name
#'
#' Returns the name of the parent function, i.e. that from whose body it is invoked.
#'
#' @export
#' @examples
#' lll = function() print(this_fun_name()); lll() # should return 'lll'
this_fun_name = function() {
  as.character(head(sys.call(which=sys.parent()),1)) 
}

################################################################################

#' Get the source code of a function
#'
#' Returns as string the body a function object supplied argument
#'
#' @name fun_source
#'
#' @param fun function for which the body of the code is desired
#'
#' @details
#' This function takes an R function on input and returns its source code as character vector.
#'
#' @return A character vector with each command of the input function as an element.
#' @export
#'
#' @examples
#' f <- function(x) { 
#'   print(x)
#'   x = 2
#'   return(invisible(x)) 
#' }
#' ff <- fun_source(f)
#' print(ff)
fun_source = function(fun, file=NULL) {

  stopifnot(is.function(fun))
  
  b = c(deparse(args(fun))[deparse(args(fun))!="NULL"], deparse(body(fun)))
  
  if(!missing(file) && !is.null(file)) {
    cat(paste(b, collapse="\n"), file = file)
    return(invisible(b))
  } else   
    return(b)
  
}

################################################################################

#' Get trailing arguments
#'
#' Returns the trailing arguments of the call that invoked the current R session.
#' Useful when calling Rscript on a script and modifying its behavior via flags
#' @export
system_call_args = function() {
  args = commandArgs(trailingOnly=TRUE)
  args = unname(sapply(args, function(x) gsub("[\r\n]","",x)))
  args[nchar(args)>0]
}

################################################################################

#' R session info
#'
#' Prints information about the current R session including the version of R
#' and the trailing arguments of the system call that started this session.
#'
#' @export
Session_info = function() {
  catn("R version information:")
  print(R.version)
  catn("\nProgram call: '",paste(commandArgs(trailingOnly=FALSE), collapse=" "),"'")
  catn("Working path: '",getwd(),"'")
}

################################################################################

#' Add path to the package path
#'
#' Adds a supplied path to the list of paths for searching packages
#'
#' @name set_pkglib
#'
#' @param libpath path to be added
#'
#' @details
#' Adds a supplied path to the list of paths for searching packages. This is useful
#' for instance when the default path is in a location which is write-protected and
#' packages need to be installed into an alternative location.
#'
#' @return A character vector with each command of the input function as an element.
#' @export
set_pkglib = function(libpath) {
  if(is.null(libpath)) return(invisible(0))
  if(.libPaths()[1]!=libpath) return(invisible(1))
  cat("Setting primary local R library to '",libpath,"' ...\n", sep="")
  dir_exist_check(libpath)
  .libPaths(libpath)
  libpath %in% .libPaths()
}

################################################################################

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
#' @export
is_win = function() {
  .Platform$OS.type=="windows"
}

#' @name is_win
#' @export
is_linux = function() {
  any(.Platform$OS.type==c("linux","unix"))
}

#' @name is_win
#' @export
is_term = function() {
  .Platform$GUI=="RTerm"
}

#' @name is_win
#' @export
is_rgui = function() {
  .Platform$GUI=="Rgui"
}

#' @name is_win
#' @export
is_rstudio = function() {
  .Platform$GUI=="RStudio"
}

################################################################################

#' Add to the system path
#'
#' Adds the supplied path among the system search paths.
#' @export
append_path = function(path) {
  PATH = Sys.getenv("PATH")
  if(regexpr(patternize(path), PATH)<=0) 
    Sys.setenv(PATH=paste0(PATH,";",path))
}

################################################################################

#' Adjust R GUI 
#'
#' Change the way the GUI looks (e.g. font size, font style)
#'
#' @export
Rgui_adjust = function(cmd = c("points = 7", "rows = 43", "columns = 177")) {
  if(is_rgui()) {
    temp = tempfile()
    catn(paste(cmd, collapse="\n"), file=temp)
    loadRconsole(file = temp)
    invisible(TRUE)
  } else invisible(FALSE)
}

#' @export
Rgui_reset = function() {
  loadRconsole(file = file.path(R.home(), "etc/Rconsole"))
  invisible(TRUE)
}

################################################################################

#' File lock check
#'
#' Check if a given file is locked by another application (e.g. by Excel).
#' Currently relies on a call to \code{wmic} and works on Windows only.
#'
#' @export
check_file_locked = function(file) {
  if(missing(file)) error("Supply file name.")
  if(is_win()) {
    call = "wmic process get commandline"
    x = try(system(call, intern=TRUE, show.output.on.console=FALSE))
    if(class(x)=="try-error") return(-1)
    as.numeric(any(regexpr(patternize(file), x)>0))
  } else return(-1)
}

################################################################################
################################################################################
################################################################################
################################################################################

## A version of 'modifyList' from utils which drops zero-length elements in 'val'
## before updating 'x' (optionally can behave the same as modifyList)
#' @export
modifyList2 = function(x, val, ..., drop_null_val=TRUE)
  utils::modifyList(x, if(drop_null_val) Filter(length, val) else val)

################################################################################

## Creates a named list: The code list(a = a, b = b) becomes nlist(a,b) and 
## list(a = a, b = 2) becomes nlist(a, b = 2), etc. (This is lifted from the package 
## 'loo' so that it doesn't have to be installed just for this one function
#' @export
nlist = function (...) {
    m = match.call()
    out = list(...)
    no_names = is.null(names(out))
    has_name = if(no_names) FALSE else nzchar(names(out))
    if(all(has_name)) return(out)
    nms = as.character(m)[-1L]
    if (no_names) {
      names(out) = nms
    } else {
      names(out)[!has_name] = nms[!has_name]
    }
    return(out)
}

################################################################################

#' Filter an object based on the names of its elements
#'
#' Returns the filtering of \code{x} based on the names of \code{x} matching 
#' the pattern given in \code{pattern}.
#' @export
filter_by_name = function(x, pattern) 
  x[regexpr(pattern, names(x))>0]

################################################################################

#' Get the last element
#'
#' Returns the last element in an vector.
#' @export
last_element = function(x) {
  x[length(x)]
}

################################################################################

#' Recursive merge
#'
#' This does a recursive merge. Taken from package reshape because it has a bug
#' which is fixed here.
#' 
#' The bug was the missing ellipsis in the call to \code{Recall()}.
#' @export
merge_recurse = function(dfs, ...) {
  if(length(dfs) == 1) {
    dfs
  } else if(length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all = TRUE, sort = FALSE, ...)
  } else {
    merge(dfs[[1]], Recall(dfs[-1], ...), all = TRUE, sort = FALSE, ...)
  }
}

################################################################################

#' Removes zero-length elements from a list
#' @export
#' @examples
#' list_clean(list(1:10, NULL, 'a'))
list_clean = function(L, null.rm=TRUE) {
  Filter(length, L)
}
  
################################################################################

#' List files matching regular pattern
#'
#' A wrapper for list.files that allows multiple patterns at once and 
#' files that match at least on of the patterns are returned. Optionally,
#' files can be sorted according to attribute in \code{attrib}. 
#'
#' Wrappers \code{list_files_latest} and \code{list_files_biggest} implement the
#' sorting according to attributes "modification time" and "size", respectively.
#' @export
list_files = function(pattern=NULL, path=".", full.names=FALSE, ..., sort=FALSE, 
  attrib=c("mtime","size","isdir", "mode","ctime","atime","exe"), decreasing = FALSE, 
  append_path=FALSE) {
  
  attrib = match.arg(attrib)
  fs = if(!length(pattern)) {
    list.files(path=path, ..., full.names=full.names)
  } else {
    unique(unlist(sapply(pattern, function(p) list.files(pattern=p, path=path, full.names=full.names, ...))))
  }
  
  if(sort) {
    fs1 = if(full.names) fs else paste0(path,"/",fs)
    info = unlist(file.info(fs1)[attrib])
    fs = fs[order(info, decreasing=decreasing)]
  }
  
  if(!full.names && append_path) paste0(path,"/",fs) else fs
  
}

#' @name list_files
#' @export
list_files_latest = function(...)
  list_files(..., sort=TRUE, attrib='mtime', decreasing=TRUE)

#' @name list_files
#' @export
list_files_biggest = function(...)
  list_files(..., sort=TRUE, attrib='size', decreasing=TRUE)

######################################################################################

#' Patternize a string
#'
#' \code{patternize} wraps special characters in string name (possibly a vector) by 
#' brackets so that it can be matched within regular expression matching (the case of 
#' "\\" has to be treated differently). Useful for instance when working with file names.
#'
#' \code{unpatternize} does the reverse of \code{patternize()}.
#'
#' @name patternize
#' @export
#' @examples
#' regexpr('notes.txt', 'notes_txt')>0                # TRUE
#' regexpr(patternize('notes.txt'), 'notes_txt')>0    # FALSE
#' regexpr(patternize('notes.txt'), 'notes.txt')>0    # TRUE
#' 
#' 'notes.txt')
patternize = function(name, special=c("+",".","(",")","$","?","\\")) {
  for(i in seq_along(name))
    for(s in special) 
      name[i] = gsub(paste0("[",s,"]"), 
                     paste0("[", paste(rep(s,1+I(s=="\\")), collapse=""),"]"), 
                     name[i])
  return(name)
}

################################################################################

#' @name patternize
#' @export
unpatternize = function(pattern)
  gsub("[][]","",pattern)

################################################################################

#' String trimming
#'
#' Removes trailing spaces from the beginning and end of a string.
#' @export
str_trim = function(x) 
  gsub("^\\s+|\\s+$", "", x)

################################################################################

#' Last characters in a string
#'
#' Gets the last \code{n} characters from a string. Vectorized.
#' @export
str_last = function(x, n=1) {
  substr(x, nchar(x)-n+1, nchar(x))
}

################################################################################

#' Lists files within a zip archive that match a regular expression pattern
#'
#' Lists files within a zip archive that match the supplied regular expression
#' pattern or patterns.
#' @export
list_zip = function(zipfiles, pattern=".*", mask_exclude=FALSE) {

  # List all files
  files = sapply(zipfiles, function(z) unzip(z, list=TRUE)$Name)
  is_list = is.list(files)

  # Keep only files that match at least one of the patterns in 'pattern' or
  # those that do not match either of the patterns when mask_exclude is TRUE
  pattern = paste(pattern, collapse="|")
  flip = -(2*mask_exclude-1)
  files = lapply(files, function(f) f[flip * regexpr(pattern, f) > 0])

  # Drop (potential) NULL elements
  files = list_clean(files)
  
  # If input was not a list, return unlisted
  if(!is_list) files = unlist(files)

  return(files)
  
}

################################################################################

#' Generates a random file name
#'
#' Generates a random file name of length \code{nchar} by drawing from the set
#' defined via \code{chars}.
#' @export
random_filename = function(path=".", nchar=3, chars=c(letters, LETTERS, 0:9, "_-%#@")) {
  file = "."
  while(file.exists(file))
    file = paste(sample(chars, nchar, replace=TRUE), collapse="")
  return(file)
}

################################################################################

#' Split file names
#'
#' Separates the path portions from file names. Returns the paths and the proper
#' file names in a list.
#'
#' @export
#' @examples
#' separate_path("c:/Windows/system.dat")
#' separate_path("system.dat")
separate_path = function(files, path0="./") {
  
  files = sub("/+$","",gsub("\\\\","/",files))
  paths = rep(path0, length(files))
  w = regexpr("/",files)>0
  
  if(any(w)) {
    x = strsplit(files[w],"/")
    paths[w] = paste0(sapply(x, function(x1) paste0(x1[-length(x1)], collapse="/")), "/")
    files[w] = sapply(x, tail, 1)
  }
  
  list(path=paths, filename=files)

}
  
################################################################################

#' Compress files
#'
#' \code{zipupf} compresses files each into its own archive. Optionally adding time 
#' stamps to the output files.
#'
#' \code{zip_files} compresses files into a single archive supplied via \code{zipfilename}.
#'
#' \code{zipup} compresses files matching the pattern in \code{mask} (and not matching 
#' the pattern in \code{mask_exclude}) each into its own archive.
#'
#' \code{zip_all_in_path} compresses all files in the supplied path (\code{path}) 
#' each into its own archive
#'
#' \code{un_zip} decompresses from \code{zfile} all files that match the pattern 
#' in \code{mask}, or those that do not match it if \code{mask_exclude} is \code{FALSE}.

#' @name zipup
#' @export
zipupf = function(files=".*", extras="-m", appendix=".zip", add_timestamp=FALSE, check_status=TRUE) {

  if(length(files)==0) return(-1)
  
  odir = getwd()
  on.exit(setwd(odir))
  
  fs = separate_paths(files, path0="./")
  
  ts = if(add_timestamp) paste0("_",file_timestamp(files)) else rep("", length(files))
  ofiles = paste0(fs$files,ts,appendix)
  
  ios = NULL
  for(i in 1:length(files)) {
  
    setwd(fs$paths[i])
    ios[i] = zip(ofiles[i], fs$files[i], extras=extras)
    setwd(odir)
    if(check_status && ios[i]!=0) warn("Problem zipping up file '",files[i],"' (status ",ios[i],").")
    
  }
  
  return(list(ofile=paste0(fs$paths,ofiles), ios=ios))
  
}

#' @name zipup
#' @export
zip_files = function(files, path=".", zipfilename, extras="-m") {

  if(missing(zipfilename)) zipfilename = paste0(random_filename(),".zip")
  odir = getwd()
  on.exit(setwd(odir))
  setwd(path)
  ios = zip(zipfilename, files, extras=extras)
  setwd(odir)
  
  if(ios==127)
    note("zip archiver appears to be missing (zip returned code 127).", 
         if(is_win()) " (Hint: Install Rtools)")
  
  if(ios==12)
    note("Zip reported an error 'name not matched' (code ",ios,", probably due to",
         " very long file names), but I'll continue anyway ...")
         
  return(ios)
      
}

#' @name zipup
#' @export
zipup = function(mask=".*", mask_exclude, outfile, path=".", appendix=".zip", extras="-m", 
  do_patternize=TRUE, chunk=Inf, announce=FALSE, single_archive=TRUE, continue_on_error=TRUE,
  retry=TRUE) {

  odir = getwd()
  on.exit(setwd(odir))
  
  # List the files matching mask
  if(do_patternize) mask = patternize(mask)
  files = list.files(path=path, pattern=mask)
  
  # If not files found, just return
  if(length(files)==0) return(list(ios=-1, zipfile=NULL))
  
  # If multiple files found, throw an error. Otherwise base the output file name on the single file found.
  if(missing(outfile) && single_archive) {
    if(length(files)>1) 
      error("Archive name must be supplied when multiple files match the mask and 'single_archive' is true (zipup).")
    outfile = paste0(files, appendix)
  }

  # Exclude those matching mask_exclude
  if(!missing(mask_exclude)) {
    if(do_patternize) mask_exclude = patternize(mask_exclude)
    files = files[regexpr(mask_exclude, files)<0]
  }
  
  # Nothing to zip up
  if(length(files)==0) {
    if(announce) note("No files found using mask '",mask,"' (after possible exclusion via 'mask_exclude').")
    return(list(ios=1))
  } else {
    if(announce) cat0("Total of ",length(files)," found using mask '",mask,"'.\nZipping up ...\n")
  }
  
  ## Define the name based on mask
  #mask = unpatternize(mask)
  #zfile = paste0(mask, appendix)

  # Zip them up into a single archive (single_archive is true) or each individually (otherwise)
  ichunk = 0
  zf = NULL
  chunk = if(!single_archive) 1 else max(1, chunk)
  while(length(files)>0) {
    
    # Define a random (short) file name (if not given on input)
    if(is.null(zf) || !single_archive) zf = paste0(random_filename(),".zip")
    
    # If single archive should be produced, add the flag 'g' (from the 2nd file on)
    ichunk = ichunk + 1
    if(single_archive && ichunk==2) extras = paste0("-",sub("-","",extras),"g")
    
    # Zip the current file up. Possibly try a few times in case a file access error occurred
    iis = c(1,rep(2,5))[c(TRUE, rep(retry,5))]
    for(i in iis) {
      
      setwd(path)
      ios = zip(zf, head(files, chunk), extras=extras)
      setwd(odir)
      
      if(ios==127) {
        note("zip archiver appears to be missing (zip returned code 127).", 
             if(is_win()) " (Hint: Install Rtools)")
      }
      
      if(ios==12) {
        note("Zip reported an error 'name not matched' (code ",ios,", probably due to",
             " very long file names), but I'll continue anyway ...")
        break
      }

      if(i<max(iis) && ios==15) {
        Sys.sleep(i^2*0.05)
        next
      }
      
      if(ios!=0) {
        #error("Error occurred during zipping!", Q=(!interactive() && !continue_on_error))
        error("Problems during zipping (status code '",ios,"').", quit=FALSE)
        if(!continue_on_error) browser()
      }
      
    }

    # Rename the zip file to a proper name (unless a single archive is to be produced)
    if(!single_archive) {
      outfile = paste0(head(files, 1), appendix)
      file_rename(zf, outfile)
    }
    
    # Remove the current file from the list of files to zip up
    files = tail(files, -chunk)
    
  } # while(length(files)>0) 
  
  # Rename the zip archive to fit mask
	if(single_archive) file_rename(zf, outfile)
  
  return(list(ios=ios, zipfile=outfile))
  
}

#' @name zipup
#' @export
zip_all_in_path = function(path=".", check_status=FALSE) {

  odir = getwd()
  on.exit(setwd(odir))
  warn("This will zip up all files in the path '",path,"' relative to current",
       " working directory '",odir,"'. Be careful!")
  wait()
  
  ds = setdiff(list.dirs(), "..")
  for(d in ds) {
    setwd(d)
    fs = setdiff(setdiff(list.files(), list.dirs(full.names=FALSE)), list.files(pattern="[.]zip$"))
    for(f in fs) {
      ios = zip(paste0(f,".zip"), f, extras="-m")
      if(check_status && ios!=0) warn("Problem zipping up file '",f,"' (status ",ios,").")
    }
    setwd(odir)
  }
  
  catn("Finished.")
  
}

#' @name zipup
#' @export
un_zip = function(zfile, mask=".*", mask_exclude=FALSE, do_patternize=TRUE, remove=FALSE) {
  
  # List the files matching mask
  if(do_patternize) mask = patternize(mask)
  files = list_zip(zfile, pattern=mask, mask_exclude=mask_exclude)

  # Nothing to zip up
  if(!length(files)) return(1)

  # Unzip the files
  unzf = unzip(zfile, files)
  
  # Check success
  ok = all(sapply(patternize(files), function(p) any(regexpr(p, unzf)>0)))
  ios = if(ok) 0 else -1
  
  # Remove files on success
  if(ios==0 && remove) file.remove(files)
  
  return(list(ios=ios, files=unzf))
  
}

#############################################################

#' Download file
#'
#' A wrapper for \code{download.file()} that handles missing files
#' without stopping on error. It uses the input URL to define
#' the destination file.
#'
#' @export
#' @examples
#' download_file('http://www.karlin.mff.cuni.cz/~kpms/index.php')
download_file = function(url, destfile=separate_paths(url)$files, ...) {
  try(download.file(url, destfile, ...))
  invisible(destfile)
}

#############################################################

#' Splits string into a vector of individual characters
#' @export
str2vector = function(x, split="") 
  unlist(strsplit(x, split=split))

#############################################################

#' Reverse the order of characters in a string
#' @export
strrev = function(x) 
  sapply(x, function(y) paste(rev(str2vector(y)), collapse=""))

#############################################################

#' Find the first or last occurrence of a substring
#'
#' Find the first/last occurrence of a substring (given as regular pattern) 
#' in a string
#' 
#' @examples
#' strpos('hello world.', 'o', first=TRUE)
#' strpos('hello world.', 'o', last=TRUE)
#' strpos('hello world.', '[el]', first=TRUE)
#' strpos('hello world.', '[el]', last=TRUE)
#' strpos('hello world.', '.')
#' strpos('hello world.', '.', patternize=TRUE)
#'
#' @export
strpos = function(string, substring, first=TRUE, last=FALSE, patternize=FALSE) {
  if(missing(first)) first = !last
  if(missing(last)) last = !first
  if(patternize) substring = patternize(substring)
  all_pos = gregexpr(substring, string)
  sapply(all_pos, function(p) if(first) head(p,1) else tail(p,1))
}

################################################################################

#' Shift (rotate) the elements in a vector in 'x' by 'lag' spaces 
#' @export
shift = function(x, lag=1) {
  if(lag==length(x) || !length(x)) return(x)
  lag = lag %% length(x)
  if(lag==0) return(x)
  append(tail(x,lag), head(x, -lag))
}


################################################################################

#' Alias for head with a different default number of elements
#' @export
h1 = function(x, n=1, recycle=TRUE, ...) {
  default_class = length(intersect(class(x), c('data.frame','matrix','ftable','table','function')))==0
  if(default_class && recycle) head(rep(x, ceiling(n/length(x))), n=n, ...) else head(x, n=n, ...)
}
  
################################################################################

#' Last elements
#'
#' An analogue for \code{base::tail} with a different default number of elements.
#' @export
#' @examples
#' t1(1:10)               # compare with tail(1:10)
#' t1(as.list(LETTERS))   # compare with tail(as.list(LETTERS))
#' @export
t1 = function(...) {
  tail(..., n=1)
}

################################################################################

#' Find midpoints of a sequence
#'
#' Find the midpoints between individual elements of a sequence given in \code{x}.
#' @examples
#' midpoints(1:10)
#' midpoints(c(1:5,3*1:5))
#' @export
midpoints = function(x) {
  0.5*(head(x, -1) + tail(x, -1))
}

################################################################################

#' Insert an element in 'what' inside 'x' at the position 'after+1'
#' @export
insert = function(x, what, after, count=1) {
  if(missing(after)) after = length(x)
  after = max(0, head(after, 1))
  append(append(if(after==0) NULL else head(x, after), rep(what, count)), if(after==0) x else tail(x, -after))
}

#' Between with many intervals
#'
#' Checks whether x is in at least one of the supplied intervals
#' @export
#' @examples
#' or_between(runif(3), list(c(0,0.1), c(0.9,1)))
or_between <- function(x, intervals) {
  stopifnot(is.list(intervals))
  is_in = sapply(intervals, function(int) between(x, int[1], int[2]))
  apply(is_in, 1, any)
}

################################################################################

## Replaces dots in file names with the value in 'chr'. Useful for changing file
## names of plots that are to be included in a latex file where the dots cause trouble
#' @export
clean_filename = function(x, chr="-", keep.last.dot=TRUE) {
  p = if(keep.last.dot) strpos(x, ".", last=TRUE) else -1
  if(p<=0) {
    gsub("[.]",chr,x)
  } else {
    paste0(gsub("[.]",chr,substr(x,1,p-1)), substr(x,p,nchar(x)))
  }
}

################################################################################

#' Reads a text from a file as a single string
#' @export
read_char = function(file, encoding) { 
  nchars = if(!is.null(attr(file,'size'))) attr(file, 'size') else file.info(file)$size
  x = readChar(file, nchars)
  if(!missing(encoding)) Encoding(x) = encoding
  x
}
  
################################################################################

read_table_zip1 = function(file, zfile, zip_type, list_connections, fun_read, trace=0, ...) {
      
  # Open connection
  if(zip_type=="zip") {
  
    infile = unz(zfile, file)
    nam = paste0(zfile,":",file)
  
  } else if(zip_type=="gz") {
  
    infile = gzfile(zfile)
    nam = zfile
  
  } else if(zip_type=="plain") {
    
    warn("Unknown compression format of file ",zfile," (read_table_zip). Attempting to read as plain text ...")
    infile = file
    nam = paste0(zfile,":",file)
    
  } else error("Unknown compression format of file ",zfile," (read_table_zip)!")

  if(identical(fun_read, read_char)) 
    attr(infile, 'size') = attr(file, 'size')
  
  # Find the newest connection
  acon = setdiff(getAllConnections(),list_connections)

  # Read the file using function 'fun_read'
  if(trace>0) cat0("Reading file ",file," from inside ",zfile," ...\n")
  res = list(fun_read(infile, ...))
  names(res) = nam
  
  # Close the connection
  if(any(getAllConnections()==acon)) close(getConnection(acon))
  
  res
  
}
    
################################################################################

#' Read file inside a zip archive
#'
#' Reads a table from within a zip file (equivalent to read.table except the
#' input file is expected to be a zip archive)
#' @export
read_table_zip = function(zipfiles, files=NULL, pattern=NULL, nonames=FALSE, 
  maxnfiles=Inf, skipnfiles=0, trace=0, maxnchar=128, solve_long=TRUE, sort_name=TRUE,
  long_action=c("copy","rename"), attempt_read_plain=FALSE, fun_read=read.table, ...) {
  
  # Process what to do to avoid long file name problems
  long_action = match.arg(long_action)
  fun_action = if(long_action=="rename") file_rename else file.copy
  
  # Read the zip file(s)
  RES = NULL
  for(zfile in zipfiles) {
  
    # Extract the extension
    zip_type = sub(".*[.]","",zfile)
		
		if(all(zip_type!=c("zip","gz")) && attempt_read_plain) zip_type = "plain"
		
    # Get the set of active connections
    list_connections = getAllConnections()
    
    # If a list of files inside zipfiles is missing, list them all
    file_list = if(!is.null(files)) files else 
                if(zip_type=="zip") unzip(zfile, list=TRUE)$Name else 
                if(zip_type=="plain") zfile else "'*'"
    
    # Transmit the file size info as an attribute if reading is to
    # be done using function 'read_char'
    if(zip_type=="zip" && identical(fun_read, read_char)) {
      sizes = unzip(zfile, list=TRUE)$Length
      file_list = lapply(1:length(file_list), function(i) {
        f = file_list[i]; attr(f, 'size') = sizes[i]; f })
    }

    # Skip the desired number of files and limit the number of files to the given maximum
    if(skipnfiles>0) file_list = tail(file_list, -skipnfiles)
    if(maxnfiles>0 && maxnfiles<length(file_list)) file_list = head(file_list, maxnfiles)
    
    # Match the mask
    if(!is.null(pattern)) file_list = file_list[regexpr(pattern, file_list)>0]

    # Make sure the file names are not too long
    restore_file = FALSE
    name_len = nchar(zfile)
    if(name_len > maxnchar && any(zip_type==c("zip","gz"))) {

      warn("Zip file name is long (",name_len," characters), which might cause errors while unzipping",
           " (Use arguments 'solve_long' and 'long_action' to enable a solution).", skip1=1, skip2=0)
      
      if(solve_long) {
      
        if(trace>0) catn("Shortening the zip file name by ",long_action,"ing ...")
        fn = paste0(random_filename(),".zip")
        if(nchar(fn) > maxnchar) {
          root = sub("/.*","/",getwd())
          fn = paste0(root,fn)
        }
        
        cat(toupperfirst(ifelse(long_action=="rename", "renam", long_action)),"ing file '",
            zfile,"' to file '",fn,"' (in path '",getwd(),"') ...\n")
        
        fun_action(zfile, fn)
        zfile_orig = zfile
        zfile = fn
        restore_file = TRUE

        if(trace==0) cat("Reading ",length(file_list)," file(s) from archive '",zfile,"' ...\n")

      } # if(solve_long)
      
    }
    
    # Loop over file names in 'file'
    for(file in file_list) {
    
      res = read_table_zip1(file, zfile, zip_type, list_connections, fun_read, trace, ...)
      RES = append(RES, res)
      
    } # for(f in file)
        
    # Remove the extra copy
    if(restore_file) {
      cat("Restoring original file names (by ",ifelse(long_action=="rename","renaming","deletion"),") ...\n")
      ios = try(if(long_action=="rename") file_rename(zfile, zfile_orig) else file_remove(zfile), silent=TRUE)
    }
          
  } # for(zfile in zipfiles)
  
  # Sort the result list according to names
  if(sort_name && length(RES)>0) {
    ord = order(names(RES))
    RES = RES[ord]
  }
  
  # Assign names to the elements in the result list
  if(nonames) names(RES) = NULL
  
  return(RES)

} # read_table_zip

################################################################################

#' Logit, probit, inverse logit functions
#' @export
logit = function(p) 
  stats::qlogis(p)
  #log(p/(1-p))

#' @export
probit = function(p)
  stats::qnorm(p)
#probit = stats::qnorm

#' @export
ilogit = function(x)
  stats::plogis(x)
#ilogit = function(x) 1 / (1+exp(-x))
#invlogit = function(x) exp(x)/(1+exp(x))
#invlogit = function(x) exp(x)/(1+exp(-x))

################################################################################

## Returns an upper tail probability of the normal distribution 
#' @export
upnorm = function(..., lower.tail=FALSE) stats::pnorm(..., lower.tail=lower.tail)

################################################################################

#' Returns an upper tail quantile of the normal distribution 
#' @export
uqnorm = function(..., lower.tail=FALSE) stats::qnorm(..., lower.tail=lower.tail)

################################################################################

#' Returns the elements on the upper-triangular as a vector
#' @export
lower_tri = function(x, diag=TRUE) 
  x[lower.tri(x, diag=diag)]

################################################################################

#' Returns the elements on the lower-triangular as a vector
#' @export
upper_tri = function(x, diag=TRUE) 
  x[upper.tri(x, diag=diag)]

################################################################################

#' Prints a warning and either stops execution or waits depending on arguments
#' @export
msg = function(t, ..., lead="", sep="", quit=FALSE, wait=FALSE, skip1=0, skip2=0, flush=TRUE) {
  cat0(rep("\n", skip1), lead, flush=FALSE)
  cat0(t, ...,"\n", sep=sep, flush=FALSE)
  cat0(rep("\n", skip2), flush=flush)
  if(quit) halt() 
  if(wait) wait() 
}

################################################################################

#' Print a warning and either stop execution or wait depending on arguments
#' @export
warn = function(..., skip1=1, skip2=1) {
  msg(..., lead="WARNING: ", skip1=skip1, skip2=skip2)
}

################################################################################

#' Print a note and optionally wait
#' @export
note = function(...) {
  msg(..., lead="NOTE: ")
}
################################################################################

#' List all open graphical devices
#' @export
all_devs = .all_devs = function(silent = TRUE) {
  if(!silent) cat0("Found ",length(dev.list())," open devices.\n") 
  dev.list()
}

################################################################################

## Closes all open graphical devices (silently by default)
#' @export
.all_devs_off = function(silent=TRUE) {

  devs = all_devs(silent)
  
  if(is.null(devs)) return(invisible(0))

  if(!silent) cat0("Closing all open devices ...\n")
  
  for(dev in devs) dev.off(dev)

  if(!is.null(dev.list()))
    warn("Some devices could not be closed.")
  else if(!silent) 
    cat0("All devices closed.\n")
  
}

################################################################################

## Does the same as .all_devs_off but announces how many devices were closed
#' @export
.ado = function() 
  .all_devs_off(silent=FALSE)

################################################################################

# Capitalize first letters of each element in the vector 'string'
#' @export
#toupperfirst = function(string) 
toupperfirst = function(...) {
  string = do.call('paste0', list(...)) 
  #substring(string, 1, 1) = toupper(substring(string, 1, 1))
  #string
  paste0(toupper(substring(string, 1, 1)), substring(string, 2))
}
      
################################################################################

#' Cumulative paste0
#'
#' Takes a vector and cumulatively pastes it together
#' @export
#' @examples
#' cumpaste0(c(0,1,1,1,0,1,0,0))
cumpaste0 = function(x, .sep = "") 
  Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)

################################################################################

#' Take time in seconds and formats it to natural language
#' @export
format_time = function(tim, prec=3, drop_hour=FALSE) {
  th = tim %/% 3600
  tim = tim - th * 3600
  tm = tim %/% 60
  tim = tim - tm * 60
  ts = round(tim, prec)
  
  if(th==0 && drop_hour) th = NULL else if(th<10) th = paste0("0",th)
  if(is.null(th)) {
    if(tm==0) tm = NULL else if(tm<10) tm = paste0("0",tm)
  } else {
    if(tm<10) tm = paste0("0",tm)
  }
  if(!is.null(tm) && ts<10) ts = paste0("0",ts)
  
  paste0(paste0(c(th,tm,ts),collapse=":"),ifelse(is.null(tm)," seconds",""))
  
}

################################################################################

#' Clock control functions
#'
#' \code{clock()) gets the current system time and optionally prints it on screen
#' \code{start_clock()} starts the runtime clock
#' \code{stop_clock()} stops the runtime clock
#' \code{read_clock()} retrieves the runtime clock
#'
#' @export
clock = function(announce=TRUE, with_date=TRUE, lead="", digs=3, eol="\n") {
  odigs = options()$digits.secs
  options(digits.secs=digs)
  d1 = paste0(Sys.time())
  options(digits.secs=odigs)
  if(!with_date) d1 = sub(".*[ ]", "", d1)
  if(announce) cat0(lead, d1, eol)
  return(invisible(c("time"=d1)))
}

################################################################################

#' @name clock
#' @export
start_clock = function(print=TRUE, envir=parent.frame(), lead="", what="") {
  #d1 = Sys.time()
  d1 = clock(announce=print, lead=paste0(lead,toupperfirst(what,"started at ")))
  #if(print) cat(lead,toupperfirst(what,"started at ",paste0(d1),".\n"), sep="")
  assign("._start_time_variable", d1, envir=envir)
  assign("._last_check_time_variable", d1, envir=envir)  
  return(invisible(c("starttime"=d1)))
}

################################################################################

#' @name clock
#' @export
read_clock = function(envir=utilbox_namespace()) {

  #d2 = Sys.time()
  d2 = clock(FALSE)
  d1 = if(exists("._last_check_time_variable", envir=envir))
          get("._last_check_time_variable", envir=envir) else
       if(exists("._start_time_variable", envir=envir))
          get("._start_time_variable", envir=envir) else
       NULL

  # Calculate current runtime
  d12 = if(!is.null(d1)) difftime(d2, d1, unit="secs") else NA
  
  # Assign the current time as the last check time
  assign("._last_check_time_variable", d2, envir=envir)

  return(d12)
}

################################################################################

#' @name clock
#' @export
stop_clock = function(d1=NULL, d12=NULL, start_print=FALSE, end_print=TRUE, 
  runtime_print=TRUE, envir=utilbox_namespace(), lead="", what="") {
  
  # Read the current clock and recall start
  #d2 = Sys.time()
  d2 = clock(FALSE)
  if(is.null(d1) && exists("._start_time_variable", envir=envir))
    d1 = get("._start_time_variable", envir=envir)

  # Announce start and calculate runtime
  if(!is.null(d1)) {
    if(start_print) 
      cat0(lead,toupperfirst(what,"started at "),paste0(d1),".\n")
    if(is.null(d12)) 
      d12 = difftime(d2,d1,unit="secs")
  }
  
  # Announce end
  if(end_print) 
    cat0(lead,toupperfirst(what,"finished at "),paste0(d2),".\n")
  if(!is.null(d12) && runtime_print)
    cat0(lead,"Total ",tolower(what),"runtime of ",format_time(as.numeric(d12)),".\n")

  return(invisible(c("runtime"=d12)))

}

################################################################################

#' Get today's date
#'
#' Returns the today's date (in the current timezone) in the specified format.
#' @export
#' @examples
#' tday()     # current day
#' tday('%Y') # current year
tday = function(format="%Y-%m-%d") {
  format(Sys.time(), format)
}
  
#' Find the closest day among dates
#'
#' Takes a vector of dates in \code{dates} and a single date in \code{day}, which
#' are assumed to be in the 'YYYY-MM-DD' format, and find the closest value in
#' \code{dates} to the value in \code{day}. If \code{position} is \code{TRUE},
#' the index of the closest date is returned, otherwise the closest date itself
#' is returned (default).
#'
#' @export
#' @examples
#' closest_day(c('2010-01-01', '2010-03-15', '2012-06-20'), '2011-01-08')
closest_day = function(dates, day, position=FALSE) {
  w = which.min(abs(force_as_integer(dates)-force_as_integer(day)))
  if(position) w else dates[w]
}
  
################################################################################

#' Invert non-zero elements in an object
#' @export
Invert1 = function(x) {
  y = rep(0, length(x))
  y[nz <- x!=0] = 1. / x[nz]
  y
}

################################################################################

## Find pseudoinverse, square root, and sqrt-inverse matrices
#' @export
Invert = function(M, inv=FALSE, sq=FALSE, sqinv=FALSE, eps=1e-12, drop=FALSE) {
  INV = SQ = SQINV = NULL
  
  if(length(M)==1) {
    if(M==0) {
      if(sq) SQ = M
      if(inv) INV = M
      if(sqinv) SQINV = M
    } else {
      if(sq) SQ = sqrt(M)
      if(inv) INV = 1/M
      if(sqinv) SQINV = 1/sqrt(M)
    }
  
  } else {
    
    e = eigen(M)
    V = e$vectors
    d = e$values
    d[abs(d)<eps] = 0
    if(sq) SQ = V %*% diag(sqrt(d)) %*% t(V)
    d = 1/d
    d[is.infinite(d)] = 0
    if(inv) INV = V %*% diag(d) %*% t(V)
    if(sqinv) SQINV = V %*% diag(sqrt(d)) %*% t(V)
  
  }
  
  L = list_clean(list(INV=INV, SQ=SQ, SQINV=SQINV))
  if(length(L)==1 && drop) L = L[[1]]
  return(L)
  
}

################################################################################

## Find the intersection of multiple sets
#' @export
mintersect = function(...) {
  sets = list(...)
  set = sets[[1]]
  for(i in seq_along(sets)[-1]) set = intersect(set, sets[[i]])
  return(set)
}

################################################################################

#' Harmonic mean
#'
#' Calculates the harmonic mean (single value) of a single vector created as a
#' combination of all its input arguments.
#'
#' @export
hmean = function(...) {
  if(length(c(...))) 1/mean(1/c(...)) else c(...)
}

#' Geometric mean
#' @export
gmean = function(x, na.rm=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  #prod(x)^(1/length(x))
  exp(sum(log(x)/length(x)))
}

#' (Weighted) geometric mean
#' @export
gmean = function(x, w, ...) 
  exp(weighted.mean(log(x), w, ...))

#' (Weighted) logistic mean
#' @export
pmean <- function(x, w, ...) 
  plogis(weighted.mean(qlogis(x), w, ...))

#' Arithmetic mean
#'
#' Arithmetic mean of \code{x}. Same functionality as \code{base::mean} except with 
#' different output when input has zero length.
#' @export
amean = function(x, na.rm=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  if(length(x)) mean(x) else NULL
}

################################################################################

#' Mode of a sample
#'
#' Finds the mode, i.e. the value that occurs with highest frequency in \code{x}.
#' The function works for both numeric and character/factor data.
#' @export
Mode = function(x, all_modes=FALSE, exclude.na=TRUE) {
  ux = unique(x)
  if(exclude.na) ux = ux[!is.na(ux)]
  tab = tabulate(match(x, ux))
  if(all_modes) {
    ux[tab == max(tab)]
  } else {
    ux[which.max(tab)]
  }
}

################################################################################

#' Functions for memory unit conversion

#' Determines the appropriate unit of memory for the size of 'x'
#' @export
get_unit = function(x) {
  w = pmax(1, pmin(trunc(log10(x)/3)+1,5))
  unit = sapply(w, function(y) if(is.na(y) || is.nan(y)) NA else switch(y, "B", "kB", "MB", "GB", "TB"))
  return(unit)
}

#' Returns the size of the unit in 'unit'
#' @export
de_unit = function(unit)
  return(if(is.na(unit) || is.nan(unit)) NA else switch(unit, "B"=1, "kB"=1e3, "MB"=1e6, "GB"=1e9, "TB"=1e12))

#' Unit conversion
#'
#' Converts which is assumed to be in unitary units (i.e. bytes) to an appropriate 
#' other unit
#' @export
convert_unit = function(x, unit, append_unit=TRUE, ndigit=3) {
  if(missing(unit)) unit = get_unit(x)
  s = rsignif(x / sapply(unit, de_unit), ndigit)
  if(append_unit) s = paste0(as.character(s), unit)
  return(s)
}

################################################################################

#' Convert a number to an abbreviated notation
#' @export
short_notation = function(x, exact_cutoff=999, ndig=1, word_units=FALSE) {
  
  # Figure out which to convert
  x = as.numeric(x)
  xx = x
  xx[is.na(x)] = as.character(NA)
  stay = x<=exact_cutoff
  xx[stay] = as.character(x[stay])
  wx = !is.na(x) & !stay

  # Figure out the units
  w = trunc(log10(x[wx])/3)
  w1 = pmin(w+1,5)
  unit = sapply(w1, function(y) if(is.na(y) || is.nan(y)) NA else 
                                if(word_units) switch(y, "", "thousand", "million", "billion", "trillion") else 
                                switch(y, "", "K", "M", "B", "T"))
  
  # Do the conversion and append the units
  xx[wx] = paste(as.character(round(x[wx]/10^(3*w),ndig)), unit, sep=ifelse(word_units," ",""))

  return(xx)
}

#' Convert a number to an abbreviated notation using 
#' @export
short_notation_exp = function(x, base=10, ndig=2, as_expr=FALSE) {
  x = sapply(x, function(x1) { 
                  z = signif(log(x1,base),ndig); 
                  if(z==0) bquote(""~10^{""~0}) else bquote(.(base)^{.(z)})})
  if(as_expr) x = sapply(x, as.expression)
  if(length(x)==1) x = x[[1]]
  return(x)
}

#' @export
N2T = short_notation

#' @export
N2Texp = short_notation_exp

#' @export
shorten_sequence = function(x, sep="-", collapse="-", f=length) 
  paste(c(paste(range(x),collapse=collapse),head(f(x),1)),collapse=sep)

################################################################################

#' Downweighting function
#' Downweights the values in x if they are below 'from' at the rate of 'speed'
#' downweight(seq(10, 200, 20))
downweight = function(x, from=100, speed=15)
  x * dnorm(pmin(x, from), from, speed) / dnorm(from, from, speed)

#' Produces names for a list of combinations of vectors
#' Takes vectors with parameter values and a vector of names and pastes them
#' together in a cartesian product way. Can be used to names the elements of a list
#' which contains the results of a run of analysis for each combination on a grid
#' (cartesian product) of parameter combinations.
#' @export
#' @examples
#' a = 1:3
#' b = 10:11
#' combine_names(a, b, names=c("a","b"))
combine_names = function(..., vars) {
  values = list(...)
  names(values) = match.call(expand.dots = FALSE)$...
  if(missing(vars)) {
    last = values[[length(values)]]
    if(length(last)==length(values)-1) {
      vars = last
      values[length(values)] = NULL
    } else {
      vars = names(values)
    }
  }
  stopifnot(length(values)==length(vars))
  v = lapply(seq_along(values), function(i) paste(vars[i],values[[i]], sep="="))
  paste_ = partial(paste, sep="_")
  do.call("paste_", rev(do.call("expand.grid", rev(v))))
}

#' Remove NA values from x
#' @export
#' @examples
#' de_na(c(1,2,NA,4,5,NA), 0)
#' de_na(c(1,2,NA,4,5,NA), 1:6)
#' de_na(c(1,2,NA,4,5,NA), Inf)
de_na <- function(x, y)
  ifelse(!is.na(x), x, y)
  
################################################################################

# improved list of objects
.ls.objects = function(pos=1, pattern, envir=parent.frame(), order.by, decreasing=FALSE, 
  head=FALSE, n=5, all.names=TRUE) {
  
  napply = function(names, fn) sapply(names, function(x) fn(try(get(x, envir=envir))))
  
  names = ls(pos=pos, pattern=pattern, envir=envir, all.names=all.names)
  
  obj.class = napply(names, function(y) as.character(class(y))[1])
  obj.mode = napply(names, mode)
  obj.type = ifelse(is.na(obj.class), obj.mode, obj.class)
  #obj.size = napply(names, function(y) object.size(get(y, envir=envir)))
  obj.size = napply(names, function(y) object.size(y))
  #obj.size = obj_size(list=names, with_unit=TRUE)
  #obj.dim = t(napply(names, function(y) as.numeric(dim(get(y, envir=envir)))[1:2]))
  obj.dim = t(napply(names, function(y) as.numeric(dim(y))[1:2]))
  vec = is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] = napply(names, length)[vec]
  out = data.frame(obj.type, obj.size, obj.dim)
  names(out) = c("Type", "Size in bytes", "Rows", "Columns")
  
  if(!missing(order.by))
    out = out[order(out[[order.by]], decreasing=decreasing),]
  
  if(head) out = head(out, n)
  
  return(out)

}

#' List all objects and their sizes
#' @export
lsos = function(..., envir=parent.frame(), n=10) {
  .ls.objects(..., envir=envir, order.by="Size in bytes", decreasing=TRUE, head=TRUE, n=n)
}

################################################################################

## Returns the sizes of supplied objects (in memory unit given in 'unit')
#' @export
obj_size = function(..., list=character(), unit="B", with_unit=TRUE, envir=parent.frame(), ndigit=2) {
  dots = match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || is.character(x)))) 
    error("'...' must contain names or character strings in obj_size()!")
  names = c(list, sapply(dots, as.character))
  # Get object sizes
  s = try(sapply(names, function(x) if(exists(x, envir=envir)) object.size(get(x, envir=envir)) else NA))
  # Convert the sizes in bytes to different units (unless error occured)
  if(class(s)=="try-error") {
    s = rep(NA, length(names))
  } else if(length(s)>0) {
    # Determine suitable unit
    if(missing(unit) && with_unit) unit = get_unit(s)
    # Convert units
    s = convert_unit(s, unit, append_unit=with_unit)
  }
  return(s)
}

################################################################################

## A more verbose version of load which can announce which objects have been loaded,
## the user can also specify which objects are expected and quit if they are not
## found inside the file
#' @export
load_objects = function(file, announce=FALSE, list_new=FALSE, expected_objects=NULL, 
                        quit_on_miss=FALSE, envir=parent.frame()) {
                        
  # Check for non-scaler file name
  if(length(file)!=1) error("Supply a single file name when calling ",this_fun_name(),"().")
  
  # Announce loading and file name
  if(announce) cat0("All objects from file '",file,"' will be loaded ...\n")
  
  # Check for missing file
  if(!file.exists(file)) error("File '",file,"' does not exist!")
  
  # Environment "local" means this function
  if(class(envir)=="character" && envir=="local") 
    envir = environment()

  ## Remove the objects that are in the file from environment 'envir'
  rme(expected_objects, envir=envir)
  
  # Load the file
  if(announce) cat0("Loading file (size ",file_size(file),") ... ")
  loaded = load(file, envir=envir)
  if(announce) cat0("done.\n")
  
  # List new objects loaded from the file
  if(list_new) {
    cat0("Getting sizes of loaded objects ...\n")
    sizes = obj_size(list=loaded, with_unit=TRUE, envir=envir)
    cat0("Loaded objects: ",paste(loaded," (",sizes,")",sep="",collapse=", "),"\n")
  } else 
    sizes = rep(NA, length(loaded))
  
  # Check for missing objects
  if(!is.null(expected_objects)) {
    miss = setdiff(expected_objects, loaded)
    if(length(miss)>0) {
      msg = paste0("The following expected objects were not loaded: '",paste(miss,collapse="' '"),"'")
      if(quit_on_miss) error(msg) else warn(msg)
    } else {
      cat0("All expected objects were successfully loaded.\n")
    }
  }

  return(invisible(cbind("Object"=loaded, "Size"=sizes)))

} # load_objects

################################################################################

#' Load objects from file
#'
#' Loads objects from a file and assigns the contents into variables listed in
#' \code{as} inside the environment in \code{envir} (the parent frame of the call
#' to \code{loadAs} by default).
#' @export
loadAs = function(file, as, what, envir=parent.frame()) {
  stopifnot(length(what)==length(as))
  loaded = load(file, envir=environment())
  if(any(what %notin% loaded)) 
    error("Object(s) '",what[which(what %notin% loaded)],"' were not found in file '",file,"'.")
  for(i in 1:length(what)) assign(as[i], get(what[i]), envir=envir)
  for(x in setdiff(loaded, what)) assign(x, get(x), envir=envir)
  return(invisible(cbind(what=what, as=c(as,setdiff(loaded, what)))))
}

################################################################################

#' Lists objects
#' 
#' Lists all objects inside given files or inside files that match pattern in 
#' \code{pattern} relative to the path in \code{dir}
#' @export
get_all_objects = function(files=NULL, dir=".", pattern="^.*[.]RData$") {

  setwd(dir)
  if(missing(files)) files = list.files(pattern=pattern)
  cat("Obtaining names of all objects ...\n"); .fc()
  loaded = list()
  
  ## Load all files
  for(file in files)
    loaded[[file]] = load_objects(file, list_new=TRUE, announce=TRUE, envir="local")
  
  cat("\nThe files contain the following objects (per file):\n\n")  
  print(loaded)
  
  return(invisible(NULL))

} 

################################################################################

#' Find objects in files
#'
#' Find an object with name in \code{object_name} inside files in \code{files} 
#' relative to the path supplied in \code{dir}. If a pattern is given (via 
#' \code{pattern}) instead of a list of file names, then all files that match 
#' the pattern are searched through looking for the object in \code{object_name}.
#' @export
find_object = function(object_name, files=NULL, dir=".", pattern="^.*[.]RData$", 
                       stop_on_found=TRUE, announce=TRUE) {
                       
  odir = getwd()
  on.exit(setwd(odir))
  
  stopifnot(!missing(object_name) && length(object_name)) 
  
  if(missing(files) || !length(files)) 
    files = list.files(dir, pattern=pattern)

  setwd(dir)
  cat("Searching for object '",object_name,"' ...\n", sep=""); .fc()
  identified_files = NULL
  for(file in files) {
    loaded = load_objects(file, list_new=TRUE, announce=announce, envir="local")
    if(object_name %in% loaded[,1]) {
      identified_files = c(identified_files, file)
      if(stop_on_found) break
    }
  }
  
  if(announce) {
    if(!length(identified_files)) {
      catn("Unfortunately the object '",object_name,"' could not be find in any of the files.")
    } else if(stop_on_found) {
      catn("Object '",object_name,"' found in file '",identified_files,"'. Search stopped.")  
    } else {
      catn("Object '",object_name,"' found in ",length(identified_files)," files.")  
    }
  }
  
  return(identified_files)
} 

################################################################################

#' Check if a sequence in 'x' has only unique elements
#' @export
is_unique = function(x) 
  !anyDuplicated(x)

#' Check if all elements in 'x' have the same value
#' @export
is_all_same = function(x) 
  length(unique(x))<=1

#' Check if a sequence in 'x' is decreasing
#' @export
is_decreasing = function(x, strictly=FALSE) 
  !is.unsorted(rev(x), strictly=strictly) 

#' Check if a sequence in 'x' is increasing
#' @export
is_increasing = function(x, strictly=FALSE) 
  !is.unsorted(x, strictly=strictly)

#' Check if elements in 'x' are effectively integers (i.e. close enough to one)
#' @export
is_integer = function(x, tol=.Machine$double.eps) 
  return(abs(x - round(x)) < 2*tol)
  #return(x==floor(x+2*tol))

#' Check if elements of 'x' are odd
#' @export
is_odd = function(x, tol=.Machine$double.eps) 
  abs((x+1) %% 2) < tol

#' Check if elements of 'x' are even
#' @export
is_even = function(x, tol=.Machine$double.eps) 
  abs(x %% 2) < tol

## Checks if a reduction of elements of 'x' are NA. Default reduction is 'all()'
## which returns TRUE iff all elements are NA. An obvious useful alternative is 
## to use 'any()' as argument to 'freduce'.
#' @export
is_na = function(x, freduce=all) 
  freduce(is.na(x))

################################################################################

## Checks if elements in 'x' are numeric even if stored as strings. In other words, 
## it checks whether x is of class numeric or whether it is a string that can has the format
## of a number, otherwise it returns FALSE. Function freduce is applied to the vector.
#' @export
is_number = function(x, freduce=all, dec='.', sep_thousand="") {
  pattern_only_digits = "^[+-]?[0-9]+$"
  pattern_real = paste0("^[+-]?[0-9]*[",dec,"][0-9]*$")
  pattern_some_digits = "[0-9]"
  a = if(is.numeric(x)) {
    rep(TRUE, length(x)) 
  } else if(is.character(x)) {
    if(nchar(sep_thousand)>0) x = gsub(paste0('[',sep_thousand,']',''),'',x)
    c1 = regexpr(pattern_only_digits, x)>0 
    c2 = regexpr(pattern_real, x)>0 | regexpr(pattern_some_digits, x)>0
    c1 | c2
  } else {
    rep(FALSE, length(x))
  }
  return(freduce(a))
}

################################################################################

#' Convert x to numeric without any warnings about non-numeric elements
#' @export
as_numeric = function(x) {
  y = as.numeric(rep(NA, length(x)))
  y[is_number(x, freduce=I)] = as.numeric(x[is_number(x, freduce=I)])
  return(y)
}

################################################################################

#' Changes the type of x to "numeric" unless it does not contain only numbers
#' @export
make_numeric = function(x, on_error=function(...) stop("Cannot convert x to class 'numeric'."))
  if(is.numeric(x)) x else if(is_number(x)) as.numeric(x) else on_error(x)

################################################################################

#' Convert to integer by stripping all non-number substrings in each element of x
#' @export
force_as_integer = function(x, ignore_sign=TRUE) {
  w = if(ignore_sign) rep(-1, length(x)) else regexpr(paste0('[-][0-9]'), x)
  y = ifelse(w<0, gsub("[^0-9]*","",x),
                  paste0('-',gsub("[^0-9]*","",substr(x,w+1,nchar(x)))))
  as.integer(y)
}

################################################################################

#' Convert to integer by stripping all non-number substrings in each element of x
#' @export
force_as_real = function(x, dec='.', ignore_sign=TRUE) {
  w = regexpr(paste0('[',dec,']'), x)
  lp = ifelse(w<0, force_as_integer(x, ignore_sign=ignore_sign), 
                   force_as_integer(substr(x,1,w-1), ignore_sign=ignore_sign))
  rp = ifelse(w<0, 0, force_as_integer(substr(x,w+1,nchar(x)), ignore_sign=TRUE))
  as.numeric(paste(lp,rp,sep='.'))
}

################################################################################

#' Check if 'x' is in scientific notation (e.g. 1.234E5)
#' @export
is_scientific = function(x, freduce=all) {
  a = sapply(x, is.numeric) & regexpr("^[0-9.]+[Ee][-0-9]+$", x)>0
  return(freduce(a))
}

################################################################################

#' Checks if values in 'x' are integers
#' @export
is_round = function(x, eps=.Machine$double.eps)
  if(is.na(x) || is.infinite(x)) FALSE else abs(round(x)-x) <= eps

################################################################################

#' Check if 'x' is a diagonal matrix 
#' @export
is_diag = function(x) 
  is.matrix(x) && nrow(x)==ncol(x) && (length(x)==1 || max(abs(x[upper.tri(x)]))<.Machine$double.eps)

################################################################################

#' Check if M is a positive-definite matrix
#' @export
is_posdef = function(M) {

  stopifnot(is.numeric(M))
  if(length(M)==1 && M>0) return(TRUE)
  stopifnot(is.matrix(M), nrow(M)==ncol(M))
  
  llibrary(mnormt)
  x = try(mnormt::rmnorm(1, mean=rep(0,nrow(M)), varcov=M), silent=TRUE)
  if(class(x)!="try-error") TRUE else structure(FALSE, msg=as.character(attr(x, "condition")))
  
}

################################################################################

#' Draw a positive-definite matrix
#'
#' Generates a "random" (more like arbitrary) positive-definite matrix with 
#' user-specified positive eigenvalues. If eigenvalues are not specified, 
#' they are generated from a uniform distribution on (1,10)
#' @export
rposdefmat = function(n, ev=runif(n,1,10), all.positive=FALSE) {

  Z = matrix(rnorm(n^2), ncol=n)
  decomp = qr(Z)
  Q = qr.Q(decomp) 
  R = qr.R(decomp)
  d = diag(R)
  ph = d / abs(d)
  O = Q %*% diag(ph)
  Z = t(O) %*% diag(ev) %*% O
  if(all.positive) Z = abs(Z)
  
  return(Z)
  
}

################################################################################

#' Draw a random variance matrix with sigma2 on the diagonal
#' @export
rvariance = function(n, sigma2=rep(1,n), ev=runif(n,1,200)) {
  if(n==1) {
    as.matrix(sigma2)
  } else {
    V = rposdefmat(n, ev=ev)
    D = diag(sqrt(sigma2 / diag(V)))
    V = D %*% V %*% D
    0.5*(V + t(V))
  }
}

################################################################################

#' Produce a variance matrix
#'
#' Produces a variance matrix with \code{rho} on the diagonal according to the 
#' specified type which can be either "random" (see \code{rvariance}) or has 
#' the structure according to \code{type}.
#' @export
def_Sigma = function(rho, N, nb=1, random=FALSE, type=c("block", "band"), bandsize=0) {

  type = match.arg(type)
  
  if(N==1) return(diag(1))
  
  if(random) {
    Sigma = rvariance(N)
  } else {
    if(N%%nb!=0) error("def_Sigma: Number of blocks 'nb' must divide 'N'.")
    n = N%/%nb
    Sigma = matrix(rho, nrow=n, ncol=n)
    if(type=="band") {
      llibrary(Matrix)
      Sigma = band(Sigma, -abs(bandsize), abs(bandsize))
    }
    Sigma = 0.5*(Sigma+t(Sigma))
    diag(Sigma) = 1.
    if(nb>1) Sigma = drep(Sigma, nb)
  }

  return(Sigma)
  
}

################################################################################

#' Normal probability of a rectangle
#'
#' Calculates the probability of an arbitrary box with respect to an equi-correlated 
#' multivariate normal random vector of dimension given by the size of upper/lower
#' @export
pmnormrect = function(upper=c(Inf, Inf), lower=c(-Inf,-Inf), mean=rep(0, length(upper)), 
                      Sigma=diag(length(upper)), varcov) {
                      
  if(!missing(varcov) && !missing(Sigma)) error("Supply either Sigma or varcov.")
  if(!missing(varcov)) Sigma = varcov
  
  llibrary(mnormt)
  
  P = if(all(is.infinite(lower))) 
    mnormt::pmnorm(upper, mean=mean, varcov=Sigma) else
    mnormt::sadmvn(lower=lower, upper=upper, mean=mean, varcov=Sigma)
    
  return(as.numeric(P))
}

################################################################################

#' Calculate a normal distribution p-value
#' Returns the p-value relative to the normal distribution with parameters m, s
#' @export
normpval = function(x, two.sided=FALSE, m=0, s=1)
 if(two.sided) 2*pnorm(abs(x), m=m, s=s, lower.tail=FALSE) else pnorm(x, m=m, s=s, lower.tail=FALSE)

################################################################################

#' Draw a normal distribution p-value
#' Draws a random sample of p-values relative to the normal distribution with parameters m, s
#' @export
rnormpval = function(M, mu, Sigma=def_Sigma(rho, N=length(mu)), rho=0, two.sided=FALSE, trace=1, what=c("p","X","pX")) {

  #stopifnot(!missing(M), !missing(mu), !missing(Sigma))
  stopifnot(is.matrix(Sigma))
  what = match.arg(what)
  
  if(missing(mu)) mu = rep(0, nrow(Sigma))

  if(is_diag(Sigma)) {
  
    X = matrix(rnorm(M*length(mu), m=rep(mu, e=M), s=rep(sqrt(diag(Sigma)), e=M)), nrow=M)
  
  } else {
 
    llibrary(mnormt)
    X = try(mnormt::rmnorm(M, mean=mu, varcov=Sigma))
    if(class(X)=="try-error") return(list(p=NA))
    if(M==1) X = matrix(X, nrow=1)
    
  }
  
  if(what=="p" || what=="pX")
    #p = if(two.sided) 2*pnorm(abs(X), lower.tail=FALSE) else pnorm(X, lower.tail=FALSE)
    p = normpval(X, two.sided)
  
  return(switch(what, "X"=X, "p"=p, list(X=X, p=p)))
      
}

rnormm = function(..., what="X") rnormpval(..., what=what)
    
################################################################################

#' Simulate p-values
#'
#' A function that simulates p-values (from the normal model only so far)
#' @export
simulate_pval = function(M, Sigma, N, n=1, n0=N, mu0=0, n1=0, mu1=0, alpha=0.05, type="bySigma", 
  model=c("norm", "unif"), two.sided=FALSE, evaluate_signif=FALSE, lambda, trace=1) {

  if(missing(M)) error("Missing argument 'M' in simulate_pval().")
  if(missing(N)) {
    if(missing(Sigma) || !is.matrix(Sigma)) error("Either 'N' or 'Sigma' (matrix) must be supplied in simulate_pval().")
    N = nrow(Sigma)
  }
  
  if(trace>0) cat("Simulating ",M*N," p-values ... ")
  
  if(!missing(Sigma) && is.null(Sigma) && type=="bySigma")
    error("Either supply non-null Sigma or change the type to something other than 'bySigma'.")
          
  if(!missing(Sigma) && !is.null(Sigma)) {
  
    label = "bySigma"
    
    if(length(Sigma)==1) Sigma = diag2(rep(1, N), Sigma)
    
    if(n0+n1!=nrow(Sigma)) error("Conflict between n0, n1 and Sigma.")    
    
    if(length(mu0)!=1 && length(mu0)!=n0) error("Supply consistent mu0 and n0.")
    if(length(mu1)!=1 && length(mu1)!=n1) error("Supply consistent mu1 and n1.")

    mu = sqrt(n) * c(rep(mu1, n1/length(mu1)), rep(mu0, n0/length(mu0)))
    
    model = head(model, 1)
    
    if(model=="norm") {
    
      if(is_diag(Sigma)) {
      
        nn = matrix(rnorm(M*length(mu), m=rep(mu, e=M), s=rep(sqrt(diag(Sigma)), e=M)), nrow=M)
      
      } else {
     
        llibrary(mnormt)
        nn = try(mnormt::rmnorm(M, mean=mu, varcov=Sigma))
        if(class(nn)=="try-error") return(list(p=NA))
        if(M==1) nn = matrix(nn, nrow=1)
        
      }
      
      p = if(two.sided) 2*pnorm(abs(nn), lower.tail=FALSE) else pnorm(nn, lower.tail=FALSE)
      rm(nn)
      
    } else error("Unknown model '",model,"'.")
    
    if(!is_posdef(Sigma)) error("Sigma must be positive-definite.")
    
    N = nrow(Sigma)
    
    ### DEBUG ###
    add_dependent = FALSE
    if(add_dependent) {
      if(N==1) {
        l1 = l2 = 1/3; l3 = max(0,1 - l1 - l2)
        p0 = p
        p = cbind(p0, 1-p0)
      } else if(N==3) {
        p[,ncol(p)] = 1 - apply(p[,1:(ncol(p)-1)], 1, max)
      }
    }
    ### DEBUG ###
    
  } else if(type=="indep") {

    label = "indep"
    if(trace>0) cat("(independent) ... ")
    p = matrix(runif(N*M), nrow=M, ncol=N)

  } else if(type=="maxdep") {

    label = "maxdep"
    if(trace>0) cat("(max dependent) ... ")
    p = matrix(runif(N*M), nrow=M, ncol=N)
    p[,ncol(p)] = 1 - apply(p[,1:(ncol(p)-1),drop=FALSE], 1, mean)

  } else if(type=="complcounterdep") {

    label = "complcounterdep"
    if(trace>0) cat("(completely counter dependent) ... ")
    p = matrix(runif(M), nrow=M, ncol=1)
    p = if(N==2) cbind(p, 1-p) else if(N==3) cbind(p, p, 1-p)    

  } else if(type=="compldep") {
    
    label = "compldep"
    if(trace>0) cat("(completely dependent) ... ")
    p = matrix(runif(M), nrow=M, ncol=N)

  } else error("Unknown type=",type," in simulate_pval().")

  if(trace>0) cat("done.\n")

  # Evaluate significance
  if(evaluate_signif) {
    if(trace>0) cat("Evaluating significance ... ")
    lam = eval(parse(text=lambda))
    keep = p <= lam
    n2 = apply(keep, 1, sum)
    p2 = p / lam
    fwer = mean(apply(n2>0 & p2 <= alpha / n2, 1, any))
    if(trace>0) cat("done.\n")
  } else p2 = n2 = keep = fwer = NULL
  
  list_clean(list(p=p, p2=p2, n2=n2, keep=keep, fwer=fwer))
  
} # simulate_pval

#############################################################

#' Perform multiple testing procedure
#'
#' Given a vector of p-values on input, performs a selected multiple testing
#' procedure.
#'
#' @return A vector of rejections.
#'
#' @export
test_pval = function(p, method="cbonf", alpha=0.05, lamSFG=0.5, lam, scale_up=TRUE) {

  methods = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none","SFG","hmp","Hartung")
  cmethods = paste0("c",methods)
  method = match.arg(method, c(methods, cmethods))
  do_cond = method %in% cmethods
  
  if(do_cond) {
    
    if(missing(lam)) error("Missing argument 'lam'.")
    wk = p < lam
    pp = p[wk] / ifelse(scale_up, lam, 1)
    meth = sub("^c","",method)
    
  } else {
  
    pp = p
    wk = rep(TRUE, length(pp))
    meth = method
    
  }
  
  rej = rep(FALSE, length(p))
  if(meth=="hommel") {
    
    if(any(wk)) rej[wk] = !statmod::hommel.test(pp, alpha=alpha)
    
  } else if(meth=="SFG") {
  
    if(any(wk)) rej[wk] = !sfg.test(pp, alpha=alpha, lamSFG=lamSFG)
  
  } else if(meth=="hmp") {
  
    if(any(wk)) rej[which(wk)[1]] = !hmp.test(pp, alpha=alpha)
  
  } else if(meth=="Hartung") {
  
    if(any(wk)) rej[which(wk)[1]] = !hartung.test(pp, alpha=alpha)
  
  } else if(any(meth==methods)) {
  
    if(any(wk)) rej[wk] = p.adjust(pp, method=meth) < alpha
  
  } else error("Unknown method '",method,"' in test_pval.")
  
  rej

}
  
#############################################################

#' Bonferroni test
#' @export  
bonf.test = function(p, alpha=0.05, lam=2, scale_up=TRUE, what="nr") {
 
  if(is.vector(p)) p = matrix(p, ncol=1)

  do_cond = lam <= 1. && lam > 0
  N = if(do_cond) apply(p < lam, 2, sum) else nrow(p)
  pp = p * N / if(do_cond && scale_up) lam else 1
  non_signif = pp >= alpha
  
  switch(what, "nr"=non_signif, "p"=pp, cbind(ns=non_signif, p=pp))
  
}

#############################################################
  
#' Holm test
#' @export  
holm.test = function(p, alpha=0.05, what="nr") {

  n = length(p)
  i = 1:n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * (n+1-i)
  non_signif = pp >= alpha
  wns = which(non_signif)
  if(length(wns)>0) non_signif[wns[1]:n] = TRUE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#############################################################

#' Hochberg test
#' @export  
hochberg.test = function(p, alpha=0.05, what="nr") {
  
  n = length(p)
  i = 1:n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * (n+1-i)
  non_signif = pp >= alpha
  ws = which(!non_signif)
  if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#############################################################

#' Benjamini-Hochberg test (FDR)
#' @export  
bh.test = function(p, alpha=0.05, what="nr") {
  
  n = length(p)
  i = 1:n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * n / i
  non_signif = pp >= alpha
  ws = which(!non_signif)
  if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#############################################################

#' Benjamini-Yekutieli test (FDR)
#' @export  
by.test = function(p, alpha=0.05, what="nr") {

  n = length(p)
  i = 1:n
  C = log(n) - digamma(1) + 0.5 / n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * n * C / i
  non_signif = pp >= alpha
  ws = which(!non_signif)
  if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#############################################################

#' Storey test (aka Storey-Fincher-Goncharuk test)
#' @export  
sfg.test = function(p, alpha=0.05, lamSFG=0.5, lam=2, scale_up=TRUE, what="nr") {
 
  if(is.vector(p)) p = matrix(p, ncol=1)

  do_cond = lam <= 1. && lam > 0
  N = if(do_cond) apply(p < lam, 2, sum) else nrow(p)
  p = p / if(do_cond && scale_up) lam else 1
  m0 = if(lamSFG>=1) nrow(p) else ( apply(p > lamSFG & p <= 1, 2, sum) + 1 ) / ( 1 - lamSFG )
  pp = p * m0 / if(do_cond && scale_up) lam else 1
  non_signif = pp >= alpha
  
  switch(what, "nr"=non_signif, "p"=pp, cbind(ns=non_signif, p=pp))
  
}

#############################################################
  
#' Harmonic mean p-value test
#'
#' This implements the global test based on the harmonic mean 
#' p-value, which is a combination p-value that is insensitive
#' to dependence among the combined p-values. 
#'
#' Requires package \code{harmonicmeanp}.
#'
#' @export  
hmp_test = function(p, alpha=0.05, what="ns") { 
  non_signif = harmonicmeanp::p.hmp(p) >= alpha
  switch(what, "ns"=non_signif, 
         stop(this_fun_name(), " can only return 'ns' (i.e. combined non-significance status)."))
  
}

#############################################################
  
#' Hartung test for dependent p-values
#'
#' Requires library \code{punitroots} which can be found at r-forge (see examples below for installation).
#' 
#' @examples
#' install.packages("urca")
#' install.packages("CADFtest")
#' install.packages("punitroots", repos="http://r-forge.r-project.org")
#' @export  
hartung.test = function(p, alpha=0.05, kappa=0.2, what="ns") {
 
  non_signif = punitroots::Hartung(p, kappa=kappa)$p.value >= alpha
  
  switch(what, "ns"=non_signif, 
         stop("hartung.test() can only return 'ns' (i.e. combined non-significant)."))
  
}

#############################################################
  
mtc.test = function(p, alpha=0.05, method="holm") {
  
  ## Hommel not yet implemented here, so just use statmod's version
  if(method=="hommel") return(statmod::hommel.test(p, alpha=alpha))
  
  # Sort the p-values (if needed)
  po = if(method!="bonferroni" && is.unsorted(p)) sort(p) else p
  
  # Set the corrections
  n = length(p)
  i = 1:n
  k = switch(method, "bonferroni"=n, 
                     "holm"=n+1-i, 
                     "hochberg"=n+1-i, 
                     "BH"=n/i, 
                     "BY"=n/i*(log(n)-digamma(1)+0.5/n), 
                     stop("Unknown method."))
  
  # Determine rejections
  non_signif = po * k >= alpha  
  
  # Modify rejections for holm, hochberg, bh, by
  if(method=="holm") {
    wns = which(non_signif)
    if(length(wns)>0) non_signif[wns[1]:n] = TRUE
    non_signif = non_signif[rank(p)]
  } else if(any(method==c("hochberg","BH","BY"))) {
    ws = which(!non_signif)
    if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
    non_signif = non_signif[rank(p)]
  }
  
  return(non_signif)
}

#############################################################

mtc.test.mul = function(p, alpha=0.05, method="holm") {

  if(is.vector(p)) p = matrix(p, ncol=1)

  if(method=="hommel") {
    apply(p, 2, statmod::hommel.test, alpha=alpha) 
  } else apply(p, 2, mtc.test, alpha=alpha, method=method)
  
}

#############################################################

mtctestFast = function(p, alpha=0.05, method=c("bonferroni","hommel","holm","hochberg","BH","BY","fdr","none","SFG"),
  n1=-1, lam=2, lamSFG=0.5, what=c("fwer","fdr","pow"), trace=0, dll_lib=NULL, unload=TRUE) { 
  
  if(is.null(dll_lib))
    dll_lib = "d:/Dropbox/Projects/Jules/Scripts/fortran/mtc.dll"
    
  cat("\nUsing DLL library '",dll_lib,"' ...\n")
  
  file_exists(dll_lib)
  
  method = match.arg(method)
  method = switch(method, "none"=0,
                          "bonferroni"=1, 
                          "hommel"=2, 
                          "holm"=3, 
                          "hochberg"=4, 
                          "BH"=, "fdr"=5, 
                          "BY"=6,
                          "SFG"=7,
                          error("Unknown method '",method,"'."))
                  
  if(is.vector(p)) p = matrix(p, ncol=1)
                        
  n = nrow(p)
  m = ncol(p)
  stopifnot(n>0, m>0)
  
  if(!any(names(getLoadedDLLs())=="mtc")) {
    if(trace>0) cat("Loading library file '",dll_lib,"' ...\n")
    if(!file.exists(dll_lib)) error("Library file '",dll_lib,"' does not exist!")
    loaded = dyn.load(dll_lib)
  }
  
  # Make sure the variables have the right size and type
  ns = matrix(FALSE, nrow=n, ncol=m)
  get_fwer = as.logical("fwer" %in% what)
  get_fdr = as.logical("fdr" %in% what)
  get_pow = as.logical("pow" %in% what)
  fwer = fdr = double(1)
  pow = double(max(1,n1))
  n = as.integer(n)
  m = as.integer(m)
  method = as.integer(method)
  ierr = integer(1)
  alpha = as.double(alpha)
  lam = as.double(lam)
  lamSFG = as.double(lamSFG)
  n1 = as.integer(n1)
  
  # Call the Fortran function
  ffun = '__mtc_MOD_mtctest_mul'
  if(trace>0) cat("Calling external function '",ffun,"' for method '",method,"' ... ")
  #OUT = .C(ffun, ns=ns, p=p, n=n, m=m, alpha=alpha, method=method, ierr=ierr, 
  #         lam=lam, lamSFG=lamSFG, n1=n1, fwer=fwer, fdr=fdr, pow=pow)
  args = list(ffun, ns=ns, p=p, n=n, m=m, alpha=alpha, method=method, ierr=ierr, 
              lam=lam, lamSFG=lamSFG, n1=n1, get_fwer=get_fwer, get_fdr=get_fdr, 
              get_pow=get_pow, fwer=fwer, fdr=fdr, pow=pow)
  OUT = do.call(".C", args)
  
  if(trace>0) cat("done.\n")
  
  if(OUT$ierr!=0) error("Non-zero return code '",OUT$ierr,"' returned by '",ffun,"' in mtctestFast().")
  
  if(unload && any(names(getLoadedDLLs())=="mtc")) dyn.unload(dll_lib)
  
  return(if(n1>=0) list(ns=OUT$ns, fwer=OUT$fwer, fdr=OUT$fdr, pow=OUT$pow) else OUT$ns) 

}

#############################################################

#' Sort a matrix of p-values (or any matrix, really)
#'
#' Sorts the columns of a given matrix. If 'n1' is supplied and positive,
#' the rows \code{1:n1} and \code{(n1+1):nrow(p)} are sorted separately.
#' @export
sort_pval_matrix = function(p, n1=0, decreasing=FALSE) {

  n1 = max(n1, 0)
  if(n1==0) {
    apply(p, 2, sort, decreasing=decreasing)
  } else {
    stopifnot(n1<=nrow(p))
    p[1:n1,] = apply(p[1:n1,,drop=FALSE], 2, sort, decreasing=decreasing)
    p[-(1:n1),] = apply(p[-(1:n1),,drop=FALSE], 2, sort, decreasing=decreasing)
    p
  }
  
}
                  
#############################################################

#' Type-I and type-II errors
#'
#' Determine the type-I (FWER, FDR) and type-II (power) errors of 
#' selected multiple testing procedures
#' @export
mtp_errors = function(p, method="cbonferroni", n1=0, alpha=0.05, lam, lamSFG=0.5, scale_up=TRUE) {

  adj_methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  methods = c(adj_methods, "SFG", "hmp")
  cmethods = paste0("c",methods)
  method = match.arg(method, c(methods, cmethods))
  do_cond = method %in% cmethods
  
  if(do_cond && missing(lam)) error("Missing argument 'lam'.")
  
  if(is.vector(p)) p = matrix(p, ncol=1)
  n = nrow(p)
  n1 = max(n1, 0)

  meth = if(do_cond) sub("^c","",method) else method

  if(meth=="bonferroni") {
  
    if(TRUE) {
    N = if(do_cond) apply(p < lam, 2, sum) else n
    a = alpha / N * if(do_cond && scale_up) lam else 1
    p0 = p[(n1+1):n,,drop=FALSE]
    fwer = mean(apply(p0, 2, min) < a)
    p1 = if(n1==0) NULL else p[1:n1,,drop=FALSE]
    nsig = if(n1==0) NA else apply(p1 < a, 2, sum)
    pow = sapply(1:n1, function(k) mean(nsig>=k))
    } else
    rej = bonf.test(p, alpha=alpha, lam=lam, scale_up=scale_up)
  
  } else {
  
    M = ncol(p)
    rej = matrix(FALSE, nrow=n, ncol=M)
    for(i in 1:M) {
    
      if(do_cond) {
        wk = p[,i] < lam
        pp = p[wk,i] / ifelse(scale_up, lam, 1)
      } else {
        wk = rep(TRUE, n)
        pp = p[,i]
      }
      
      if(!any(wk)) next
              
      rej[wk,i] = 
      if(meth=="bonferroni") {
        !mtc.test(pp, alpha=alpha, method="bonferroni")
      } else if(meth=="hommel") {
        !statmod::hommel.test(pp, alpha=alpha)
      } else if(meth=="holm") {
        !holm.test(pp, alpha=alpha)
      } else if(meth=="hochberg") {
        !hochberg.test(pp, alpha=alpha)
      } else if(meth=="BH") {
        !bh.test(pp, alpha=alpha)
      } else if(meth=="BY") {
        !by.test(pp, alpha=alpha)
      } else if(meth=="SFG") {
        !sfg.test(pp, alpha=alpha, lamSFG=lamSFG)
      } else if(any(meth==adj_methods)) {
        p.adjust(pp, method=meth) < alpha
      } else error("Unknown method '",method,"' in test_pval.")
      
    } # for(i in 1:M)
    
    fwer = mean(apply(rej[(n1+1):n,,drop=FALSE], 2, max)>0)
    pow = sapply(1:n1, function(k) mean(apply(rej[1:n1,,drop=FALSE], 2, sum)>=k))
      
  }
  
  return(list(fwer=fwer, fdr=NA, pow=pow))

} # mtp_errors
  
#############################################################

#' @name mtp_errors
#' @export
mtp_errors_fast = function(p, method="cbonferroni", n1=0, alpha=0.05, lamSFG=0.5, 
  lam, scale_up=TRUE, what=c("fwer","fdr","pow")) {

  methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none", "SFG")
  cmethods = paste0("c",methods)
  method = match.arg(method, cmethods)
  do_cond = method %in% cmethods
  
  if(do_cond && missing(lam)) error("Missing argument 'lam'.")
  
  X = mtctestFast(p, alpha=alpha, method=if(do_cond) sub("^c","",method) else method, 
                  n1=max(n1, 0), lam=ifelse(do_cond, lam, 2), what=what)
      
  list(fwer=X$fwer, fdr=X$fdr, pow=X$pow)

}
  
#############################################################

#' @name mtp_errors
#' @export
mtp_errors_slow = function(p, methods, n1=0, k, alpha=0.05, lamSFG=0.5, lam, scale_up=TRUE, trace=1, what=c("fwer","fdr","power")) {

  if(missing(methods))
    methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "SFG", "hmp", "Hartung")
    
  if(is.vector(p)) p = matrix(p, ncol=1)

  L = list()
  for(method in methods) {
  
    if(trace>0) cat0("evaluating rejections for method '",method,"' (column-wise on p) ... ")
    
    rej = apply(p, 2, test_pval, alpha=alpha, method=method, lam=lam, scale_up=scale_up, lamSFG=lamSFG)  
    
    if(trace>0) cat0("evaluating fwer, fdr, power ... ")
    
    fwer = fdr = power = NULL
    
    if(n1<nrow(rej) && any(c("fwer","fdr") %in% what)) { 
      nrej0 = apply(rej[(n1+1):nrow(rej),,drop=FALSE], 2, sum)
      if("fwer" %in% what) fwer = mean(nrej0>=1)
      if("fdr" %in% what) fdr = mean(nrej0 / pmax(1,apply(rej, 2, sum)))
    }
    
    if(n1>0 && "power" %in% what) {
      if(missing(k)) k = 1:n1
      nrej1 = apply(rej[k,,drop=FALSE], 2, sum)
      power = sapply(k, function(k1) mean(nrej1>=k1))
      names(power) = paste0(">=",k)
    }

    L1 = Filter(length, list(fwer=fwer, fdr=fdr, power=power))
    L = append(L, structure(list(L1), names=method))
    
    if(trace>0) catn()
    
  }
  
  L

}

#############################################################

#' Pattern sort
#'
#' Sorts input according to the order given in pattern (given in 
#' \code{pattern}) by matching the names of the input (i.e. \code{x}) 
#' against the pattern.
#'
#' @export
pattern_sort = function(x, pattern, get_order=FALSE) {
  
  if(!length(x) || is.null(names(x))) 
    return(if(get_order) seq_along(x) else x)
  
  nam = names(x)
  ords = lapply(pattern, function(pat) which(regexpr(pat, nam)>0))
  ords = append(ords, list(setdiff(seq_along(x), unlist(ords))))
  reords = lapply(ords, function(w) order(nam[w]))
  ords = lapply(seq_along(ords), function(i) ords[[i]] = ords[[i]][reords[[i]]])
  ord = unlist(ords)
  
  if(get_order) ord else x[ord]
}

################################################################################

#' Reorder x according to order and drop the extra attributes generated by 'reorder()'
#' @export
reord = function(x, order) { 
  y = reorder(x, order)
  attributes(y) = NULL
  y
}

################################################################################

#' A more versatile version of \code{base:which.max} and \code{which.min}.
#'
#' Essentially, these functions behave like their base equivalents except they can 
#' return the position of both first or last extreme in a vector (the last by default). 
#' They also differ in the treatment of NAs. The removal of NA values can be done in two
#' ways, they are either removed and then the position of the extreme in a vector
#' stripped of NAs is returned, or they are ignored (default) and the position
#' of a extreme within the entire original vector is returned.
#'
#' @export
#' @examples
#' which_max(1:5)
#' which_max(1:5, last=FALSE)
#' which_max(c(NA,1:5))
#' which_max(c(NA,1:5), na.ignore=FALSE, na.rm=TRUE)
which_max = function(x, last=TRUE, na.rm=FALSE, na.ignore=TRUE, arr.ind=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  if(na.ignore) x[is.na(x)] = -Inf
  w = if(last) length(x) + 1 - which.max(rev(x)) else which.max(x)
  if(length(dim(x))>1 && arr.ind) 
    w = which(array(seq_along(x), dim=dim(x))==w, arr.ind=arr.ind)
  return(w) 
}
  
################################################################################

#' @name which_max
#' @export
which_min = function(x, last=TRUE, na.rm=FALSE, na.ignore=TRUE, arr.ind=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  if(na.ignore) x[is.na(x)] = Inf
  w = if(last) length(x) + 1 - which.min(rev(x)) else which.min(x)
  if(length(dim(x))>1 && arr.ind) 
    w = which(array(seq_along(x), dim=dim(x))==w, arr.ind=arr.ind)
  return(w) 
}

################################################################################

#' Get the minimal distance between elements in 'x'
#' @export
min_dif = function(x)
  return(if(length(x)==1) NA else min(diff(sort(x))))
  #return(min(sapply(x, function(x1) min(abs(x1-x)))))

################################################################################

#' Replace zeros in 'x' with 'value'
#' @export
unzero = function(x, value=1) {
  x[x==0] = value
  x
}

################################################################################

#' A wrapper for base::max which doesn't produce warnings
#' @export
max0 = function(..., na.rm=FALSE, val0=-Inf) {
  x = unlist(c(...))
  if(na.rm) x = x[!is.na(x)]
  if(!length(x)) val0 else base::max(..., na.rm=na.rm)
}

################################################################################

#' An easier way to produce histograms with 'nbreaks' bins
#' @export
hist2 = function(..., nbreaks) {
  if(missing(nbreaks)) {
    hist(...)
  } else {
    arg = list(...)
    x = if(any(names(arg)=="x")) arg[["x"]] else arg[[1]]
    x = x[is.finite(x)]
    if(length(x)==0) error("No values to plot in hist2().")
    breaks = seq(min(min(x)*0.99,min(x)-1e-9),max(max(x)*1.01,max(x)+1e-9), l=nbreaks)
    hist(..., breaks=breaks)
  }
}

################################################################################

#' Contingency table
#'
#' Does the same thing as table but makes sure that the counts of values in obligate are
#' included in the final table but only if a tabulation of a single vector is performed
#' @export
table2 = function(..., obligate=NULL) {
  tab = table(...)
  nams = names(tab)
  if(!is.null(obligate) && !is.null(nams)) {
    miss = setdiff(obligate, nams)
    if(length(miss)>0) {
      ztab = as.table(rep(0, length(miss)))
      names(ztab) = miss
      tab = append(tab, ztab)
      nams = names(tab)
      ord = order(if(is_numeric(nams)) as.numeric(nams) else nams)
      tab = tab[ord]
    }
  }
  return(tab)
}

################################################################################

#' Quick tabulation
#'
#' A quicker version of table(). Assumes natural number values and every value outside 
#' 1,2,... is counted as 0.
#' @export
tablenat = function(x) {
  C = tabulate(x)
  lev = which(C>0)
  C = C[lev]
  names(C) = lev
  if(sum(C)<length(x)) C = c("0"=length(x)-sum(C), C)
  return(C)
}

################################################################################

#' Create a directory
#'
#' A verbose version of \code{dir.create}.
#' @export
dir_create = function(dir, ask=interactive(), trace=0) {
  
  if(!dir.exists(dir)) {
    
    if(ask) 
      wait("Directory '",dir,"' does not exist and it will be created ...")
    
    dir.create(dir, showWarnings=FALSE, recursive=TRUE)
    
    if(!dir.exists(dir)) 
      error("Directory '",dir,"' could not be created!")
      
  } else if(trace>0)
    cat("No need to create, directory '",dir,"' already exists.\n", sep="")
    
  return(dir)
    
}

################################################################################

#' Remove multiple directories
#' @export
dir_remove = function(dir)
  sapply(dir, unlink, recursive=TRUE)

################################################################################

#' Find the position of the last non-zero element
#' @export
last_nonzero = function(x)
  length(x) - which.max(rev(x)!=0) + 1

#' Find the position of the last positive element
#' @export
last_positive = function(x)
  length(x) - which.max(rev(x)>0) + 1

#' Find the position of the last element in 'x' with value larger than 'y'
#' @export
last_above = function(x, y)
  length(x) - which.max(rev(x)>y) + 1

#' Find the position of the first non-zero element
#' @export
first_nonzero = function(x)
  which.max(x!=0)

#' Finds the position of the first element in 'x' with value larger than 'y'
#' @export
first_above = function(x, y)
  which.max(x>y)

#' Strip non-positive elements
#' @export
positive = function(x) 
  x[!is.na(x) & x>0]

#' Strip non-negative elements
#' @export
negative = function(x) 
  x[!is.na(x) & x<0]

#' Strip elements that are not non-negative
#' @export
nonnegative = function(x) 
  x[!is.na(x) & x>=0]

#' Strip elements that are not non-position
#' @export
nonpositive = function(x) 
  x[!is.na(x) & x<=0]

################################################################################

#' Round the elements of vector x to 'nd' digits
#'
#' Rounds the elements of vector x to 'nd' digits while making sure x does 
#' not become non-unique (if it was unique to begin with)
#' @export
round2 = function(x, nd=0) {
  x_is_unique = anyDuplicated(x)==0
  for(d in seq(nd,nd+15)) {
    y = as.numeric(sprintf(paste0("%.",d,"f"), x))
    if(!x_is_unique || anyDuplicated(y)==0) break
  }
  return(y)
}

#' @export
round_nearest = function(x, b=1) {
  round(a/b)*b
}

################################################################################

#' Round numbers
#'
#' Rounds a number (can be a vector) to the nearest power of 'base'
#' @export
round_nearest_power = function(x, base=10)
  base^round(log(x,base))

#############################################################

#' Round numbers
#'
#' Rounds 'x' to 'ndigit' digits after the decimal point without losing precision
#' before the decimal point
#' @export
rsignif = function(x, ndigit=0) {
  rs = function(x, ndigit=0) {
    if(is.na(x) || is.nan(x)) return(x)
    if(abs(x)<=.Machine$double.xmin) return(x)
    if(abs(x)>=10^(ndigit-1)) return(round(x))
    ndigitd = ndigit - ceiling(log10(abs(x)))
    if(ndigitd > 300) return(x)
    y = round(x*10^max(0,ndigitd))*10^(-max(0,ndigitd))
    return(y)
  }
  return(sapply(x, rs, ndigit))
}

################################################################################

#' Round numbers
#'
#' Same as signif except it allows non-numeric input and doesn't halt on it
#' @export
signif2 = function(x, ...) 
  if(is.numeric(x)) signif(x, ...) else x

################################################################################

#' Fractions of elements
#'
#' Similar to \code{base::diff} except returns ratios instead of differences
#' @export
frac = function(x) 
  if(length(x)<=1) NA else tail(x, -1) / head(x, -1)

################################################################################

#' Check for permutation
#'
#' Checks if 'x' is a permutation of 'y'
#' @export
is_permutation = function(x, y)
  isTRUE(all.equal(sort(as.vector(x)), sort(as.vector(y))))

################################################################################

#' Generates all permutations of elements in 'x'
#' @export
all_permutations = function(x) {
  llibrary(gtools)
  if(length(x)>20) 
    warn("Length of 'x' is large, there are ",factorial(length(x))," permutations",
         " and generating them will likely take long.")
  ip = permutations(n=length(x), r=length(x))
  matrix(x[ip], ncol=length(x))
}

################################################################################

#' Get a sequence
#'
#' Produces a sequence in the same manner as \code{base::seq} except it allows
#' for input that generates and empty sequence without producing a warning
#' @export
seq2 = function(...)
  if(class(s <- try(seq(...), silent=TRUE))=="try-error") NULL else s

################################################################################

#' Every n-th element of an object
#'
#' Extracts every n-th element of an object (vector, list, possibly others)
#' starting from 'start'.
#' @examples
#' every_nth(1:10, 2)
#' every_nth(1:10, 2, 2)
#' @export
every_nth = function(x, n, start=1L)
  x[seq.int(as.integer(start), length(x), as.integer(n))]

################################################################################

#' Produce a sequence of numbers
#'
#' Produces an equidistant sequence between a and b which contains both
#' m1 and m2 of appropriate length near len. The sequence might not contain
#' the border points points a and b. Forcing them into the sequence
#' can be done via add_a and/or add_b, but the equi-distance of all points 
#' might no longer be true.
#' @export
seq_around = function(a, b, m1, m2, len, add_a=FALSE, add_b=FALSE) {
  
  stopifnot(!missing(a))
  if(length(a)>1) {
    b = max(a)
    a = min(a)
    if(missing(len) && is_integer(a) && is_integer(b)) len = b-a+1
  } else stopifnot(!missing(b))
  
  if(a > b) {
    if(len>1) error("With 'len>1' the value in 'b' cannot be smaller than value in 'a'!")
    return(a)
  }

  # Use ordinary seq if both middle points
  if(missing(m1) && missing(m2)) {
    
    x = seq(a,b,l=len)
  
  # Add only one middle point
  } else if(missing(m2) || abs(m1-m2)/abs(b-a)<1e-6) {

    if(a>m1) error("Make m1 >= a.")
    if(m1>b) error("Make m1 <= b.")
    if(m1==a || m1==b) {
      x = seq(a,b,l=len)
    } else {
      rat = (m1 - a) / (b - a)
      l1 = max(2,ceiling(len * rat))
      start_seq = seq(a,m1,l=l1)
      end_seq = seq(m1,b,by=start_seq[2]-start_seq[1])
      x = c(start_seq, end_seq[-1])
    }
  
  # Add both middle points
  } else {
  
    if(m1>m2) { tmp = m2; m2 = m1; m1 = tmp; rm(tmp) }
    if(m2>=b) error("Make both midpoints smaller than b.")
    if(a>=m1) error("Make both midpoints larger than a.")
    rat = (m2 - m1) / (b - a)
    l1 = max(2,ceiling(len * rat))
    middle_seq = seq(m1,m2,l=l1)
    start_seq = rev(seq(m1,a,by=middle_seq[1]-middle_seq[2]))
    end_seq = seq(m2,b,by=middle_seq[2]-middle_seq[1])
    x = c(start_seq, middle_seq[-1], end_seq[-1])
  
  }
  
  # Append the ends
  if(add_a && head(x,1)>a) x = c(a,x)
  if(add_b && tail(x,1)<b) x = c(x,b)
  
  return(x)
}
  
################################################################################

#timest = function(add_time=TRUE, add_pid=TRUE) {
#  stamp = ""
#  if(add_time) {
#    d = date()
#    timestamp = paste(substr(d,21,24),substr(d,5,7),substr(d,9,10),substr(d,12,13),
#                      substr(d,15,16),substr(d,18,19),sep="")
#    timestamp = sub(" ","0",timestamp)
#    stamp = paste0(stamp,timestamp)
#  }
#  if(add_pid)
#    stamp = paste0(stamp,"pid",Sys.getpid())
#  return(stamp)
#}

#' Generate a time stamp
#'
#' Generates a "unique" stamp based on the current time and process id
#' @export
#' @examples
#' timest()
#' timest(add_pid=FALSE)
#' timest(format="%Y%m%d%H%M%S")
timest = function(add_time=TRUE, add_pid=TRUE, format="%Y%b%d%H%M%S") {
  paste0(if(add_time) format(Sys.time(), format) else NULL,
         if(add_pid) paste0("pid",Sys.getpid()) else NULL)
}

################################################################################

#' Samples n variables from an arbitrary density function specified in 'pdf'
#' @export
samplepdf = function(n, pdf, ..., spdf.lower = -Inf, spdf.upper = Inf) {

  endsign = function(f, sign = 1) {
      b = sign
      while (sign * f(b) < 0) b = 10 * b
      return(b)
  }

  vpdf = function(v) sapply(v, pdf, ...)  # vectorize
  cdf = function(x) integrate(vpdf, spdf.lower, x)$value
  invcdf = function(u) {
      subcdf = function(t) cdf(t) - u
      if (spdf.lower == -Inf) 
          spdf.lower = endsign(subcdf, -1)
      if (spdf.upper == Inf) 
          spdf.upper = endsign(subcdf)
      return(uniroot(subcdf, c(spdf.lower, spdf.upper))$root)
  }
  
  sapply(runif(n), invcdf)

}

#' Check for a slash
#'
#' Check if 'dir' ends with a slash and if not it appends slash
#' @export
dir_slash_check = function(dir)
  ifelse(regexpr("[/\\]$",dir)<0, paste0(dir,"/"), dir)
  
#' Directory existence check
#'
#' Checks if directory \code{dir} exists (relative to the current working directory)
#' and if not it tries to create it. On success the value supplied in \code{dir} is returned. 
#' If directory creation fails, then an error is thrown unless \code{stop_if_fail=FALSE} 
#' was supplied, in which case the value \code{./} is returned. Verboseness is controled
#' via \code{trace} being set to 0 (no message) or otherwise.
#' @export
dir_exist_check = function(dir, stop_if_fail=TRUE, create_on_missing=TRUE, trace=1) {
  
  if(dir.exists(dir)) return(dir)
  
  if(trace>0) 
    cat0("Directory '",dir," does not exist",if(create_on_missing) " and will be created",".\n")

  if(create_on_missing) dir.create(dir, showWarnings=FALSE)

  if(!dir.exists(dir)) {
    msg = paste0("Directory '",dir,"' could not be created.")
    if(stop_if_fail) error(msg) else if(trace>0) warn(msg)
    dir = "./"
  }
  
  return(dir)
}

#' Check file existence
#'
#' Check if file exists and throws an error if it does not. A verbose version of file.exists()
#' @export
file_exists = function(files, stop_on_error=TRUE) {
  exists = file.exists(files)
  if(!all(exists))
    error("Files '",paste(files[!exists], collapse="'\n'"),"' do not exist in the path '",getwd(),"'!")
  return(invisible(exists))
}
check_file_exists = file_exists

#' Delete file
#' @export
file_remove = function(file, retry=TRUE, nretry=10) {
  for(f in file) {
    for(i in (1:(1+nretry))[c(TRUE,rep(retry, nretry))]) {
      if(file.exists(f)) {
        if(i>1) Sys.sleep(0.05*i)
        msg = tryCatch(file.remove(f), warning=function(w) return(invisible(w)))
        if(i==1+nretry && (!class(msg)=="logical" || !msg)) 
          warn("Problem when removing file '",f,"'. (Message: ", msg$message,")")
      }
    }
  }
  return(invisible(msg))
}

#' Rename multiple files
#' @export
file_rename = function(from, to, retry=TRUE) {

  if (!isTRUE(file.info(dirname(to))$isdir)) 
    dir.create(dirname(to), recursive=TRUE)

  for(i in c(1,2[retry])) {
    if(i==2) Sys.sleep(0.05)
    msg = tryCatch(file.rename(from, to), warning=function(w) return(invisible(w)))
    if(i==1 && (class(msg)!="logical" || !msg)) {
      warn("Problem when renaming file '",from,"' to '",to,"'.", skip1=0, skip2=0)
    } else break
  }
  return(invisible(msg))
  
}

#' Returns a timestamp based on the file's last modification time
#' @export
file_timestamp = function(filename, attrib='mtime') {
  times = .POSIXct(unlist(file.info(filename)[attrib]))
  names(times) = filename
  gsub("-","",gsub(":","",gsub("[ ]","",times)))
}

#' Rename a file by adding a time stamp based on the file's modification time
#' @export
file_rename_timestamp = function(filename, attrib='mtime') {
  newname = paste0(filename,"_",file_timestamp(filename))
  file.rename(filename, newname)
}

#' Move multiple files
#'
#' @export
file_move = function(files, destination, strip.dir=TRUE)
  sapply(files, function(f) file_rename(f, paste0(destination,"/", if(strip.dir) sub(".*[/\\]","",f) else f)))

#' Sort files names
#'
#' Sorts files in 'files' by time or size in descending/ascending order
#' @export
file_sort = function(files, by=c("time","name","isdir","mode","mtime","ctime","atime","exe"), 
                     decreasing=switch(by, time=, mtime=, ctime=, atime=TRUE, FALSE),  nget=NULL) {
  if(!length(files)) return(files)
  if(!length(by)) error("Missing value in 'by'!")
  by = by[1]
  if(by=="time") by = "ctime"
  if(by=="name") {
    f = sort(files, decreasing=decreasing)
  } else {
    Info = file.info(files)
    if(all(colnames(Info)!=by)) 
      error("Unknown sorting attribute '",by,"' in file_sort()!")
    f = files[order(Info[,by], decreasing=decreasing)]
  }
  if(!is.null(nget)) f = head(f, nget)
  return(f)
}

#' Sort file names
#'
#' Sorts file names in in 'files' by attribute in 'by' (by default modification time)
#' @export
file_sort_time = function(files, by="mtime", decreasing=TRUE) {
  if(!length(files)) return(files)
  files[order(file.info(files)[,"ctime"], decreasing=decreasing)]
}

#' Check file existence and emptiness
#'
#' Checks if file exists, tries to read it to see whether there is any data in it
#' @export
file_empty = function(file) {
  file_exists(file)
  res = try(read.table(file, nrow=1), silent=TRUE)
  return(class(res)=="try-error")
}

#' Get file sizes
#' 
#' Returns the sizes of files in 'files' in appropriate units
#' @export
file_size = function(files) {
  convert_unit(file.info(files)[,"size"])
}

#' Check if file can be open
#' @export
file_can_open_check = function(filename) {
  do_nothing = function(x) invokeRestart("muffleWarning")
  zz = withCallingHandlers(try(close(file(filename, open="ab")), silent=TRUE), warning=do_nothing)
  all(class(zz) != "try-error")
}

################################################################################

#' Value replacement
#'
#' Replaces the values in \code{x} with \code{new_values} either in the order of 
#' occurrence if \code{old_values} is missing, or using \code{old_values} for reference.
#' @export
replace2 = function(x, new_values, old_values, is_pattern=TRUE, number_new_values=FALSE) {
  
  if(missing(old_values)) {
    y = as.factor(x)
    stopifnot(nlevels(y)==length(new_values))
    levels(y) = new_values
    if(any(class(new_values)==c("integer","numeric"))) {
      y = as.numeric(levels(y))[y]
    } else if(class(new_values) == "character") {
      y = as.character(y)
    } else {
      y = as(as.character(y), class(new_values))
    }
  } else {
    y = x
    stopifnot(length(new_values)==length(old_values)) 
    f = if(is_pattern) regexpr else `==`
    idx = lapply(old_values, function(v) which(f(v, x)>0))
    len = sapply(idx, length)
    val = rep(new_values, len)
    if(number_new_values) val = paste0(val, unlist(sapply(len, seq)))
    y[unlist(idx)] = val
  }
  
  return(y)
}

#replace2 = plyr::revalue

################################################################################

# #' Repeat
# #'
# #' An upgraded version of \code{rep}. Allows for different number of occurrences 
# #' for each value supplied in the first argument.
# #' @export
# #' @examples
# #' rep2(c('a','b'),1:2)
# rep2 = function(...) {
  
  # d = list(...)
  
  # if(length(d)==2) {
    # x = d[[1]]
    # y = d[[2]]
    
    # if(any(y<0)) error("All values in the second argument must be non-negative.")
    
    # is_each = !is.null(names(d)[2]) && regexpr("^e.*$",names(d)[2])>0
    
    # if(is_each && !is.null(y) && length(y)>1 && length(y)==length(x)) {
      # unlist(sapply(1:length(y), function(i) rep(x[i], t=y[i])))
    # } else if(is_each && length(y)==0) {
      # numeric(0)
    # } else {
      # rep(...)
    # }
    
  # } else rep(...)

# }

################################################################################

#' Repeat
#'
#' An upgraded version of \code{rep}. Allows for different number of occurrences 
#' for each value supplied in the first argument.
#' @export
#' @examples
#' rep2(c('a','b'), e=3:4)
rep2 = function(x, each, ...) {
  
  if(missing(each)) {
    rep(...)
  } else if(length(each)==0) {
    numeric(0)
  } else if(length(each)>1 && length(each)==length(x)) {
    unlist(sapply(seq_along(each), function(i) rep(x[i], t=each[i])))
  } else {
    rep(..., each=each)
  }  

}

rep2b = function(...) {
  
  d = list(...)
  
  if(length(d)==2) {
    
    eaches = c("each","eac","ea","e")
    w_each = head(which(eaches %in% names(d)),1)
    is_each = length(w_each)==1
    
    if(!is_each) {
      rep(...)
    } else {
    
      each = d[[eaches[w_each]]]
      x = d[-which(names(d) == eaches[w_each])][[1]]
    
      if(length(each)==0) {
        numeric(0)
      } else if(length(each)>1 && length(each)==length(x)) {
        unlist(sapply(seq_along(each), function(i) rep(x[i], t=each[i])))
      } else
        rep(...)
    
    }
    
  } else rep(...)

}

################################################################################

#' Get normal distribution p-value
#'
#' Returns the p-value relative to the normal distribution with parameters m, s
#' @export
normpval = function(x, two.sided=FALSE, m=0, s=1)
 if(two.sided) 2*pnorm(abs(x), m=m, s=s, lower.tail=FALSE) else pnorm(x, m=m, s=s, lower.tail=FALSE)

################################################################################

#' Fix LaTeX Bibliography
#'
#' Puts authors' names into 'Lastname1, Firstname1 and Lastname2, Firstname2'
#' format in a bibtex bibliography file in 'infile', abbreviates the first 
#' names with/without a dot and outputs the new bibliography into outfile
#' Assumes that records are wrapped by '{' and '}' and not '"'. 
#' (Consider removing this limitation)
#' @export
fix_bibliography = function(infile, outfile, dot=".", lastname_first=TRUE,
  special_words="^[Vv]an$|^[Dd]e$|^[Dd]er$|^[Dd]os$") {

  # Read the bibliography
  cat("Reading bibliography from file '",infile,"' ...\n", sep="")
  x = scan(infile, what='character', sep='\n', blank.lines.skip=FALSE)

  # Loop through the author lines to process them
  cat("Processing bibliography ...\n")
  for(k in seq_along(x)) {

    if(regexpr("author.=", x[k])<0) next
  
    ## Extract the names only
    # Find the record opening opening 
    b1 = strpos(x[k], "{", first=TRUE)
    if(b1<0) stop("Could not locate '{' for line ",k,": ",x[k])
    b2 = strpos(x[k], "}", last=TRUE)
    if(b2<0) stop("Could not locate '}' for line ",k,": ",x[k])
    s1 = substr(x[k], 1, b1-1)
    s2 = substr(x[k], b1+1, b2-1)
    s3 = substr(x[k], b2+1, nchar(x[k]))
    
    ## An empty author found
    if(b1+1 > b2-1) {
      note("An empty author record found on line ",k,".")
      next
    }
    
    # Separate the names on 'and'
    n = unlist(strsplit(s2, " [Aa]nd "))
    
    # Check if author name contains "LaTeX protected strings"
    if(regexpr("[{}]",s2)>0) {
      cat("Check if manual edit for author '",s2,"' required (line ",k,").\n")
      next
    }
    
    # Go through the names and abbreviate the first names
    for(i in seq_along(n)) {
      
      # If in "last name comma first names" format, transform it
      if(regexpr("[,]",n[i])>0) {
        a = unlist(strsplit(n[i], "[,]"))
        n[i] = paste(str_trim(tail(a,-1)), str_trim(head(a,1)))
      }
      
      # Abbreviate first names
      a = unlist(strsplit(n[i], "[ .]"))
      a = a[nchar(a)>0]
      for(j in seq2(1,(length(a)-1),1))
        a[j] = if(regexpr(special_words,a[j])<=0) paste0(substr(a[j],1,1),dot) else paste0(" ",a[j])
      
      # Paste the names back together
      if(lastname_first) {
        a = str_trim(a)
        split = length(a) - 1
        while(split>0) {
          if(regexpr(special_words,a[split])<0) break
          split = split - 1
        }
        n[i] = if(split==0) paste(a, collapse=" ") else 
          paste0(paste(tail(a,-split), collapse=" "), ", ", paste(head(a, split), collapse=" "))
      } else {
        n[i] = paste(paste(head(a, -1), collapse=""), tail(a,1))
      }
    }
    
    # Paste the author line back together
    x[k] = paste0(s1,"{",paste(n, collapse=" and "),"},")
    
  }

  # Update the bibliography and save it to a file
  cat("Saving bibliography to file '",outfile,"' ...\n", sep="")
  cat(paste(x, collapse="\n"), file=outfile)
  
}

#############################################################

#' Get autocorrelation
#'
#' Calculates autocorrelation of x with given lag
#' @export
acor = function(x, lag=1) {
  suppressWarnings(cor(x[-((length(x)-lag+1):length(x))], x[-(1:(lag))]))
}

#############################################################

#' Get ASCII code
#'
#' Returns an ASCII table code of a character in 'x'
#' @export
asc = function(x) {
  strtoi(charToRaw(x), 16L)
}

################################################################################

#############################################################

#' Generate a vector of colors
#'
#' Generate a vector of colors based on difference of values in the supplied vector
#' @export
#' @examples
#' x = c('a','b','a','c'); as_color(x)
as_color = function(x)
  rainbow(length(unique(x)))[as.numeric(as.factor(x))]

#assign("cat", cat0, envir=.GlobalEnv)
#assign("cat", cat0, envir=.utilbox)

#cat = utils::getFromNamespace("cat", ns="base")
#trace(cat, tracer=quote(if(missing(sep)) sep <- ""), at=1, print=FALSE)
##utils::assignInNamespace("cat", cat0, ns="base")

########################################################################

#' Return a list of colors
#'
#' Returns a list of \code{n} interesting and contrasting colors. Up to 20 unique
#' colors. The colors are recycled for \code{n} above 20.
#' @export
Colors = function(n=20)
  h1(c("#074263", "#0B5394", "#3D85C6", "#6D9EEB", "#A4C2F4", "#CFE2F3", "#5B0F00", 
    "#85200C", "#A61C00", "#CC4125", "#DD7E6B", "#E6B8AF", "#F8CBAD", "#F4CCCC", 
    "#274E13", "#38761D", "#E06666", "#CC0000", "#20124D", "#492EEE"), n)

########################################################################

#' P-value combination
#'
#' Combines p-values using one of the available methods 
#' @export
combine_pvalues = function(P, lam, method=c("jelle","jakub","fisher","stouffer"), summarize=FALSE, ...) {

  if(is.vector(P)) P = matrix(P, nrow=1, ncol=length(P))

  if(summarize) {
    cat0("Smallest raw p-value: ",min(P),"\n")
    w = which.min(apply(log10(P), 1, mean))
    cat0("Row with the smallest average p-values (on log scale): ",w,"\n")
    print(P[w,])
    c1 = do_pval_comb(P[w,], method="fisher")
    cat0("Fisher combination of these p-values: ",c1,"\n")
    c2 = do_pval_comb(P[w,], method="jelle")
    cat0("Jelle combination of these p-values: ",c2,"\n")
  }
  
  do_pval_comb(P, lam, method, ...)
  
}

#########################################################################

#' P-value combination using Fisher method
#'
#' Combines p-values using the Fisher combination methods 
#' @export
combine_fisher = function(p, silent=TRUE)
  do_pval_comb(p, method="fisher", trace=1*!silent)

#########################################################################

#' P-value combination using Fisher method
#'
#' Combines p-values using the Stouffer's combination methods 
#' @export
combine_stouffer = function(p, silent=TRUE)
  do_pval_comb(p, method="stouffer", trace=1*!silent)

#########################################################################

#=======================================================================#
# Code by Jelle Goeman. Calculates the asymptotic p-value using methods #
# of Kotz, Johnson and Boyd (1967) and Box (1954)                       #
#=======================================================================#

.pAsymptotic = function(x, lams, bet, accuracy=1e-12) {

  m = length(lams)

  if (lams[1] == 0) {
    upper = 1.0
  } else {
    if (m == 1) {
      upper = pchisq(x / lams, df = 1, lower.tail = FALSE)
    } else {
      # get the tuning parameter beta
      if (missing(bet)) {
        lams = sort(lams)
        ruben = 2 * lams[1] * lams[m] / (lams[1] + lams[m])
        harmonic = 1/mean(1/lams)
        bet = min(ruben, harmonic) * (1. - 1e-15)
      }
      # get an upper bound to the number of iterations needed
      A = qnorm(.Machine$double.neg.eps)^2
      B = x/bet
      maxiter = trunc(0.5 * (A + B + sqrt(A*A + 2*A*B) - m))
      # starting values
      #maxiter = sum(maxiter)
      d = numeric(maxiter)
      c = numeric(maxiter+1)
      c[1] = prod(sqrt(bet / lams))
      sumc = c[1]
      chi = pchisq(x / bet, df = m, lower.tail = FALSE)
      partialsum = c[1] * chi
      dbase = (1 - bet /lams)
      ready = FALSE
      mixture = TRUE
      ix = 1
      # iterate!
      while (!ready) {
        d[ix] = 0.5 * sum(dbase^ix)
        c[ix+1] = mean(c[1:ix] * d[ix:1])
        if (c[ix+1] < 0)
          mixture = FALSE
        sumc = sumc + c[ix+1]
        partialsum = partialsum + c[ix+1] * chi
        chi = pchisq(x / bet, df = m + 2 * ix + 2, lower.tail = FALSE)
        lower = partialsum + (1.0 - sumc) * chi
        upper = partialsum + 1.0 - sumc
        if (mixture)
          ready = ((upper - lower) / (upper + lower) < 10^-5) || 
                   (ix == maxiter) || (upper < accuracy)
        else {
          ready = TRUE
          upper = .pAsymptotic(x, lams, mean(c(bet, min(lams))))
        }
        ix = ix + 1
      }
    }
  }
  
  if (upper < accuracy) upper = accuracy / 10.0
  
  return(upper)
}

#########################################################################

#' Quantile function of a weighted chi-square distribution
#'
#' Returns the quantile of a distribution that arises when independent
#' chisquare-distributed variables with \code{df0} degrees of freedom
#' are summed on the logarithmic scale with weights in \code{w}
#' @export
qchisqw = function(p, weights, lower.tail=TRUE, epsw=1e-5, epsv=1e-12, df0=2) {

  if(any(weights<0)) error("Negative weights!")
  
  # Check the input
  np = length(p)
  if(np==1) {
    weights = matrix(weights, nrow=1)
  } else if(is.vector(weights)) {
    weights = matrix(rep(weights, e=np), nrow=np)
  }
  stopifnot(is.matrix(weights), nrow(weights)==np)
  
  # Get the chisquare quantile
  q0 = qchisq(p, df=df0*ncol(weights), lower.tail=FALSE) * apply(t(weights), 2, mean)
  
  # If weights are different, use pchisqw
  if(diff(range(weights)) < epsw) {
    q0
  } else {
    logp = log(p)
    fun = function(x) (log(pchisqw(x, weights=weights, lower.tail=lower.tail, epsw=epsw, df0=df0)) - logp)^2
    value = 1
    iter = 0
    max_iter = 10
    while(value>epsv && iter<max_iter) {
      iter = iter + 1
      x = optim(q0, fun, method="Brent", lower=0, upper=5*q0)
    }
    if(x$value>epsv) error("Non-convergent optimization in qchisqw() (value ",x$value,")!")
    x$par
  }
  
}

#########################################################################

#' Get the probability of a weighted chi-square sum
#'
#' Calculates the probability of a weighted sum of chisquare-df0 variables
#' being below (lower.tail==TRUE) or above (lower.tail=FALSE) x, where
#' weights specifies the weights. Fails if some (but not all) weights are equal
#' @export
pchisqw = function(x, weights, lower.tail=TRUE, epsw=1e-5, df0=2) {

  nx = length(x)
  if(nx==1) {
    weights = matrix(weights, nrow=1)
  } else if(is.vector(weights)) {
    weights = matrix(rep(weights, e=nx), nrow=nx)
  }
  stopifnot(is.matrix(weights), nrow(weights)==nx)
  
  # If all equal, use standard chisquare distribution function
  if(diff(range(weights))<epsw) {
  
    p = pchisq(x, df=df0 * ncol(weights), lower.tail=lower.tail)
  
  # Otherwise use Box (1953), Theorem 2.4
  } else {

    # Calculate the probability of exceeding x
    wx = x / weights
    px = pchisq(wx, df=df0, lower.tail=FALSE)
    a = sapply(1:nx, function(i) 
          sapply(1:ncol(weights), function(j) prod(weights[i,j]/(weights[i,j]-weights[i,-j]))))
    p = apply(a * t(px), 2, sum)
    
    # Get the lower tail probability (i.e. probability of not exceeding x)
    if(lower.tail) p = 1. - p
    
  }
  
  return(p)
  
}      

#########################################################################

#' Combine p-values
#'
#' Performs combination of p-values using Fisher method or Box method (the
#' latter via Jelle's code in .pAsymptotic or Jakub's code below
#' @export
do_pval_comb = function(P, lam=1, method="jelle", eps_p=1e-3, fac_lam=1e-2, 
  Debug=FALSE, na.rm=TRUE, trace=1) {

  # Check for unknown combination method
  if(all(method!=c("fisher","stouffer","jakub","jelle")))
    error("Unknown combination method '",method,"'!")
  
  # Replace non-sensical p-values with NA
  if(trace>0) cat0("Checking for NA values ...\n")
  P[P<0. | P>1.] = NA

  # Make sure P is a matrix
  if(is.vector(P)) P = matrix(P, nrow=1, ncol=length(P))
  
  # Store dimensions of P
  nres = ncol(P)
  np = nrow(P)
  
  # Make sure no weighing is done when lam=1
  if(length(lam)==1 && lam==1) lam = rep(lam, ncol(P))

  if(is.vector(lam)) {
    same_lam = TRUE
    mat_lam = matrix(lam, nrow=np, ncol=nres, byrow=TRUE)
  } else {
    same_lam = FALSE
    mat_lam = lam
  }
  
  # Check problems with P and lambda
  if(any(dim(mat_lam)!=dim(P)))
    stop("Incompatible sizes of lam and P!")
  if(all(lam==0)) stop("Some values in lam must be non-zero!")
  
  # Transform p-values to chisquare-2 variables
  if(trace>0) cat("Getting the logarithm of the p-values ...\n")
  if(any(method==c("fisher","jakub","jelle"))) chi2 = -2*log(P)
  
  # Combine using Fisher's method
  if(tolower(method)=="fisher") {
    
    if(trace>0) cat("Combining p-values (",nrow(chi2)," sets) via Fisher's method (no weighing) ...\n", sep="")
    X0 = apply(chi2, 1, sum, na.rm=na.rm)
    nres = apply(!is.na(chi2), 1, sum)
    PP = pchisq(X0, df=2*nres, lower.tail=FALSE)
  
  } else if(tolower(method)=="stouffer") {
    
    if(trace>0) cat("Combining p-values (",nrow(chi2)," sets) via Stouffer's z-score method (with weighing) ...\n", sep="")
    Z = qnorm(P, lower.tail=FALSE)
    X0 = apply(mat_lam*Z, 1, sum, na.rm=na.rm) / sqrt(apply(mat_lam^2, 1, sum))
    PP = pnorm(X0, lower.tail=FALSE)
  
  # Combine using Box's method
  } else {

    if(missing(lam)) stop("Missing lam in do_pval_comb()!")

    # Define lam as matrix and calculate the score
    X0 = apply(mat_lam*chi2, 1, sum, na.rm=na.rm)
    
    if(trace>0) 
      cat0("Combining p-values (",nrow(chi2)," sets) via Box's method (with weighing, ",method,"'s code) ...\n")
     
    # Use Jelle's general implementation (slow)
    if(tolower(method)=="jelle") {

      PP = sapply(1:np, function(i) .pAsymptotic(X0[i], lams=rep(mat_lam[i,], t=2), accuracy=1e-100))

    # Use Jakub's less general implementation (fast)
    } else {
      
      # If weights are too close, randomize them
      mindif = 0 #.005*diff(range(lam))
      mdif = apply(mat_lam, 1, min_dif)
      #print(summary(mdif))
      if(any(mdif <= mindif)) {
        if(trace>0) cat0("Randomizing weights ...\n")
        if(same_lam) {
          rlam = randomize(lam, mindif=mindif)
          mat_lam = matrix(lam, nrow=np, ncol=nres, byrow=TRUE)      
        } else {
          for(i in 1:np) {
            if(mdif[i]>mindif) next
            if((i==1 || i%%100==0) && trace>0) cat("Randomizing p-value ",i," out of ",np," ...\n",sep="")
            if(Debug) {
              print("--------------------------")
              print(min_dif(mat_lam[i,]))
            }
            mat_lam[i,] = randomize(mat_lam[i,], mindif=mindif)
            if(Debug) 
              print(min_dif(mat_lam[i,]))
          }
          #mat_lam = apply(mat_lam, 1, randomize, mindif=mindif)
        }
      }

      # Check for zeros (which there really should not be at this point)
      mdif = apply(mat_lam, 1, min_dif)
      if(any(mdif==0))
        error("Some weights are equal, which is not allowed with Jakub's code.", Q=!interactive())
      
      Q = X0 / mat_lam
      PQ = pchisq(Q, df=2, lower.tail=FALSE)

      a = if(same_lam) {
        sapply(1:nres, function(j) prod(lam[j]/(lam[j]-lam[-j])))
      } else {
        sapply(1:np, function(i) sapply(1:nres, function(j) prod(mat_lam[i,j]/(mat_lam[i,j]-mat_lam[i,-j]))))
      }
      
      PP = apply(a * t(PQ), 2, sum)

      if(Debug) {
        print("summary(lam)"); print(summary(as.vector(lam)))
        print("summary(a)"); print(summary(as.vector(a)))
        print("summary(Q)"); print(summary(as.vector(Q)))
        print("summary(PP)"); print(summary(as.vector(PP)))
        print("min(PQ)"); print(min(PQ, na.rm=TRUE))
        print("min(PP)"); print(min(PP, na.rm=TRUE))
      }
    
    } # if(method=="jakub")

    if(any(is.na(PP) & !is.na(X0)))
      error("Non-NA input p-values were combined into NA p-values!", Q=!interactive())
    if(any(!is.na(PP)) && min(PP, na.rm=TRUE)<0)
      error("Some p-values are negative!", Q=!interactive())
      
  }

  # If any p-values are negative, correct them using Jelle's code
  if(any(PP<0.0, na.rm=TRUE)) {
    wn = which(PP<0.0)
    if(!exists(".pAsymptotic", mode="function"))
      error("Missing function .pAsymptotic()! Look for Jelle's script that",
            " defines the function.")
    PP[wn] = sapply(seq_along(wn), function(i) 
               .pAsymptotic(X0[wn[i]], lams=rep(mat_lam[i,], t=2), accuracy=1e-100))
  }
  
  # Check for PP above 1 and trim them if they are very close
  if(any(PP>1.0, na.rm=TRUE))
    PP[PP>1.0 & PP<1+eps_p] = 1.0

  # Check again for PP above 1 even after correction
  if(any(PP>1.0, na.rm=TRUE)) { 
    cat("\nERROR: SOME P-VALUES ABOVE 1!\n\n")
    cat("Maximum p-value: ", max(PP), "\n", sep="")
    print(summary(PP))
    if(interactive()) browser() else stop()
  }

  # Announce the smallest combined p-value
  if(trace>0) cat0("Smallest combined p-value: ",min(PP, na.rm=TRUE),"\n")
  
  # Compare the output of Jakub's and Jelle's codes for small p-values
  eps = 1e-6
  if(FALSE && any(PP<eps, na.rm=TRUE)) {
    cat0("Checking results for small p-values (",sum(PP<eps)," p-values below ",eps,") ...\n")
    ws = which(PP<eps)
    PP2 = sapply(X0[ws], .pAsymptotic, lams=rep(lam, t=2), accuracy=1e-100)
    cat0("Smallest p-value found after re-check: ",min(PP2),"\n")
    md = max(abs(PP[ws]-PP2))
    cat0("Maximum difference between Jakub's and Jelle's code: ",md,"\n")
    mrd = max(abs((PP[ws]-PP2)/unzero(PP[ws]+PP2)*2-1))
    cat0("Maximum relative difference between Jakub's and Jelle's code: ",mrd,"\n")
    PP[ws] = PP2
  }

  # If there are still negative or NA p-values, stop
  if(any(PP<0.0, na.rm=TRUE))
    error("Some p-values are negative!", Q=!interactive())
  if(any(is.na(PP) & !is.na(X0)))
    error("Non-NA input p-values were combined into NA p-values!", Q=!interactive())

  return(PP)

}

########################################################################

#' Bootstrap
#'
#' Returns a vector of B bootstrap values of real-valued statistic T, where T 
#' should be an R-function which returns a scalar. Arguments of T can be supplied
#' via the ellipsis '...'. If length of x and B are small enough, do it all at once
#' Otherwise, do it for each iteration separately.
#' @export
bootstrap = function(x, T, B = 100., ..., portion=1e8) {
  if(length(x)*B < portion) {
    X = matrix(sample(x, length(x)*B, replace = TRUE), nrow=length(x))
    apply(X, 2, T, ...)
  } else {
    sapply(1:B, function(i) T(sample(x, replace = TRUE), ...))
  }
  
}

################################################################################

#' Empirical probability frequency (density)
#'
#' @export
#' @examples 
#' ecf(sample(1:3, 100, repl=TRUE))
edf = function(x) {
  y = c(table(x))
  y / sum(y)
}
  
################################################################################

#' Convert to base k
#' @export
to_base_k = function(x, k=3) {
  c(if(x>=k) to_base_k(x %/% k, k) else NULL, x %% k)
}
  
#' Convert to base k (vectorized version)
#' @export
int2k = Vectorize(to_base_k)

################################################################################

#' Recursive string substitution
#'
#' Allows for multiple arguments in \code{what} and \code{with} which are replaced
#' inside \code{where}. \code{what} and \code{with} are processed in parallel which
#' means that they must have the same \code{length}.
#'
#' @export
#' @examples
#' require(magrittr)
#' c("a",'b') %>% subr(., paste0("|",.,"|"), "abbada")
subr = function(what, with, where) {
  stopifnot(identical(length(what), length(with)))
  if(length(what)==0) return(where)
  where %<>% sub(what[1], with[1], .)
  subr(what[-1], with[-1], where)
}

################################################################################

#' Add tight axis labels to a plot
#' @export
add_tight_axes = function(xlab=NULL, ylab=NULL, linex=1.4, liney=1.47, padjx=-0.85, 
  padjy=0.85, tclx=-0.3, tcly=-0.3, cex=NULL, cexx=1, cexy=1, cexfaclabx=1, cexfaclaby=1, 
  xlim=c(-Inf,Inf), ylim=c(-Inf,Inf), scipenx, scipeny, scipen0, ylabfun, xlabfun,
  do_x=TRUE, do_y=TRUE) {
  
  if(missing(cexx) && !missing(cex)) cexx = cex
  if(missing(cexy) && !missing(cex)) cexy = cex

  if(!missing(scipenx)) options(scipen=scipenx)
  xt = axTicks(side=1)
  xt = xt[xlim[1]<=xt & xt<=xlim[2]]
  xtl = if(!missing(xlabfun)) xlabfun(xt) else xt
  if(do_x) axis(side=1, at=xt, lab=xtl, line=0, padj=padjx, tcl=tclx, cex.axis=cexx)
  
  if(!missing(scipeny)) options(scipen=scipeny)
  yt = axTicks(side=2)
  yt = yt[xlim[1]<=yt & yt<=ylim[2]]
  ytl = if(!missing(ylabfun)) ylabfun(yt) else yt
  if(do_y) axis(side=2, at=yt, lab=ytl, line=0, padj=padjy, tcl=tcly, cex.axis=cexy)
  
  if(!missing(scipen0)) options(scipen=scipen0)
  mtext(xlab, side=1, line=linex, cex=cexfaclabx*cexx)
  mtext(ylab, side=2, line=liney, cex=cexfaclaby*cexy)
	
  box()
  
  return(invisible(NULL))
}

#' Add a box above a plot
#' @export
add_box_above = function(nboxes=0, labels=NULL, hfr=0.07, hsp=0.01, hfac=1, base=0, 
                         col="black", bg=grey(0.35), cex=1, offset=0, logscale="") {
  
  # Check for an unopen device
  if(dev.cur()==1) {
    warn("Cannot add box because no device seems to be open.")
    return(invisible(NULL))
  }
  
  if(length(hfr)==1) hfr = rep(hfr, nboxes)
  if(length(hsp)==1) hsp = rep(hsp, nboxes)
  if(length(bg)==1) bg = rep(bg, nboxes)
  if(length(col)==1) col = rep(col, nboxes)
  if(length(cex)==1) cex = rep(cex, nboxes)
  if(length(offset)==1) offset = rep(offset, nboxes)
  
  # Calculate position of the box
  xy = par()$usr
  hght = diff(xy[3:4]) / hfac^1.5
  
  if(class(labels)=="call") labels = list(labels)
  
  par(xpd=NA)
  yt = base + xy[4]
  for(i in 1:nboxes) {
  
    yb = yt + hsp[i]*hght
    yt = yb + hfr[i]*hght
    if(regexpr("x", logscale)>0) xy[1:2] = 10^xy[1:2]
    if(regexpr("y", logscale)>0) {
      yb = 10^yb
      yt = 10^yt
    }

    # Plot the box
    rect(xleft=xy[1], xright=xy[2], ybottom=yb, ytop=yt, col=bg[i])
    
    l = 0.85
    if(length(labels)>=i) 
      text(x=mean(xy[1:2]), y=l*yb+(1-l)*yt, lab=as.expression(labels[[i]]), pos=3, col=col[i], 
           cex=cex[i], offset=offset[i])
    
  }
  par(xpd=FALSE)
  
  return(invisible(NULL))
}
