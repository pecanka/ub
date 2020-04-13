#source("d:/Dropbox/Projects/R/utilitybox.R")

################################################################################
########################### DEFINITION OF FUNCTIONS ############################
################################################################################

# This script is meant to be sourced into R (ideally at launch) from within the
# global environment (.GlobalEnv) to define various utility functions. When the
# script is sourced inside the global environment, after changing the R local 
# library path and alters several options via options(), it creates the environment
# '.env' and it recursively sources itself within the environment '.env', which 
# executes the bottom portion of the code (where utility functions are defined)
# inside '.env'. After '.env' is populated with functions, it is attached.

populate_env = function() {

  # Set high penalization for scientific notation, turn off reading of
  # strings from files as factors, set the default repository
  options(scipen=5)
  options(stringsAsFactors=FALSE)
  options("repos"=c(CRAN="https://cran.rstudio.com/"))

  #.this_file = if(all(names(sys.frame(1))!="ofile")) NULL else sys.frame(1)$ofile
  calls = sapply(sys.calls(), function(cl) as.character(cl[2]))
  w = regexpr("utilitybox.R", calls)>0
  this_file_full_name = if(any(w)) calls[w] else return(FALSE)

  # Populate the environment '.env.' with the utility functions defined below
  cat("Populating environment '.env' ...\n", sep="")
  sys.source(this_file_full_name, envir=get(".env", envir=.GlobalEnv))

  ## Attach the environment which was populated with the functions defined below
  while(any(search()==".env")) detach(.env)
  cat("Attaching environment '.env' ...\n", sep="")
  attach(.env)

}

## Create a new invisible environment for all the functions to go in so it doesn't clutter 
## the workspace (i.e. .GlobalEnv).
if(!exists(".env", envir=.GlobalEnv) || !is.environment(get(".env", envir=.GlobalEnv))) 
  assign(".env", new.env(), envir=.GlobalEnv)
 
## Make sure the environment that is about to be populated is empty
eval(rm(list=ls(all=TRUE)), envir=get(".env", envir=.GlobalEnv))
  
## If this code is not being evaluated inside the environment '.env', set it up for such 
## evaluation and call evaluate (i.e. source) it inside '.env' (see sys.source() call below)
if(!identical(environment(), get(".env", envir=.GlobalEnv))) populate_env() else {

################################################################################

script_dir = function()
  if(all(names(sys.frame(1))!="ofile")) "." else dirname(sys.frame(1)$ofile)

################################################################################

## Quits without saving or asking to save
.q = function(save="no") base::q(save=save)

################################################################################

## Logical negation of %in%, i.e. returns a logical vector TRUE for elements of 
## first argument that are not in the second argument and vice-versa
'%notin%' = '%nin%' = Negate('%in%')

######################################################################################

## Concatenation operator (i.e. an operator equivalent to paste0)
'%.%' = function(a, b) paste0(a, b)

######################################################################################

# Version of cat that automatically flushes the console if 'flush' is TRUE (this
# affects buffered output enabled R sessions (such as Rgui on Windows) only
cat0 = function(..., sep="", flush=TRUE, flush_cycle=1) {
  
  # Print the text
  base::cat(..., sep=sep)
  
  # Retrieve the environment '.env'
  env = get(".env", envir=.GlobalEnv)

  # Define a counter of prints since the last flush
  if(!exists(".flush_counter", env=env)) assign(".flush_counter", 0, env=env)
  
  # Increase the counter by 1
  assign(".flush_counter", get(".flush_counter", env=env) + 1, env=env)
  
  # If either flush is true or the counter has reached the limit, flush the console
  if(flush || get(".flush_counter", env=env)>=flush_cycle) {
    utils::flush.console()
    assign(".flush_counter", 0, env=env)
  }
  
}

################################################################################

## Just go ahead and make the 'sep' in base::cat equal to an empty string
formals(cat)$sep <- ""
#trace(base::cat, tracer=quote(if(missing(sep)) sep=''), at=1)
#cat = partial(cat, sep="")
#mean = partial(mean, na.rm=TRUE)
#cat = cat0

################################################################################

## Version of cat0 which also prints end-of-line symbol
catn = function(...) cat0(...,"\n")

################################################################################

## Short hand for flush.console()
.fc = fc = utils::flush.console

################################################################################

## An "empty" function
void = function(x) return(invisible(0))

################################################################################

## Shorthand for removal of all, i.e. for rm(list=ls())
.rma = function(all.names=FALSE, keep=".env")
  rm(list=setdiff(ls(envir=.GlobalEnv, all.names=all.names), keep), envir=.GlobalEnv)

################################################################################

## Deletes all elements in x but checks if they exist (in pos/envir) first
rme = function(..., pos=-1, envir=as.environment(pos)) {
  rme1 = function(y) 
    if(length(ls(envir=envir, pattern=paste0("^",y,"$")))>0) rm(list=y, envir=envir)
  objs = as.character(match.call(expand.dots = FALSE)$`...`)
  invisible(sapply(objs, rme1))
}

################################################################################

## A function that allows for changing of default values for arguments of other functions
hijack = function (FUN, ...) {
  .FUN <- FUN
  args <- list(...)
  invisible(lapply(seq_along(args), function(i) {
    formals(.FUN)[[names(args)[i]]] <<- args[[i]]
  }))
  .FUN
}

################################################################################

is_same_object = function(a, b) {
  a = as.character(substitute(a))
  b = as.character(substitute(b))
  env = parent.frame(1)
  identical(do.call("tracemem", list(get(a, envir=env))),
            do.call("tracemem", list(get(b, envir=env))),)
}

################################################################################

## Define a concatenation operator (i.e. an operator equivalent to paste0
'%.%' = function(a, b) 
  paste0(a, b)

######################################################################################

## Define a concatenation operator which doesn't evaluate names
'%.%' = function(a, b) 
  paste0(as.character(substitute(a)), as.character(substitute(a)))

######################################################################################

## Function which changes name of a variable by reassigning 'from' to 'to' and
## removing the object 'from'
.var_rename_right = function(from, to) {
  from = as.character(substitute(from))
  to = as.character(substitute(to))
  if(identical(from, to)) return(invisible(-1))
  env = parent.frame(1)
  assign(to, get(from, envir=env), envir=env)
  rm(list=from, envir=env)
  return(invisible(0))
}

## Create an alias which takes input in reverse order
.var_rename_left = function(to, from) { }
body(.var_rename_left) = body(.var_rename_right)

## An operator version of the above functions
"%->%" = .var_rename_right
"%<-%" = .var_rename_left

################################################################################

## Pauses the execution until the user presses ENTER (for continue) or ESC (for quit)
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

## Sets options()$warn to either 0 or 2, which makes R either stop when a warning
## is issued (>=2) or not (0). If argument 'turn_on' is missing, the function acts
## as a toggle turning stopping on warnings on and off on alternative calls.
.sow = function(turn_on, announce=TRUE) {
  if(missing(turn_on)) 
    turn_on = options()$warn<2
  options(warn=ifelse(turn_on, 2, 0))
  if(announce) 
    note("Stopping on warnings has been ",ifelse(turn_on,"ENABLED","DISABLED"),".")
}

################################################################################

## Switch recovering on error ON/OFF (same logic as .sow())
.roe = function(turn_on, announce=TRUE) {
  if(missing(turn_on)) 
    turn_on = is.null(options()$error)
  options(error=if(turn_on) recover else NULL)
  if(announce) 
    note("Recovery on error has been ",ifelse(turn_on,"ENABLED","DISABLED"),".")
}


################################################################################

## A different way of stopping. Intended to be called by halt but the behavior
## wasn't as hoped/expected. Abandoned for now. 
.halt = function() {
  error = simpleError("")
  class(error) = c("myerror", class(error))
  signalCondition(error)
}

################################################################################

## Prints an error message and stops differently for interactive/non-interactive modes
halt = function(error="") {
  if(interactive()) {
    if(error!="") cat("\n")
    error = paste0(error,"\nExecution halted.\n")
    #tryCatch(.halt(), myerror=function(...) cat(error))
    stop(error, call.=FALSE)
  } else {
    if(error!="") cat(error)
    cat("\nExecution halted.\n")
    utils::flush.console()
    q("no", status=1, runLast=FALSE)
  }
}

################################################################################

## A more elaborate version of stop(). It can do beeping and if inside an interactive
## session it doesn't stop execution but enters a browsing mode
error = function(t, ..., sep="", Q=TRUE, BROWSE, nskip1=0) {
  
  # Play sound if beepr package available
  if(any(list_installed_packages()=="beepr")) {
    require("beepr")
    try(beep(1))
  }
  
  if(!missing(BROWSE) && BROWSE) Q = FALSE

  # Show the error and stop if Q is true
  for(i in rep(0,max(0,nskip1))) cat("\n")
  error = paste(t, ..., sep=sep)
  if(Q) {
    if(!interactive()) error = paste0("\nERROR: ",error,"\n")
    halt(error) 
  } else {
    cat("\nERROR: ")
    cat(error)
    cat("\n\n")
    utils::flush.console()
    do.call(browser, list(), envir=sys.frame(sys.parent()))
  }
  
}

################################################################################

## Returns the name of the function from which it is invoked as character string
this_fun = function(level=-1)
  as.character(sys.call(level))[1]

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
fun_source = function(fun) {
  stopifnot(is.function(fun))
  c(deparse(args(fun))[deparse(args(fun))!="NULL"], deparse(body(fun)))
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
  return(libpath %in% .libPaths())
}

################################################################################

is_win = function() .Platform$OS.type=="windows"
is_linux = function() any(.Platform$OS.type==c("linux","unix"))
is_term = function() .Platform$GUI=="RTerm"
is_rgui = function() .Platform$GUI=="Rgui"
is_rstudio = function() .Platform$GUI=="RStudio"

################################################################################

## Adds 'path' to the system path
append_path = function(path) {
  PATH = Sys.getenv("PATH")
  if(regexpr(patternize(path), PATH)<=0) 
    Sys.setenv(PATH=paste0(PATH,";",path))
}

################################################################################

## Check if a given file is locked by another application (e.g. Excel)
check_file_locked = function(file) {
  if(is_win()) {
    call = "wmic process get commandline"
    x = try(system(call, intern=TRUE, show.output.on.console=FALSE))
    if(class(x)=="try-error") return(-1)
    as.numeric(any(regexpr(patternize(file), x)>0))
  } else return(-1)
}

################################################################################

## Returns trailing arguments to the call that invoked the current R session
## Useful when calling Rscript on a script and modifying its behavior via flags
get_args = function() {
  args = commandArgs(trailingOnly=TRUE)
  args = unname(sapply(args, function(x) gsub("[\r\n]","",x)))
  args = args[nchar(args)>0]
  return(args)
}

################################################################################

## Prints some details about the current R session including the version of R
session_info = function() {
  CALL = commandArgs(trailingOnly=FALSE)
  cat("R version information:\n")
  print(R.version)
  cat("\n")
  cat("Program call: '",paste(CALL, collapse=" "),"'\n", sep="")
  cat("Working path: '",getwd(),"'\n", sep="")
}

################################################################################

## A version of 'modifyList' from utils which drops zero-length elements in 'val'
## before updating 'x' (optionally can behave the same as modifyList)
modifyList2 = function(x, val, ..., drop_null_val=TRUE)
  utils::modifyList(x, if(drop_null_val) Filter(length, val) else val)

################################################################################

## Creates a named list: The code list(a = a, b = b) becomes nlist(a,b) and 
## list(a = a, b = 2) becomes nlist(a, b = 2), etc. (This is lifted from the package 
## 'loo' so that it doesn't have to be installed just for this one function
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

## Returns the last element in an vector (Hint: for quickest execution use the code 
## rev(x)[1] directly in your code
last_element = function(x) rev(x)[1]

################################################################################

## This does a recursive merge. Taken from package reshape because it has a bug
## which is fixed here (bug: missing ellipsis in the call to Recall())
merge_recurse = function (dfs, ...) {
  if (length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all = TRUE, sort = FALSE, ...)
  } else {
    merge(dfs[[1]], Recall(dfs[-1], ...), all = TRUE, sort = FALSE, ...)
  }
}

################################################################################

## Removes zero-length elements from a list
list_clean = function(L, null.rm=TRUE)
  return(Filter(length, L))
  
################################################################################

# A wrapper for list.files that allow multiple patterns at once and 
# files that match at least on of the patterns are returned
list_files = function(pattern=NULL, ...) {
  if(length(pattern)==0) {
    list.files(...)
  } else {
    unique(unlist(sapply(pattern, function(p) list.files(pattern=p, ...))))
  }
}

######################################################################################

# Wraps special characters in string name (possibly a vector) by brackets so that
# it can be matched within regular expression matching (the case of "\\" has to be
# treated differently)
patternize = function(name, special=c("+",".","(",")","$","?","\\")) {
  for(i in seq2(1,length(name),1))
    for(s in special) 
      name[i] = gsub(paste0("[",s,"]"), 
                     paste0("[", paste(rep(s,1+I(s=="\\")), collapse=""),"]"), 
                     name[i])
  return(name)
}

################################################################################

## Does the opposite of 'patternize()'
unpatternize = function(pattern)
  gsub("[][]","",pattern)

################################################################################

## Removes trailing spaces from the beginning and end of a string
str_trim = function (x) gsub("^\\s+|\\s+$", "", x)

################################################################################

# Lists files within a zip archive that match pattern (which can contain multiple
# regular patterns)
list_zip = function(zipfiles, pattern=".*", mask_exclude=FALSE) {

  # List all files
  files = sapply(zipfiles, function(z) unzip(z, list=TRUE)$Name)
  is_list = is.list(files)

  # Keep only files that match at least one of the patterns in 'pattern'
  pattern = paste(pattern, collapse="|")
  flip = -as.numeric(mask_exclude)
  files = lapply(files, function(f) f[flip * regexpr(pattern, f) > 0])

  files = list_clean(files)
  if(!is_list) files = unlist(files)

  return(files)
  
}

################################################################################

## Zips up files matching mask (and not matching mask_exclude) each into its own archive
zipup = function(mask=".*", mask_exclude, outfile, path=".", appendix=".zip", extras="-m", do_patternize=TRUE, 
  chunk=Inf, announce=FALSE, single_archive=TRUE, continue_on_error=TRUE, retry=TRUE) {

  # List the files matching mask
  if(do_patternize) mask = patternize(mask)
  files = list.files(path=path, pattern=mask)
  
  # If not files found, just return
  if(length(files)==0) return(list(ios=-1, zipfile=NULL))
  
  # Otherwise define a file name if a single file found, but if multiple
  # files found, quit
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
    if(announce) note("No files found using mask '",mask,"'.")
    return(list(ios=1))
  } else {
    if(announce) cat("Total of ",length(files)," found using mask '",mask,"'.\nZipping up ...\n")
  }
  
  # Define the name based on mask
  mask = unpatternize(mask)
  #zfile = paste0(mask, appendix)

  # Zip them up into a single archive (single_archive is true) or each individually (otherwise)
  ichunk = 0
  zf = NULL
  chunk = if(!single_archive) 1 else max(1, chunk)
  while(length(files)>0) {
    
    # Define a randon (short) file name (if not given on input)
    if(is.null(zf) || !single_archive) zf = paste0(random_filename(),".zip")
    
    # If single archive should be produced, add the flag 'g' (from the 2nd file on)
    ichunk = ichunk + 1
    if(single_archive && ichunk==2) extras = paste0("-",sub("-","",extras),"g")
    
    # Zip the current file up. Possibly try twice in case a file access error occured
    iis = c(1,rep(2,5))[c(TRUE, rep(retry,5))]
    for(i in iis) {
      
      ios = zip(zf, head(files, chunk), extras=extras)
      
      if(ios==12) {
        note("Zip reported an error 'name not matched' (probably due to very long file names), but I'll continue anyway.")
        break
      }

      if(i<max(iis) && ios==15) {
        Sys.sleep(i^2*0.05)
        next
      }
      
      if(ios!=0) {
        #error("Error occurred during zipping!", Q=(!interactive() && !continue_on_error))
        error("Problems during zipping!", Q=FALSE)
        if(!continue_on_error) browser()
      }
      
    }

    # Rename the zip file to a proper name (unless a single archive is to be produced)
    if(!single_archive) {
      outfile = paste0(head(files, 1), appendix)
      file_rename(zf, outfile)
    }
    
    # Remove the current file from the list of files to add
    files = tail(files, -chunk)
    
  } # while(length(files)>0) 
  
  # Rename the zip archive to fit mask
	if(single_archive) file_rename(zf, outfile)
  
  return(list(ios=ios, zipfile=outfile))
  
}

################################################################################

zip_all_in_path = function(path=".") {

  wd = getwd()
  warn("This will zip up all files in the path '",path,"' relative to current working directory '",wd,"'!")
  wait()
  
  ds = setdiff(list.dirs(), "..")
  for(d in ds) {
    setwd(d)
    fs = setdiff(setdiff(list.files(), list.dirs(full.names=FALSE)), list.files(pattern="[.]zip$"))
    for(f in fs) zip(paste0(f,".zip"), f, extras="-m")
    setwd(wd)
  }
  
  catn("Finished.")
  
}

################################################################################

## Unzips from 'zfile' all files that match (do not match if mask_exclude is false) the mask
un_zip = function(zfile, mask=".*", mask_exclude=FALSE, do_patternize=TRUE, remove=FALSE) {
  
  # List the files matching mask
  if(do_patternize) mask = patternize(mask)
  files = list_zip(zfile, pattern=mask, mask_exclude=mask_exclude)

  # Nothing to zip up
  if(length(files)==0) return(1)

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

## Splits string into a vector of individual characters
str2vector = function(x) 
  unlist(strsplit(x, split=""))

#############################################################

## Reverses the order of characters in a string
strrev = function(x) 
  sapply(x, function(y) paste(rev(str2vector(y)), collapse=""))

#############################################################

## Finds the first/last occurence of a character in a string
strpos = function(string, char, first=!last, last=!first) {
  stopifnot(xor(first, last))
  if(missing(first)) first = !last
  s = if(first) string else strrev(string)
  p = c(regexpr(paste0("[",char,"]"), s))
  if(!first && p>0) p = nchar(string) - p + 1
  return(p)
}

################################################################################

## Shifts (rotates) the elements in a vector in 'x' by 'lag' spaces 
shift = function(x, lag=1) {
  if(lag==length(x) || length(x)==0) return(x)
  lag = lag %% length(x)
  if(lag==0) return(x)
  append(tail(x,lag), head(x, -lag))
}


################################################################################

h1 = function(...)
  head(..., n=1)
  
################################################################################

t1 = function(...)
  tail(..., n=1)
  
################################################################################

## Find the midpoints between individual elements in 'x'
midpoints = function(x)
  0.5*(head(x, -1) + tail(x, -1))

################################################################################

## Insert an element in 'what' inside 'x' at the position 'after+1'
insert = function(x, what, after) {
  after = max(0, head(after, 1))
  append(append(if(after==0) NULL else head(x, after), what), if(after==0) x else tail(x, -after))
}

################################################################################

## Replaces dots in file names with the value in 'chr'. Useful for changing file
## names of plots that are to be included in a latex file where the dots cause trouble
clean_filename = function(x, chr="-", keep.last.dot=TRUE) {
  p = if(keep.last.dot) strpos(x, ".", last=TRUE) else -1
  if(p<=0) {
    gsub("[.]",chr,x)
  } else {
    paste0(gsub("[.]",chr,substr(x,1,p-1)), substr(x,p,nchar(x)))
  }
}

################################################################################

## Reads a table from within a zip file (equivalent to read.table except the
## input file is expected to be a zip archive)
read_table_zip = function(zipfiles, files=NULL, pattern=NULL, nonames=FALSE, 
  maxnfiles=Inf, skipnfiles=0, trace=0, maxnchar=128, solve_long=TRUE, 
  long_action=c("copy","rename"), attempt_read_plain=FALSE, ...) {
  
  # Process what to do to avoid long file name problems
  long_action = tolower(head(long_action, 1))
  if(!is.null(long_action) && !is.na(long_action) && long_action!="rename") long_action = "copy"
  fun_action = switch(long_action, "rename"=file_rename, "copy"=file.copy, void)
  
  # Read the zip file(s)
  RES = nams = NULL
  for(zfile in zipfiles) {
  
    # Extract the extension
    zip_type = sub(".*[.]","",zfile)
		
		if(all(zip_type!=c("zip","gz")) && attempt_read_plain) zip_type = "plain"
		
    # Get the set of active connections
    set1 = getAllConnections()
    
    # If a list of files inside zipfiles is missing, list them all
    file = if(!is.null(files)) files else 
       if(zip_type=="zip") unzip(zfile, list=TRUE)$Name else 
       if(zip_type=="plain") zfile else "'*'"
    
    # Skip the desired number of files
    if(skipnfiles>0) file = tail(file, -skipnfiles)

    # Sex maximum number of files
    file = head(file, maxnfiles)
    
    # Match the mask
    if(!is.null(pattern)) file = file[regexpr(pattern, file)>0]

    # Make sure the file names are not too long
    restore_file = FALSE
    name_len = nchar(zfile)
    if(name_len > maxnchar && any(zip_type==c("zip","gz"))) {

      warn("Zip file name is long (",name_len," characters), which might cause errors while unzipping",
           " (Use arguments 'solve_long' and 'long_action' to enable a solution).", skip1=1, skip2=0)
      
      if(solve_long && !is.null(long_action) && !is.na(long_action)) {
      
        if(trace>0) 
          cat("Trying to avoid the problems via shorting the zip file name by ",long_action,"ing ...\n")
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

        if(trace==0) cat("Reading ",length(file)," file(s) from archive '",zfile,"' ...\n")

      }
      
    }
    
    # Loop over file names in 'file'
    for(f in file) {
    
      # Open connection
      if(zip_type=="zip") {
      
        infile = unz(zfile, f)
        nams = c(nams, paste0(zfile,":",f))    
      
      } else if(zip_type=="gz") {
      
        infile = gzfile(zfile)
        nams = c(nams, paste0(zfile))
      
      } else if(zip_type=="plain") {
			  
				warn("Unknown compression format of file ",zfile," (read_table_zip). Attempting to read as plain text ...")
				infile = f
        nams = c(nams, paste0(zfile,":",f))    
				
			} else error("Unknown compression format of file ",zfile," (read_table_zip)!")
    
      # Find the newest connection
      set2 = getAllConnections()
      acon = setdiff(set2,set1)
    
      # Read the file
      if(trace>0) cat("Reading file ",f," from inside ",zfile," ...\n")
      res = read.table(infile, ...)
      RES = append(RES, list(res))
      
      # Close the connection
      if(any(getAllConnections()==acon)) close(getConnection(acon))
    
    } # for(f in file)
        
    # Remove the extra copy
    if(restore_file) {
      cat("Restoring original file names (by ",ifelse(long_action=="rename","renaming","deletion"),") ...\n")
      ios = try(if(long_action=="rename") file_rename(zfile, zfile_orig) else 
                if(long_action=="copy") file_remove(zfile) else 
                error("Unknown action '",long_action,"'."), silent=TRUE)
    }
          
  } # for(zfile in zipfiles)
  
  # Assign nammes to the elements in the result list
  if(!nonames) names(RES) = nams
  
  return(RES)

}

################################################################################

## Logit, probit, inverse logit functions
logit = function(x) log(x/(1-x))
probit = stats::qnorm
ilogit = function(x) 1 / (1+exp(-x))
#invlogit = function(x) exp(x)/(1+exp(x))
#invlogit = function(x) exp(x)/(1+exp(-x))

################################################################################

## Returns an upper tail probability of the normal distribution 
upnorm = function(..., lower.tail=FALSE) stats::pnorm(..., lower.tail=lower.tail)

################################################################################

## Returns an upper tail quantile of the normal distribution 
uqnorm = function(..., lower.tail=FALSE) stats::qnorm(..., lower.tail=lower.tail)

################################################################################

## Prints a warning and stops or waits depending on arguments
warn = function(t, ..., sep="", quit=FALSE, wait=FALSE, skip1=1, skip2=1, flush=TRUE) {
  cat0(rep("\n", skip1), flush=FALSE)
  cat0("WARNING: ", flush=FALSE)
  cat0(t,...,"\n",sep=sep, flush=FALSE)
  cat0(rep("\n", skip2), flush=flush)
  if(quit) halt() 
  if(wait) wait() 
}

################################################################################

## Print a note and optionally waits
note = function(t, ..., sep="", W=FALSE, skip1=0, skip2=0, flush=TRUE) {
  cat0(rep("\n", skip1), flush=FALSE)
  cat0("NOTE: ", flush=FALSE)
  cat0(t,...,"\n",sep=sep, flush=FALSE)
  cat0(rep("\n", skip2), flush=flush)
  if(W) wait() 
}

################################################################################

## Lists all open graphical devices
all_devs = .all_devs = function(silent=TRUE) {

  # Announce how many open
  if(!silent) { 
    cat("Found ",length(dev.list())," open devices.\n", sep=""); .fc() 
  }

  dev.list()
  
}

################################################################################

## Closes all open graphical devices (silently by default)
.all_devs_off = function(silent=TRUE) {

  # Announce how many open
  if(!silent) { 
    cat("Found ",length(dev.list())," open devices.\n", sep=""); .fc() 
  }
  
  # Clope all open devices
  if(!is.null(dev.list())) {
    if(!silent) { 
      cat("Closing all open devices ...\n", sep=""); .fc() 
    }
    for(i in dev.list()) dev.off()
    # Check for success
    if(!is.null(dev.list())) {
      warn("Some devices could not be closed.")
    } else if(!silent) { 
      cat("All devices closed.\n"); .fc() 
    }
  }
  
}

################################################################################

## Does the same as .all_devs_off but announces how many devices were closed
.ado = function() .all_devs_off(silent=FALSE)

################################################################################

## Capitalizes first letters of each element in the vector 'string'
toupperfirst = function(string) 
  sapply(string, function(s) paste0(toupper(substring(s, 1,1)), substring(s, 2)))
      
################################################################################

format_time = function(tim, prec=3, drop_hour=FALSE) {
# Takes time in seconds and formats 
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
  tt = paste0(paste0(c(th,tm,ts),collapse=":"),ifelse(is.null(tm)," seconds",""))
  return(tt)
}

################################################################################

clock = function(announce=TRUE, with_date=TRUE, lead="", digs=3, eol="\n") {
# Gets the current time (optinally prints it)
  odigs = options()$digits.secs
  options(digits.secs=digs)
  d1 = paste0(Sys.time())
  options(digits.secs=odigs)
  if(!with_date) d1 = sub(".*[ ]","",d1)
  if(announce) cat(lead,d1,eol, sep="")
  return(invisible(c("time"=d1)))
}

################################################################################

## Starts the runtime clock and prints an announcement
start_clock = function(print=TRUE, envir=parent.frame(), lead="", what="analysis") {
# Starts the runtime clock
  d1 = Sys.time()
  if(print) cat(lead,toupperfirst(what)," started at ",paste0(d1),".\n", sep="")
  assign("._start_time_variable", d1, envir=envir)
  assign("._last_check_time_variable", d1, envir=envir)  
  return(invisible(c("starttime"=d1)))
}

################################################################################

## Gets the current value of the runtime clock created by the other functions
read_clock = function(envir=parent.frame()) {
# Reads the runtime clock

  # Read the current clock and recall start
  d2 = Sys.time()
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

## Stops the runtime clock
stop_clock = function(d1=NULL, d12=NULL, start_print=FALSE, end_print=TRUE, 
                      runtime_print=TRUE, envir=parent.frame(), lead="", what="analysis") {
  
  # Read the current clock and recall start
  d2 = Sys.time()
  if(is.null(d1) && exists("._start_time_variable", envir=envir))
    d1 = get("._start_time_variable", envir=envir)

  # Announce start and calculate runtime
  if(!is.null(d1)) {
    if(start_print) cat(lead,toupperfirst(what)," started at ",paste0(d1),".\n", sep="")
    if(is.null(d12)) d12 = difftime(d2,d1,unit="secs")
  }
  
  # Announce end
  if(end_print) cat(lead,toupperfirst(what)," finished at ",paste0(d2),".\n", sep="")
  if(!is.null(d12) && runtime_print)
    cat(lead,"Total ",tolower(what)," runtime of ",format_time(as.numeric(d12)),".\n", sep="")
  .fc()

  return(invisible(c("runtime"=d12)))

}
  
################################################################################

## Inverts elements in an object except those that are zero which are kept zero
Invert1 = function(x) {
  y = rep(0, length(x))
  y[x!=0] = 1/x[x!=0]
  return(y)
}

################################################################################

## Finds pseudoinverse, square root, and sqrt-inverse matrices
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

## Finds the intersect multiple sets
mintersect = function(...) {
  sets = list(...)
  set1 = sets[[1]]
  for(i in seq2(2, length(sets), 1)) set1 = intersect(set1, sets[[i]])
  return(set1)
}

################################################################################

## Calculates the harmonic mean of x and y
hmean = function(...) if(length(c(...))==0) c(...) else 1/mean(1/c(...))
#hmean = function(x, y=NULL) 1/mean(c(1/x,1/y))

################################################################################

## Calculates the geometric mean of x
gmean = function(x, na.rm=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  #prod(x)^(1/length(x))
  exp(sum(log(x)/length(x)))
}

################################################################################

## Calculates the arithmetric mean of x (same as mean but with a different output
## on length zero input
amean = function(x, na.rm=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  if(length(x)>0) mean(x) else NULL
}

################################################################################

## Functions for memory unit conversion ##

## Determines the appropriate unit of memory for the size of 'x'
get_unit = function(x) {
  w = pmax(1, pmin(trunc(log10(x)/3)+1,5))
  unit = sapply(w, function(y) if(is.na(y) || is.nan(y)) NA else switch(y, "B", "kB", "MB", "GB", "TB"))
  return(unit)
}

## Returns the size of the unit in 'unit'
de_unit = function(unit)
  return(if(is.na(unit) || is.nan(unit)) NA else switch(unit, "B"=1, "kB"=1e3, "MB"=1e6, "GB"=1e9, "TB"=1e12))

## Converts which is assumed to be in unitary units (i.e. bytes) to an appropriate 
## other unit
convert_unit = function(x, unit, append_unit=TRUE, ndigit=3) {
  if(missing(unit)) unit = get_unit(x)
  s = rsignif(x / sapply(unit, de_unit), ndigit)
  if(append_unit) s = paste0(as.character(s), unit)
  return(s)
}

################################################################################

short_notation = function(x, exact_cutoff=999, ndig=1, word_units=FALSE) {
  
  # Figure out which to convert
  x = as.numeric(x)
  xx = x
  xx[is.na(x)] = as.character(NA)
  xx[x<=exact_cutoff] = as.character(x[x<=exact_cutoff])
  wx = !is.na(x) & x>exact_cutoff

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

short_notation_exp = function(x, base=10, ndig=2, as_expr=FALSE) {
  x = sapply(x, function(x1) { z = signif(log(x1,base),ndig); 
                               if(z==0) bquote(""~10^{""~0}) else bquote(.(base)^{.(z)})})
  if(as_expr) x = sapply(x, as.expression)
  if(length(x)==1) x = x[[1]]
  return(x)
}

N2T = short_notation
N2Texp = short_notation_exp

shorten_sequence = function(x, sep="-", collapse="-", f=length) 
  paste(c(paste(range(x),collapse=collapse),head(f(x),1)),collapse=sep)

################################################################################

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

# shorthand
lsos = function(..., envir=parent.frame(), n=10) {
  .ls.objects(..., envir=envir, order.by="Size in bytes", decreasing=TRUE, head=TRUE, n=n)
}

################################################################################

## Returns the sizes of supplied objects (in memory unit given in 'unit')
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
load_objects = function(file, announce=FALSE, list_new=FALSE, expected_objects=NULL, 
                        quit_on_miss=FALSE, envir=parent.frame()) {
                        
  # Check for non-scaler file
  if(length(file)!=1) error("Supply a single file name to load_objects().")
  
  # Announce loading and file name
  if(announce) {
    cat("All objects from file '",file,"' will be loaded ...\n",sep=""); .fc()
  }
  
  # Check for missing file
  if(!file.exists(file)) error("File '",file,"' does not exist!")
  
  # Environment "local" means this function
  if(class(envir)=="character" && envir=="local") 
    envir = sys.frame(sys.nframe())

  ## Remove the objects that are in the file from environment 'envir'
  rme(expected_objects, envir=envir)
  
  # List currently present variables in 'envir'
  lst = c(ls(envir=envir), "lst")
  
  # Load the file
  if(announce) { cat("Loading file (size ",file_size(file),") ... ", sep=""); .fc() }
  loaded = load(file, envir=envir)
  if(announce) { cat("done.\n"); .fc() }
  
  # List new objects loaded from the file
  #loaded = setdiff(ls(envir=envir),lst)
  sizes = NULL
  if(list_new) {
    cat("Getting sizes of loaded objects ...\n"); .fc()
    sizes = obj_size(list=loaded, with_unit=TRUE, envir=envir)
    cat("Loaded objects: ",paste(loaded," (",sizes,")",sep="",collapse=", "),"\n", sep=""); .fc()
  }
  
  # Check for missing objects
  if(!is.null(expected_objects)) {
    miss = setdiff(expected_objects, loaded)
    if(length(miss)>0) {
      msg = paste0("The following expected objects were not loaded: '",paste(miss,collapse="' '"),"'")
      if(quit_on_miss) error(msg) else warn(msg)
    } else {
      cat("All expected objects were successfully loaded.\n"); .fc()
    }
  }

  return(invisible(cbind("Object"=loaded, "Size"=sizes)))

} # load_objects

################################################################################

## Lists all objects inside given files or inside files that match pattern in 
## 'pattern' relative to the path in 'dir'
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

## Find an object with name 'object_name' inside files in 'files' relative to the 
## path supplied in 'dir'. If a pattern is given instead of list of files all
## files that match the pattern are searched for the object
find_object = function(object_name, files=NULL, dir=".", pattern="^.*[.]RData$", 
                       stop_on_found=TRUE, announce=TRUE) {
  if(missing(object_name) || length(object_name)==0) 
    error("Missing object name to find!")
  if(missing(files) || length(files)==0) 
    files = list.files(dir, pattern=pattern)

  setwd(dir)
  cat("Searching for object '",object_name,"' ...\n", sep=""); .fc()
  identified_files = NULL
  for(file in files) {
    loaded = load_objects(file, list_new=TRUE, announce=announce, envir="local")
    w = loaded[,1] == object_name
    if(any(w)) {
      identified_files = c(identified_files, file)
      if(stop_on_found) break
    }
  }
  
  if(announce) {
    if(length(identified_files)==0) {
      cat("Unfortunately the object '",object_name,"' could not be find in any of the files.\n", sep="")
    } else {
      if(stop_on_found) {
        cat("Object '",object_name,"' found in file '",identified_files,"'. Search stopped.\n", sep="")  
      } else {
        cat("Object '",object_name,"' found in ",length(identified_files)," files.\n", sep="")  
      }
    }
    .fc()
  }
  
  #if(!is.null(identified_files))
  #  names(identified_files) = object_name
  
  return(identified_files)
} 

################################################################################

## Checks if a sequence in 'x' has only unique elements
is_unique = function(x) anyDuplicated(x)==0 

## Check if all elements in 'x' have the same value
is_all_same = function(x) length(unique(x))<=1

## Checks if a sequence in 'x' is decreasing
is_decreasing = function(x) !is.unsorted(rev(x)) 

## Checks if a sequence in 'x' is increasing
is_increasing = function(x) !is.unsorted(x)

## Checks if elements in 'x' are effectively integers (i.e. close enough to one)
is_integer1 = function(x, tol=.Machine$double.eps) 
  return(x==floor(x+2*tol))

## Checks if elements of 'x' are odd
is_odd = function(x, tol=.Machine$double.eps) abs((x+1) %% 2) < tol

## Checks if elements of 'x' are even
is_even = function(x, tol=.Machine$double.eps) abs(x %% 2) < tol

## Checks if a reduction of elements of 'x' are NA. Default reduction is 'all()'
## which returns TRUE iff all elements are NA. An obvious useful alternative is 
## to use 'any()' as argument to 'freduce'.
is_na = function(x, freduce=all) freduce(is.na(x))

################################################################################

## Checks if elements in 'x' are numeric even if stored as strings. In other words, 
## it checks whether x is of class numeric or whether it is a string that can has the format
## of a number, otherwise it returns FALSE. Function freduce is applied to the vector.
is_number = function(x, freduce=all) {
  a = if(is.numeric(x)) rep(TRUE, length(x)) else 
      if(is.character(x)) 
        regexpr("^[0-9]+$", x)>0 | (regexpr("^[-]?[0-9]*[.][0-9]*$", x)>0 & regexpr("[0-9]", x)>0) else 
        rep(FALSE, length(x))
  return(freduce(a))
}

################################################################################

## Convert x to numeric without any warnings about non-numeric elements
as_numeric = function(x) {
  y = as.numeric(rep(NA, length(x)))
  y[is_numeric(x, freduce=I)] = as.numeric(x[is_numeric(x, freduce=I)])
  return(y)
}

################################################################################

## Changes the type of x to "numeric" unless it does not contain only numbers
make_numeric = function(x, on_error=function(...) stop("Cannot convert x to class 'numeric'."))
  if(is.numeric(x)) x else if(is_number(x)) as.numeric(x) else on_error(x)

################################################################################

## Converts to integer by stripping all non-number substrings in each element of x
force_as_integer = function(x)
  as.integer(gsub("[^0-9]*","",x))

################################################################################

## Checks if 'x' is in scientific notation (e.g. 1.234E5)
is_scientific = function(x, freduce=all) {
  a = sapply(x, is.numeric) & regexpr("^[0-9.]+[Ee][-0-9]+$", x)>0
  return(freduce(a))
}

################################################################################

## Checks if values in 'x' are integers
is_round = function(x, eps=.Machine$double.eps)
  return(if(is.na(x) || is.infinite(x)) FALSE else abs(round(x)-x)<=eps)

################################################################################

## Checks if 'x' is a diagonal matrix 
is_diag = function(x) 
  is.matrix(x) && nrow(x)==ncol(x) && (length(x)==1 || max(abs(x[upper.tri(x)]))<.Machine$double.eps)

################################################################################

## Checks if M is a positive-definite matrix
is_posdef = function(M) {
  if(!is.numeric(M)) error("M must be numeric.")
  if(length(M)==1 && M>0) return(TRUE)
  if(!is.matrix(M)) error("M is not a matrix.")
  if(nrow(M)!=ncol(M)) error("M must be a square matrix.")
  llibrary("mnormt")
  x = try(mnormt::rmnorm(1, mean=rep(0,nrow(M)), varcov=M), silent=TRUE)
  if(class(x)=="try-error") {
    y = FALSE
    attr(y,"msg") = as.character(attr(x, "condition"))
  } else y = TRUE
  return(y)
}

################################################################################

# Sorts x according to the order given in pattern by matching the names of x to 
# those patterns
pattern_sort = function(x, pattern, get_order=FALSE) {
  if(length(x)==0 || is.null(names(x))) return(if(get_order) seq2(1,length(x),1) else x)
  
  nam = names(x)
  ords = lapply(pattern, function(pat) which(regexpr(pat, nam)>0))
  ords = append(ords, list(setdiff(1:length(x), unlist(ords))))
  reords = lapply(ords, function(w) order(nam[w]))
  ords = lapply(1:length(ords), function(i) ords[[i]] = ords[[i]][reords[[i]]])
  ord = unlist(ords)
  
  return(if(get_order) ord else x[ord])
}

################################################################################

## Reorders x according to order and drop the extra attributes generated by 'reorder()'
reord = function(x, order) { 
  y = reorder(x, order)
  attributes(y) = NULL
  return(y) 
}

################################################################################

## A more versatile version of which.max
which_max = function(x, last=TRUE, na.rm=FALSE, na.ignore=TRUE, arr.ind=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  if(na.ignore) x[is.na(x)] = -Inf
  w = if(last) length(x) + 1 - which.max(rev(x)) else which.max(x)
  if(length(dim(x))>1 && arr.ind) 
    w = which(array(1:length(x), dim=dim(x))==w, arr.ind=arr.ind)
  return(w) 
}
  
################################################################################

## A more versatile version of which.min
which_min = function(x, last=TRUE, na.rm=FALSE, na.ignore=TRUE, arr.ind=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  if(na.ignore) x[is.na(x)] = -Inf
  w = if(last) length(x) + 1 - which.min(rev(x)) else which.min(x)
  if(length(dim(x))>1 && arr.ind) 
    w = which(array(1:length(x), dim=dim(x))==w, arr.ind=arr.ind)
  return(w) 
}

################################################################################

## Returns the minimal distance between elements in 'x'
min_dif = function(x)
  return(if(length(x)==1) NA else min(diff(sort(x))))
  #return(min(sapply(x, function(x1) min(abs(x1-x)))))

################################################################################

## Replaces zeros in 'x' with 'value'
unzero = function(x, value=1) {
  x[x==0] = value
  return(x)
}

################################################################################

## An wrapper for max() which doesn't produce warnings
max0 = function(..., na.rm=FALSE, val0=-Inf) {
  x = c(...)
  if(na.rm) x = x[!is.na(x)]
  if(length(x)==0) val0 else max(..., na.rm=na.rm)
}

################################################################################

## An easier way to produce histograms with 'nbreaks' bins
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

## Does the same thing as table but makes sure that the counts of values in obligate are
## included in the final table but only if a tabulation of a single vector is performed
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

# A quicker version of table(). Assumes natural number values and every value outside 
# 1,2,... is counted as 0.
tablenat = function(x) {
  C = tabulate(x)
  lev = which(C>0)
  C = C[lev]
  names(C) = lev
  if(sum(C)<length(x)) C = c("0"=length(x)-sum(C), C)
  return(C)
}

################################################################################

## A version of setwd() which attempts to create the given path
setwd2 = function(dir, create=TRUE, ask=TRUE) {
  if(!dir.exists(dir)) {
    if(create) {
      if(ask && interactive()) {
        cat("Directory '",dir,"' does not exist. It will be created unless you press ESC ...\n", sep="")
        wait()
      }
      dir.create(dir)
    } else {
      error("Cannot change working directory to '",dir,"' since it does not exist and create is 'FALSE'!")
    }
  }
  setwd(dir)
}

################################################################################

## A verbose version of dir.create()
dir_create = function(dir, ask=TRUE, trace=0) {
  
  if(!dir.exists(dir)) {
    
    if(ask && interactive()) {
      cat("Directory '",dir,"' does not exist. It will be created unless you press ESC ...\n", sep="")
      wait()
    }
    
    dir.create(dir, showWarnings=FALSE, recursive=TRUE)
    
    if(!dir.exists(dir)) error("Directory '",dir,"' could not be created!")
      
  } else if(trace>0)
    cat("No need to create, directory '",dir,"' already exists.\n", sep="")
    
  return(dir)
    
}

################################################################################

## Remove multiple directories
dir_remove = function(dir)
  sapply(dir, unlink, recursive=TRUE)

################################################################################

## Finds the position of the last non-zero element in 'x'
last_nonzero = function(x)
  length(x) - which.max(rev(x)!=0) + 1

## Finds the position of the last positive element in 'x'
last_positive = function(x)
  length(x) - which.max(rev(x)>0) + 1

## Finds the position of the last element in 'x' with value larger than 'y'
last_above = function(x, y)
  length(x) - which.max(rev(x)>y) + 1

## Finds the position of the first non-zero element in 'x'
first_nonzero = function(x)
  which.max(x!=0)

## Finds the position of the first element in 'x' with value larger than 'y'
first_above = function(x, y)
  which.max(x>y)

## Strips 'x' of non-positive elements
positive = function(x) return(x[!is.na(x) & x>0])
## Strips 'x' of non-negative elements
negative = function(x) return(x[!is.na(x) & x<0])
## Strips 'x' of elements that are not non-negative
nonnegative = function(x) return(x[!is.na(x) & x>=0])
## Strips 'x' of elements that are not non-position
nonpositive = function(x) return(x[!is.na(x) & x<=0])

################################################################################

## Rounds the elements of vector x to 'nd' digits while making sure x does 
## not become non-unique (if it was unique to begin with)
round2 = function(x, nd=0) {
  x_is_unique = anyDuplicated(x)==0
  for(d in seq(nd,nd+15)) {
    y = as.numeric(sprintf(paste0("%.",d,"f"), x))
    if(!x_is_unique || anyDuplicated(y)==0) break
  }
  return(y)
}

################################################################################

## Rounds 'x' to 'ndigit' digits after the decimal point without losing precision
## before the decimal point
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

## Same as signif except it allows non-numeric input and doesn't halt on it
signif2 = function(x, ...) if(is.numeric(x)) signif(x, ...) else x

################################################################################

## Similar to diff except returns ratios instead of differences
frac = function(x) 
  if(length(x)<2) NA else tail(x, -1) / head(x, -1)

################################################################################

## Checks if 'x' is a permutation of 'y'
is_permutation = function(x, y)
  isTRUE(all.equal(sort(as.vector(x)), sort(as.vector(y))))

################################################################################

## Generates all permutations of elements in 'x'
all_permutations = function(x) {
  llibrary(gtools)
  if(length(x)>20) warn("Length of 'x' is large, there are ",factorial(length(x))," permutations.")
  ip = permutations(n=length(x), r=length(x))
  matrix(x[ip], ncol=length(x))
}

################################################################################

seq2 = function(...)
  if(class(s <- try(seq(...), silent=TRUE))=="try-error") NULL else s

################################################################################

## Produces an equidistant sequence between a and b which contains both
## m1 and m2 of appropriate length near len. The sequence might not contain
## the border points points a and b. Forcing them into the sequence
## can be done via add_a and/or add_b, but the equi-distance of all points 
## might no longer be true.
seq_around = function(a, b, m1, m2, len, add_a=FALSE, add_b=FALSE) {
  
  if(missing(a)) error("Specify a!")
  if(length(a)>1) {
    b = max(a)
    a = min(a)
    if(missing(len) && is_integer(a) && is_integer(b)) len = b-a+1
  } else if(missing(b)) error("Specify b!")
  if(missing(m1) && !missing(m2)) error("Specify m1!")
  
  if(a>b) {
    if(len>1) error("When argument 'len' is supplied, the value in 'b' cannot be smaller than value in 'a'!")
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

timest = function(add_time=TRUE, add_pid=TRUE) {
# "Unique" stamp generating function based on current time and process id
  stamp = ""
  if(add_time) {
    d = date()
    timestamp = paste(substr(d,21,24),substr(d,5,7),substr(d,9,10),substr(d,12,13),
                      substr(d,15,16),substr(d,18,19),sep="")
    timestamp = sub(" ","0",timestamp)
    stamp = paste0(stamp,timestamp)
  }
  if(add_pid)
    stamp = paste0(stamp,"pid",Sys.getpid())
  return(stamp)
}

################################################################################

## Lists all installed packages
list_installed_packages = function() {
  x = try(installed.packages()[,"Package"])
  return(if(class(x)=="try-error") "" else x)
}
  
################################################################################

## Lists all loaded packages
list_installed_packages = function()
  installed.packages()[,"Package"]

## Lists all loaded packages
list_loaded_packages = function()
  c(sessionInfo()$basePkgs, names(sessionInfo()$otherPkgs))

################################################################################

## Checks if a package is installed
pckg_is_installed = function(pckg) 
  any(installed.packages()[,"Package"]==pckg)
  
################################################################################

## Loads specified libraries. On input, pckgs must either be a character vector 
## of package names or a list of lists where each (sub) list of format list(name=..., src=...) 
## where name is the package name and src is the package source (repository or local) from 
## which the package should be loaded
llibrary = function(pckgs=NULL, method=NULL, quietly=TRUE, character.only=FALSE,
  fail=warn, url_CRAN="https://cloud.r-project.org/") {

  #cat("Loading required packages:\n"); .fc()
  path = getwd()

  if(!character.only) pckgs = as.character(substitute(pckgs))

  ## Check for present pckgs
  #if(is.null(pckgs)) error("Missing library specification!")
  if(is.null(pckgs)) return()
   
  ## Get the currently loaded libraries
  loaded_packages = list_loaded_packages()
  
  ## Make sure pckgs is a list (use CRAN as default source)
  if(is.vector(pckgs)) 
    pckgs = lapply(pckgs, function(x) list(name=x, src="CRAN"))

  ## Load necessary packages
  for(lib in pckgs) {
  
    # Check if package already loaded
    if(any(lib$name==loaded_packages)) next
    # Check if current package really needed (if method supplied)
    if(!is.null(method) && !is.null(lib$method) && !any(lib$method==method)) next

    # Announce loading of the current package
    if(!quietly) cat("Loading package ",lib$name," ...\n", sep=""); .fc()

    # Check if current package is installed
    if(all(lib$name!=installed.packages()[,"Package"])) {
    
      cat("Library '",lib$name,"' seems to be missing! Trying to install it from '",lib$src,"' ...\n", sep="")
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
      msg = paste0("Package '",lib$name,"' was not available at '",
                   if(lib$src=="CRAN") url_CRAN else lib$src,"'.", 
                   " Please install it manually and try again.")
      fail(msg)
      next
    }

    # If this point reached without errors, it is installed, so the package is loaded
    if(!quietly) cat("Loading package '",lib$name,"' ...\n", sep=""); .fc()
    require(lib$name, character.only=TRUE, quietly=quietly)
  
    ### SPECIAL CODE FOR PROJECT 'MultiRegression' ###
    ## Load modified grplasso files
    if(lib$name=="grplasso") {
      path_grplasso = paste0(path,"/source/grplasso/R")
      cat("Looking for grplasso R files in '",path_grplasso,"' ...\n",sep="")
      r_files = list.files(path=path_grplasso, pattern="*.R$", full.names=TRUE)
      if(length(r_files)==0) stop("No *.R files found!")
      if(is.null(method) || method=="grpl2")
        r_files = c(r_files, paste0(path,"/grplasso_modified_l2.R"))
      if(is.null(method) || method=="grplinf")
        r_files = c(r_files, paste0(path,"/grplasso_modified_linf.R"))
      for(r_file in r_files) {
        cat("Sourcing '",r_file,"' (Note: No need to worry about 'masked by .GlobalEnv' warnings) ... \n", sep=""); .fc()
        source(r_file)
      }
    }
    ### SPECIAL CODE FOR PROJECT 'MultiRegression' ###
    
  } # for(lib in pckgs)

} # llibrary

load_libraries = llibrary

################################################################################

## Unloads libraries
unload_library = function(pckgs=NULL, character.only=FALSE) {
  
  if(!character.only) pckgs = as.character(substitute(pckgs))
  
  for(pckg in pckgs) detach(paste0("package:",pckg), character.only=TRUE, unload=TRUE)
  
  return(invisible(TRUE))
  
}

################################################################################

## Samples n variables from an arbitrary density function specified in 'pdf'
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

################################################################################

## Checks if 'dir' ends with a slash and if not it appends slash
slashcheck = function(dir)
  return(ifelse(regexpr("[/\\]$",dir)<0, paste0(dir,"/"), dir))
  
################################################################################

## Checks if dir exists and if not it tries to create it. If that fails, then path './' is returned
check_dir_exist = dir_exist_check = function(dir, stop_if_fail=TRUE, trace=1) {
  if(!dir.exists(dir) && trace>0) 
    cat("Directory '",dir," does not exist and will be created.\n")

  dir.create(dir, showWarnings=FALSE)

  if(!dir.exists(dir)) {
    if(stop_if_fail) error("Directory '",dir,"' could not be created.")
    dir = "./"
  }
  
  return(dir)
}

################################################################################

## A verbose version of file.exists()
file_exists = function(file) {
# Checks if file exists and throws an error if it does not
  for(file1 in file) {
    if(!file.exists(file1))
      error("File '",file1,"' does not exist in path '",getwd(),"'!")
  }
  return(invisible(NULL))
}
check_file_exists = file_exists

################################################################################

## Deletes file
file_remove = function(file, retry=TRUE, nretry=10) {
  for(f in file) {
    for(i in (1:(1+nretry))[c(TRUE,rep(retry, nretry))]) {
      if(file.exists(f)) {
        if(i>1) Sys.sleep(0.05*i)
        msg = tryCatch(file.remove(f), warning=function(w) return(invisible(w)))
        if(i==1+nretry && (!class(msg)=="logical" || !msg)) 
          cat("WARNING: Problem when removing file '",f,"'. (Message: ", msg$message,")\n")
      }
    }
  }
  return(invisible(msg))
}

################################################################################

## Renames files
file_rename = function(from, to, retry=TRUE) {

  todir = dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)

  for(i in (1:2)[c(TRUE,retry)]) {
    if(i==2) Sys.sleep(0.05)
    msg = tryCatch(file.rename(from, to), warning=function(w) return(invisible(w)))
    if(i==1 && (class(msg)!="logical" || !msg)) {
      warn("Problem when renaming file '",from,"' to '",to,"'.", skip1=0, skip2=0)
    } else break
  }
  return(invisible(msg))
  
}

################################################################################

## Moves files in 'files' to path in 'destination'
file_move = function(files, destination, strip.dir=TRUE)
  sapply(files, function(f) file_rename(f, paste0(destination,"/",if(strip.dir) sub(".*[/\\]","",f) else f)))

################################################################################

## Sorts files in 'files' by time or size in descending/ascending order
file_sort = function(files, by=c("time","name","isdir","mode","mtime","ctime","atime","exe"), 
                     decreasing=TRUE, nget=NULL) {
  if(length(files)==0) return(files)
  if(length(by)==0) error("Missing value in 'by'!")
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

################################################################################

## Sorts file names in in 'files' by attribute in 'by' (by default modification time)
file_sort_time = function(files, by="mtime", decreasing=TRUE) {
  if(length(files)==0) return(files)
  files[order(file.info(files)[,"ctime"], decreasing=decreasing)]
}

################################################################################

## Checks if file exists, tries to read it to see whether there is any input data in it
file_empty = function(file) {
  file_exists(file)
  res = try(read.table(file), silent=TRUE)
  return(class(res)=="try-error")
}

################################################################################

## Returns the sizes of files in 'files' in appropriate units
file_size = function(files) {
  s = file.info(files)[,"size"]
  s = convert_unit(s)
  return(s)
}

################################################################################

## Check if file can be open
file_can_open_check = function(filename) {
  do_nothing = function(x) invokeRestart("muffleWarning")
  zz = withCallingHandlers(try(file(filename, open="ab"), silent=TRUE), warning=do_nothing)
  if(all(class(zz) == "try-error")) {
    return(FALSE)
  } else {
    close(zz)
    return(TRUE)
  }
}

################################################################################

## Replaces the values in x with new_values either in the order of occurrence if 
## old_values is missing, or using old_values for reference

replace2 = function(x, new_values, old_values, is_pattern=TRUE) {
  
  if(missing(old_values)) {
    y = as.factor(x)
    if(nlevels(y)!=length(new_values)) error("new_values do not fit x!")
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
    if(length(new_values)!=length(old_values)) 
      error("The lengths of new_values and old_values must match.")
    f = if(is_pattern) regexpr else `==`
    idx = lapply(old_values, function(v) which(f(v, x)>0))
    #print(idx)
    #browser()
    len = unlist(lapply(idx, length))
    val = rep2(new_values, len)
    y[unlist(idx)] = val
  }
  
  return(y)
}

replace2 = plyr::revalue

################################################################################

## Returns the p-value relative to the normal distribution with parameters m, s
normpval = function(x, two.sided=FALSE, m=0, s=1)
 if(two.sided) 2*pnorm(abs(x), m=m, s=s, lower.tail=FALSE) else pnorm(x, m=m, s=s, lower.tail=FALSE)

################################################################################

## Puts authors' names into 'Lastname1, Firstname1 and Lastname2, Firstname2'
## format in a bibtex bibliography file in 'infile', abbreviates the first 
## names with/without a dot and outputs the new bibliography into outfile
## Assumes that records are wrapped by '{' and '}' and not '"'. 
## (Consider removing this limitation)
fix_bibliography = function(infile, outfile, dot=".", lastname_first=TRUE,
  special_words="^[Vv]an$|^[Dd]e$|^[Dd]er$|^[Dd]os$") {

  # Read the bibliography
  cat("Reading bibliography from file '",infile,"' ...\n", sep="")
  x = scan(infile, what='character', sep='\n', blank.lines.skip=FALSE)

  # Loop through the author lines to process them
  cat("Processing bibliography ...\n")
  for(k in 1:length(x)) {

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
    for(i in 1:length(n)) {
      
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
        #if(tail(a,1)=="Manolio") { print(n); browser() }
        #if(tail(a,1)=="others") { print(n); browser() }
      } else {
        n[i] = paste(paste(head(a, -1), collapse=""), tail(a,1))
      }
    }
    
    # Paste the author line back together
    x[k] = paste0(s1,"{",paste(n, collapse=" and "),"},")
    
    #break

  }

  # Update the bibliography and save it to a file
  #x[w] = y
  x = paste(x, collapse="\n")

  cat("Saving bibliography to file '",outfile,"' ...\n", sep="")
  cat(x, file=outfile)
  
}

#############################################################

## Calculates autocorrelation of x with given lag
acor = function(x, lag=1) 
  suppressWarnings(cor(x[-((length(x)-lag+1):length(x))], x[-(1:(lag))]))

#############################################################

## Returns an ascii table code of a character in 'x'
asc = function(x) strtoi(charToRaw(x),16L)

################################################################################

#assign("cat", cat0, envir=.GlobalEnv)
#assign("cat", cat0, envir=.env)

#cat = utils::getFromNamespace("cat", ns="base")
#trace(cat, tracer=quote(if(missing(sep)) sep <- ""), at=1, print=FALSE)
##utils::assignInNamespace("cat", cat0, ns="base")

########################################################################

combine_pvalues = function(P, lam, method="jelle", summarize=FALSE, ...) {

  # Make sure P is a matrix
  if(is.vector(P)) P = matrix(P, nrow=1, ncol=length(P))

  # Report on input
  if(summarize) {
    cat("Smallest raw p-value: ",min(P),"\n", sep=""); .fc()
    w = which.min(apply(log10(P), 1, mean))
    cat("Row with smallest average p-values: ",w,"\n", sep=""); .fc()
    print(P[w,])
    c1 = do_pval_comb(P[w,], method="fisher")
    cat("Fisher combination of these p-values: ",c1,"\n", sep=""); .fc()
    c2 = do_pval_comb(P[w,], method="jelle")
    cat("Jelle combination of these p-values: ",c2,"\n", sep=""); .fc()
  }
  
  # Perform the p-value combination
  PP = do_pval_comb(P, lam, method, ...)
  
  return(PP)

}

#########################################################################

combine_fisher = function(p, silent=TRUE)
  do_pval_comb(p, method="fisher", trace=1*!silent)

#########################################################################

combine_stouffer = function(p, silent=TRUE)
  do_pval_comb(p, method="stouffer", trace=1*!silent)

#########################################################################

#==========================================================
# Code by Jelle Goeman. Calculates the asymptotic p-value 
# using methods of Kotz, Johnson and Boyd (1967) and
# Box (1954)
#==========================================================

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

qchisqw = function(p, w, lower.tail=TRUE, epsw=1e-5, epsv=1e-12, df0=2) {

  if(any(w<0)) error("Negative weights!")
  
  # Check the input
  np = length(p)
  if(np==1) {
    w = matrix(w, nrow=1)
  } else if(is.vector(w)) {
    w = matrix(rep(w, e=np), nrow=np)
  }
  stopifnot(is.matrix(w), nrow(w)==np)
  
  nw = ncol(w)
  #nw = apply(w>0, 1, sum)

  # Get the chisquare quantile
  q0 = qchisq(p, df=df0*nw, lower.tail=FALSE) * apply(w, 1, mean)
  
  # If weights are different, use pchisqw
  if(diff(range(w))<epsw) {
    x = q0
  } else {
    logp = log(p)
    fun = function(x) (log(pchisqw(x, w=w, lower.tail=lower.tail, epsw=epsw, df0=df0)) - logp)^2
    value = 1
    iter = 0
    max_iter = 10
    while(value>epsv && iter<max_iter) {
      iter = iter+1
      x = optim(q0, fun, method="Brent", lower=0, upper=5*q0)
      value = x$value
    }
    if(value>epsv) error("Non-convergent optimization in qchisqw() (value ",value,")!")
    x = x$par
  }
  
  return(x)
    
}

#########################################################################

pchisqw = function(x, w, lower.tail=TRUE, epsw=1e-5, df0=2) {
# Calculates the probability of a weighted sum of chisquare-2 variables
# being below (lower.tail==TRUE) or above (lower.tail=FALSE) x, where
# w specifies the weights. Fails if some (but not all) weights are equal

  nx = length(x)
  if(nx==1) {
    w = matrix(w, nrow=1)
  } else if(is.vector(w)) {
    w = matrix(rep(w, e=nx), nrow=nx)
  }
  stopifnot(is.matrix(w), nrow(w)==nx)
  
  nw = ncol(w)
  #nw = apply(w>0, 1, sum)
  
  # If all equal, use standard chisquare distribution function
  if(diff(range(w))<epsw) {
  
    p = pchisq(x, df=df0*nw, lower.tail=lower.tail)
  
  # Otherwise use Box (1953), Theorem 2.4
  } else {

    # Calculate the probability of exceeding x
    wx = x / w
    px = pchisq(wx, df=df0, lower.tail=FALSE)
    a = sapply(1:nx, function(i) sapply(1:nw, function(j) prod(w[i,j]/(w[i,j]-w[i,-j]))))
    p = apply(a * t(px), 2, sum)
    
    # Get the lower tail probability (i.e. not exceeding x)
    if(lower.tail) p = 1. - p
    
  }
  
  return(p)
  
}      

#########################################################################

do_pval_comb = function(P, lam=1, method="jelle", eps_p=1e-3, fac_lam=1e-2, 
  Debug=FALSE, na.rm=TRUE, trace=1) {
# Performs combination of p-values using Fisher method or Box method (the
# latter via Jelle's code in .pAsymptotic or Jakub's code below

  # Check for unknown combination method
  if(all(method!=c("fisher","stouffer","jakub","jelle")))
    error("Unknown combination method '",method,"'!")
  
  # Replace non-sensical p-values with NA
  if(trace>0) cat("Checking for NA values ...\n")
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
  
  # Drop columns of P with lam much smaller than the largest value
  #keep = abs(lam)/max(abs(lam))>fac_lam
  #P = P[,keep,drop=FALSE]
  #lam = lam[keep]
  
  #browser()
  
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
    
    # Use Jelle's general implementation (slow)
    #if(T|| (method=="Jelle" || (!is.na(mdif) && mdif<5e-5))) {
    if(tolower(method)=="jelle") {

      if(trace>0) cat("Combining p-values (",nrow(chi2)," sets) via Box's method (with weighing, Jelle's code) ...\n", sep="")
     
      if(!exists(".pAsymptotic", mode="function"))
        error("Missing function .pAsymptotic()! Look for Jelle's script that",
              " defines the function.")
        
      PP = sapply(1:np, function(i) .pAsymptotic(X0[i], lams=rep(mat_lam[i,], t=2), accuracy=1e-100))

    # Use Jakub's less general implementation (fast)
    } else {
      
      if(trace>0) cat("Combining p-values (",nrow(chi2)," sets) via Box's method (with weighing, Jakub's code) ...\n", sep="")
      
      # If weights are too close, randomize them
      mindif = 0 #.005*diff(range(lam))
      mdif = apply(mat_lam, 1, min_dif)
      #print(summary(mdif))
      if(any(mdif<=mindif)) {
        if(trace>0) cat("Randomizing weights ...\n")
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
      #print(summary(mdif))
      if(any(mdif==0))
        error("Some weights are equal, which is not allowed with Jakub's code.", Q=!interactive())
      
      Q = X0 / mat_lam
      PQ = pchisq(Q, df=2, lower.tail=FALSE)

      #s = sapply(1:nres, function(j) prod(sign(lam[j]-lam[-j])))
      #b = sapply(1:nres, function(j) exp(sum(log(lam[j])-log(abs(lam[j]-lam[-j])))))
      #a = b*s

      if(same_lam) {
        #a = sapply(1:nres, function(j) prod((1-lam[-j]/lam[j])^(-1)))
        a = sapply(1:nres, function(j) prod(lam[j]/(lam[j]-lam[-j])))
      } else {
        a = sapply(1:np, function(i) sapply(1:nres, function(j) prod(mat_lam[i,j]/(mat_lam[i,j]-mat_lam[i,-j]))))
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
    PP[wn] = sapply(1:length(wn), function(i) 
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
  if(trace>0) cat("Smallest combined p-value: ",min(PP, na.rm=TRUE),"\n", sep="")
  
  # Compare the output of Jakub's and Jelle's codes for small p-values
  eps = 1e-6
  if(FALSE && any(PP<eps, na.rm=TRUE)) {
    cat("Checking results for small p-values (",sum(PP<eps),
        " p-values below ",eps,") ...\n", sep=""); .fc()
    ws = which(PP<eps)
    PP2 = sapply(X0[ws], .pAsymptotic, lams=rep(lam, t=2), accuracy=1e-100)
    cat("Smallest p-value found after re-check: ",min(PP2),"\n", sep=""); .fc()
    md = max(abs(PP[ws]-PP2))
    cat("Maximum difference between Jakub's and Jelle's code: ",md,"\n", sep="")
    mrd = max(abs((PP[ws]-PP2)/unzero(PP[ws]+PP2)*2-1))
    cat("Maximum relative difference between Jakub's and Jelle's code: ",mrd,"\n", sep="")
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

################################################################################

bootstrap = function(x, T, B = 100., ...) {
# Returns a vector of B bootstrap values of real-valued statistic T, where T 
# should be an R-function which returns a scalar. Arguments of T can be supplied
# via the ellipsis '...'
  
  # If length of x and B are small enough, do it all at once
  if(length(x)*B<1e8) {
    X = matrix(sample(x, length(x)*B, replace = TRUE), nrow=length(x))
    y = apply(X, 2, T, ...)
  # Otherwise, do it for each iteration separately
  } else {
    y = sapply(1:B, function(i) T(sample(x, replace = TRUE), ...))
  }
  return(y)
}

################################################################################

to_base_k = function(x, k=3)
  c(if(x>=k) to_base_k(x %/% k, k) else NULL, x %% k)  
  
int2k = Vectorize(to_base_k)

################################################################################

} # if(identical(environment(), .env)) ... else

