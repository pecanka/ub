#source("d:/Dropbox/Projects/R/populate_env.R")

################################################################################
########################### DEFINITION OF FUNCTIONS ############################
################################################################################

# This script is meant to be sourced into R (ideally at launch) from within the
# global environment (.GlobalEnv) to define various utility functions. When the
# script is sourced inside the global environment, after changing the R local 
# library path and alters several options via options(), it creates the environment
# '.utilbox' and it recursively sources itself within the environment '.utilbox', which 
# executes the bottom portion of the code (where utility functions are defined)
# inside '.utilbox'. After '.utilbox' is populated with functions, it is attached.

populate_env = function(file="utilitybox.R", this_file="populate_env.R") {

  ## Set high penalization for scientific notation, turn off reading of
  ## strings from files as factors, set the default repository
  options(scipen=5)
  options(stringsAsFactors=FALSE)
  options("repos"=c(CRAN="https://cran.rstudio.com/"))

  ## Identify the file in 'this_file' and extract its path to source the file 'file'
  calls = sapply(sys.calls(), function(cl) as.character(cl[2]))
  w = regexpr(this_file, calls)>0
  if(!any(w)) {
    cat("Cannot identify the file '",this_file,"' on the call stack.\n")
	return(FALSE)
  }
  this_file_full_name = sub(paste0(this_file,"$"), file, calls[w])
  cat("Populating environment '.utilbox' by sourcing the file '",this_file_full_name,"' ...\n", sep="")
  sys.source(this_file_full_name, envir=get(".utilbox", envir=.GlobalEnv))

  ## Attach the environment which was populated with the functions defined below
  while(any(search()==".utilbox")) detach(".utilbox")
  cat("Attaching environment '.utilbox' ...\n", sep="")
  attach(.utilbox)

}

## Create a new invisible environment for all the functions to go in so it doesn't clutter 
## the workspace (i.e. .GlobalEnv).
assign(".utilbox", new.env(), envir=.GlobalEnv)
 
## If this code is not being evaluated inside the environment '.utilbox', set it up for such 
## evaluation and call evaluate (i.e. source) it inside '.utilbox' (see sys.source() call below)
populate_env()

rm(populate_env)