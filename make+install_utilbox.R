#while(any(search()=="package:utilbox")) detach("package:utilbox"); source("d:/Dropbox/Projects/R/utilbox/make+install_utilbox.R"); require(utilbox)

####################################################################################

## Set the 'utilbox' package name, its directory and the location of source files ##
package = list(name="utilbox", dir="utilbox", filedir="source")

do_update_documentation = TRUE
do_quick_install = FALSE

####################################################################################

## Function that returns the path to the script file from which it is called
find_script_dir = function() {
  if("ofile" %in% names(sys.frame(1))) {
    dirname(sys.frame(1)$ofile)
  } else {
    "."
  }
}

## Load required packages. First check if the packages are installed 
## and if not, install them (on Windows, make sure Rtools is installed)
avail_pckg = installed.packages()[,"Package"]
for(p in c('devtools','roxygen2')) {
  if(! p %in% avail_pckg) {
    cat("Installing '",p,"' ...\n"); install.packages(p)
  }
  library(p, character.only=TRUE)
}

path = find_script_dir() 	# e.g. 'd:/Dropbox/Projects/R/utilbox/'

cat("Working path:",path,"\n")

if(path=='.')
  stop("Something is probably wrong with the identification of the path. I better stop here.")

setwd(path)

if(do_quick_install) {
  cat("\nRUNNING A QUICK INSTALLATION...\n\n"); flush.console()
}

#file.remove(package$dir)

## Make sure the package directory exists
cat("Creating basic directory structure for the package...\n")
if(!dir.exists(package$dir)) create(package$dir)

## Delete all existing files in the package
cat("Deleting any old files that might be present...\n")
files = list.files(package$dir, full.names=TRUE, recursive=TRUE)
files = files[!file.info(files)$isdir]
if(!do_update_documentation) files = files[regexpr('/man/',files)<=0]
if(do_quick_install) files = files[regexpr('NAMESPACE',files)<=0]
if(length(files)>0) sapply(files, file.remove)

## Copy all of the source files to the package directory
cat("Copying source files to the package R directory...\n")
files = list.files(pattern='^.*[.]R$', path=package$filedir, full.names=TRUE)
for(f in files) {
  file.copy(f, paste0(package$dir,"/R/",sub('.*/','',f)), overwrite=TRUE)
}

cat("Placing the description file into the package directory...\n")
file.copy('DESCRIPTION', package$dir)

## Create documentation
if(do_update_documentation) {
  cat("Creating documentation...\n")
  setwd(package$dir)
  document()
  setwd("..")
} else {
  cat("\nUPDATING OF DOCUMENTATION SKIPPED!\n\n"); flush.console()
}

cat("Package created.\n")

## Build the package
cat("Building the package...\n")
build(package$name, path='build')

## Install the package
cat("Installing the package...\n")
install(package$name, quick=do_quick_install)

cat("Adding the package's source path variable to .GlobalEnv and to .Rprofile so that it is set at R startup ...\n")
package_complete_path = file.path(path, package$filedir)
startup_code = paste0("assign('.",package$name,"_source_path', '", package_complete_path, "', envir=.GlobalEnv)")
assign('.utilbox_source_path', package_complete_path, envir=.GlobalEnv)
R_del_code_startup(substr(startup_code, 1, 30))
R_add_code_startup(startup_code)

cat("Cleaning up...\n")
rm(package, path, files)
cat("Finished.\n")

if(!do_update_documentation) cat("\nWARNING: DOCUMENTATION NOT UPDATED.\n\n")