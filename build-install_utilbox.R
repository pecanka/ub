#while('package:utilbox'%in%search()) detach('package:utilbox'); source('d:/Dropbox/Projects/R/utilbox/build-install_utilbox.R'); require(utilbox)

####################################################################################

## Set the 'utilbox' package name, its directory and the location of source files ##
package = list(name='utilbox', dir='utilbox', filedir='source')

do_update_documentation = TRUE
do_quick_install = FALSE
which_version_increase = 'minor3'   # select one one major / minor1 / minor2 / minor3

####################################################################################

show_msg = function(...) {
  base::message(...)
  flush.console()
}

## Function that returns the path to the script file from which it is called
find_script_dir = function() {
  if('ofile' %in% names(sys.frame(1))) {
    dirname(sys.frame(1)$ofile)
  } else {
    '.'
  }
}

## Load required packages. First check if the packages are installed 
## and if not, install them (on Windows, make sure Rtools is installed)
avail_pckg = installed.packages()[,'Package']
for(p in c('devtools','roxygen2')) {
  if(! p %in% avail_pckg) {
    show_msg("Installing '",p,"' ..."); install.packages(p)
  }
  library(p, character.only=TRUE)
}

version_increase = function(version, check_current_version_exists=TRUE) {
  
  file_build_latest = list.files('build', pattern=version)
  if(check_current_version_exists && length(file_build_latest)==0) {
    return(version)
  }
  
  version_next = as.numeric(unlist(strsplit(version, '[.]')))
  w = switch(which_version_increase, 'major'=1, 'minor1'=2, 'minor2'=3, 'minor3'=4)
  version_next[w] = version_next[w] + 1
  
  if(w<4) 
    version_next[(w+1):4] = 0
    
  paste(version_next, collapse='.')
}

####################################################################################

path = find_script_dir() 	# e.g. 'd:/Dropbox/Projects/R/utilbox/'

show_msg("Working path:",path,"")
if(path=='.') stop("Something is probably wrong with the identification of the path. I better stop here.")
setwd(path)

if(do_quick_install) {
  show_msg("RUNNING A QUICK INSTALLATION..."); flush.console()
}

#file.remove(package$dir)

## Make sure the package directory exists
show_msg("Creating basic directory structure for the package...")
if(!dir.exists(package$dir)) create(package$dir)

## Delete all existing files in the package
show_msg("Deleting any old files that might be present...")
files = list.files(package$dir, full.names=TRUE, recursive=TRUE)
files = files[!file.info(files)$isdir]                                  # keep directories
if(!do_update_documentation) files = files[!grepl('/man/',files)]       # keep man files if documentation is not to be updated
if(do_quick_install) files = files[!grepl('NAMESPACE',files)]           # keep the NAMESPACE file on quick install
if(length(files)>0) sapply(files, file.remove)                          # remove what's left in 'files'

## Copy all of the source files to the package directory
show_msg("Copying source files to the package R directory...")
files = list.files(pattern='^.*[.]R$', path=package$filedir, full.names=TRUE)
dir = paste0(package$dir,"/R/")
if(!dir.exists(dir)) dir.create(dir)
for(f in files) {
  stopifnot(file.copy(f, paste0(dir,sub('.*/','',f)), overwrite=TRUE))
}

show_msg("Placing the description file into the package directory ...")
stopifnot(file.copy('DESCRIPTION', package$dir, overwrite=TRUE))

show_msg("Updating version info in the DESCRIPTION file ...")
file_DESC = file.path(package$dir,'DESCRIPTION')
DESC = readLines(file_DESC)
is_version = grep('Version: ',DESC)
version_prev = sub('.*[ ]','',DESC[is_version])
version_next = version_increase(version_prev)

DESC[is_version] = sub('[ ].*',paste0(' ',version_next),DESC[is_version])
show_msg('... from version ',version_prev,' to ',version_next)
writeLines(DESC, file_DESC)
file.copy(file.path(package$dir,'DESCRIPTION'), '.', overwrite=TRUE)

## Create documentation
if(do_update_documentation) {
  show_msg("Creating documentation...")
  setwd(package$dir)
  document()
  setwd("..")
} else {
  show_msg("UPDATING OF DOCUMENTATION SKIPPED!"); flush.console()
}

show_msg("Package created.")

## Build the package
show_msg("Building the package ...")
build(package$name, path='build')

show_msg("Updating the 'latest' package file ...")
files = file.info(list.files('build', full.names=TRUE))
file_latest = rownames(files)[which.max(files$mtime)]
file.copy(file_latest, sub(version_next, 'latest', file_latest, fixed=TRUE), overwrite=TRUE)

## Install the package
show_msg("Installing the package...")
install(package$name, quick=do_quick_install)

# Copy the newly created NAMESPACE file 
if(!do_quick_install) {
  file.copy(file.path(package$dir,'NAMESPACE'), '.', overwrite=TRUE)
}

show_msg("Adding the package's source path variable to .GlobalEnv and to .Rprofile so that it is set at R startup ...")
package_complete_path = file.path(path, package$filedir)
startup_code = paste0("assign('.",package$name,"_source_path', '", package_complete_path, "', envir=.GlobalEnv)")
assign('.utilbox_source_path', package_complete_path, envir=.GlobalEnv)
utilbox::R_del_code_startup(substr(startup_code, 1, 30))
utilbox::R_add_code_startup(startup_code)

show_msg("Cleaning up...")
rm(package, path, files)
show_msg("Finished.")

if(!do_update_documentation) 
  show_msg("WARNING: DOCUMENTATION NOT UPDATED.")
  
 