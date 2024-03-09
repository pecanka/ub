#while('package:ub'%in%search()) detach('package:ub'); source('d:/Dropbox/Projects/R/ub/build-install_ub.R'); require(ub)

####################################################################################

## Set the 'ub' package name, its directory and the location of source files ##
package = list(name='ub', dir='ub', filedir='source')

do_update_documentation = TRUE
do_quick_install = FALSE
which_version_increase = 'minor3'   # select one one major / minor1 / minor2 / minor3

####################################################################################

## Messaging function with flushing of buffer
show_msg = function(...) {
  base::message(...)
  flush.console()
}

## Function for automatic increase of package version
version_increase = function(version, check_current_version_exists=TRUE) {
  
  file_build_latest = list.files('builds', pattern=version)
  if(check_current_version_exists && length(file_build_latest)==0) {
    return(version)
  }
  
  version_next = as.numeric(unlist(strsplit(version, '[.]')))
  w = switch(which_version_increase, 'major'=1, 'minor1'=2, 'minor2'=3, 'minor3'=4)
  version_next[w] = version_next[w] + 1
  
  if(w<4) {
    version_next[(w+1):4] = 0
  }
  
  paste(version_next, collapse='.')
}

####################################################################################

(function() {

  ## Load required packages. First check if the packages are installed 
  ## and if not, install them (on Windows, make sure Rtools is installed)
  avail_pckg = installed.packages()[,'Package']
  for(p in c('this.path','devtools','roxygen2')) {
    if(! p %in% avail_pckg) {
      show_msg("Installing '", p, "' ...")
      install.packages(p, repos = "https://cloud.r-project.org/")
    }
    show_msg("Loading package '", p, "' ...")
    suppressWarnings(suppressPackageStartupMessages(
      library(p, character.only=TRUE, warn.conflicts = FALSE)
    ))
  }

  ## Set the working directory to the location of this script
  path = this.path::this.dir()
  odir <- setwd(path)
  on.exit(setwd(odir))

  if(do_quick_install) {
    show_msg("RUNNING A QUICK INSTALLATION ...")
  }

  #file.remove(package$dir)

  ## Make sure the package directory exists
  show_msg("Creating basic directory structure for the package ...")
  if(!dir.exists(package$dir)) create(package$dir)

  ## Delete all existing files in the package
  show_msg("Deleting any old files that might be present ...")
  files = list.files(package$dir, full.names=TRUE, recursive=TRUE, include.dirs = FALSE)
  if(!do_update_documentation) files = files[!grepl('/man/',files)]       # keep man files if documentation is not to be updated
  if(do_quick_install) files = files[!grepl('NAMESPACE',files)]           # keep the NAMESPACE file on quick install
  if(length(files)>0) sapply(files, file.remove)                          # remove what's left in 'files'

  ## Copy all of the source files to the package directory
  show_msg("Copying source files to the package R directory ...")
  files = list.files(pattern='^.*[.]R$', path=package$filedir, full.names=TRUE)
  dir = paste0(package$dir,"/R/")
  if(!dir.exists(dir)) dir.create(dir)
  for(f in files) {
    stopifnot(file.copy(f, paste0(dir,sub('.*/','',f)), overwrite=TRUE))
  }

  ## Put the description file into the package directory and update the version info
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
    show_msg("Creating documentation ...")
    setwd(package$dir)
    document()
    setwd("..")
  } else {
    show_msg("UPDATING OF DOCUMENTATION SKIPPED!"); flush.console()
  }

  show_msg("Package created.")

  ## Build the package
  show_msg("Building the package ...")
  build(package$name, path='builds')
  show_msg("Updating the 'latest' package file ...")
  files = file.info(list.files('builds', full.names=TRUE))
  file_latest = rownames(files)[which.max(files$mtime)]
  file.copy(file_latest, sub(version_next, 'latest', file_latest, fixed=TRUE), overwrite=TRUE)

  ## Install the package
  show_msg("Installing the package ...")
  install(package$name, quick=do_quick_install)

  ## Copy the newly created NAMESPACE file 
  if(!do_quick_install) {
    invisible(file.copy(file.path(package$dir,'NAMESPACE'), '.', overwrite=TRUE))
  }

  ## Modify the .Rprofile file
  show_msg("Adding the package's source path variable to .GlobalEnv and to .Rprofile so that it is set at R startup ...")
  package_complete_path = file.path(path, package$filedir)
  startup_code = paste0("assign('.",package$name,"_source_path', '", package_complete_path, "', envir=.GlobalEnv)")
  assign('.ub_source_path', package_complete_path, envir=.GlobalEnv)
  ub::R_del_code_startup(substr(startup_code, 1, 30))
  ub::R_add_code_startup(startup_code)
  show_msg("Finished.")

  if(!do_update_documentation) {
    show_msg("WARNING: DOCUMENTATION NOT UPDATED.")
  }
  
  invisible()
    
})()
