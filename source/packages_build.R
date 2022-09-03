#' Build a package file
#'
#' `build_package()` takes a source code of a package and created the installation file
#' for the package using the devtools package. Depending on the way it is called, the 
#' the package version in the DESCRIPTION file is automatically increased (controlled by
#' `increase_version`), the documentation is updated (when `update_doc = TRUE`) using the
#' roxygen2 package (via `devtools::document()`), the installation file is build (when
#' `build = TRUE`) and the package is installed (regularly when `install_mode = 'yes'`,
#' or quickly when `install_mode = 'quick'`). Optionally, the package installation path
#' is added to .GlobalEnv and to the .Rprofile file.
#'
#' `build_utilbox()` calls `build_package()` using the settings for utilbox for a complete
#' creation (i.e., create, update documentation, build, install, attach)
#'
#' `install_utilbox()` calls `build_package()` using the settings for utilbox.
#'
#' @export
build_package = function(pckg_name, pckg_dir = pckg_name, pckg_dir_source = 'source', path = '.', 
    create = TRUE, update_doc = TRUE, build = TRUE, install_mode = c('no','yes','quick'), 
    increase_version = c('minor3','minor2','minor1','major','none'), add_path_to_global = TRUE, 
    attach = TRUE, install_prerequisities = TRUE) {

  msg = function(...) {
    base::message(...)
    flush.console()
  }
  
  version_increase = function(version, check_current_version_exists=TRUE) {
    
    file_build_latest = list.files('build', pattern=version)
    if(check_current_version_exists && length(file_build_latest)==0) {
      return(version)
    }
    
    version_next = as.numeric(unlist(strsplit(version, '[.]')))
    w = switch(increase_version, 'major'=1, 'minor1'=2, 'minor2'=3, 'minor3'=4)
    version_next[w] = version_next[w] + 1
    
    if(w<4) 
      version_next[(w+1):4] = 0
      
    return(paste(version_next, collapse='.'))
    
  }

  install_mode = match.arg(install_mode)
  increase_version = match.arg(increase_version)
  
  orig_dir = getwd()
  on.exit(orig_dir)
  setwd(path)
  path = normalizePath(getwd())
  
  package = list(name = pckg_name, dir = pckg_dir, dir_source = pckg_dir_source)
  
  dir_base = normalizePath(package$dir, mustWork = FALSE)
  dir_src = normalizePath(package$dir_source, mustWork = FALSE)
  dir_R = normalizePath(paste0(package$dir,"/R/"), mustWork = FALSE)   
  
  msg("CREATING PACKAGE '", package$name, "' ...")
  msg("Installation path: ", path)
  msg("Package path: ", dir_base)
  
  if(!nzchar(package$dir))
    stop('Specify non-empty package directory.')
    
  if(identical(path, dir_base))
    stop('The package path and the installation path must be different.')

  if(grepl(dir_base, dir_src, fixed = TRUE))
    stop("Source code directory (",dir_src,") must not be inside the package path (",dir_base,").")
    
  unload = try(detach(paste0('package:',package$name), character.only = TRUE, unload = TRUE), silent = TRUE)

  avail_pckg = installed.packages()[,'Package']
  for(pckg in c('devtools','roxygen2')) {
    if(! pckg %in% avail_pckg) {
      if(install_prerequisities) {
        msg("Package '",pckg,"' is required. Installing it ...")
        install.packages(pckg)
      } else {
        stop("Package '",pckg,"' is required. Please install it or set `install_prerequisities = TRUE`.")
      }
    }
    #library(pckg, character.only = TRUE)
  }

  msg("Installation mode: ", switch(install_mode, 'no' = 'NOT INSTALLED', 'yes' = 'REGULAR', 'quick' = 'QUICK'))
  
  if(create) {
  
    if(!dir.exists(package$dir)) {
      msg("Creating package directory '",dir_base,"' ...")
      dir.create(package$dir)
    }
    
    msg("Checking for any existing files in '", dir_R, "' ...")
    files = list.files(package$dir, full.names=TRUE, recursive=TRUE)
    files = files[!file.info(files)$isdir]
    
    if(!update_doc) {
      files = files[!grepl('/man/',files)]
    }
    
    if(install_mode == 'quick') {
      files = files[!grepl('NAMESPACE',files)]
    }
    
    if(length(files)>0) {
      msg("Deleting any exiting files in '",package$dir,"' that might be present...")
      sapply(files, file.remove)
    }

    msg("Copying source files from '",dir_src,"' to '", dir_R, "' ...")
    
    if(!dir.exists(dir_R)) {
      dir.create(dir_R)
    }
    
    files = list.files(pattern='^.*[.]R$', path = package$dir_source, full.names = TRUE)
    
    if(length(files)==0)
      stop("No *.R files found in the path '",dir_src,"'.")
    
    for(f in files) {
      stopifnot(file.copy(f, dir_R), overwrite = TRUE)
    }
    
    msg(length(files), " copied.")

    msg("Placing the description file into the package directory '",dir_base,"' ...")
    
    if(!file.exists('DESCRIPTION'))
      stop("The description file '",file.path(getwd(),'DESCRIPTION'),"' does not exist. Please create it.")
    
    stopifnot(file.copy('DESCRIPTION', package$dir, overwrite=TRUE))

    if(increase_version == 'none') {
      msg("Package version not updated.")
    } else {

      msg("Updating version info in the file 'DESCRIPTION' ...")
    
      file_DESC = file.path(package$dir,'DESCRIPTION')
      DESC = readLines(file_DESC)
      is_version = grep('Version: ',DESC)
      version_prev = sub('.*[ ]','',DESC[is_version])
      version_next = version_increase(version_prev)

      DESC[is_version] = sub('[ ].*',paste0(' ',version_next),DESC[is_version])
      msg('... from version ',version_prev,' to ',version_next)
      writeLines(DESC, file_DESC)
      file.copy(file.path(package$dir,'DESCRIPTION'), '.', overwrite=TRUE)
      
    }

    msg("Package created.")
    
  } else {
    msg("PACKAGE NOT BEING CREATED.")
  }

  if(update_doc) {
  
    msg("Creating documentation...")
    
    setwd(package$dir)
    devtools::document()
    setwd(path)
    
  } else {
    msg("DOCUMENTATION NOT BEING CREATED / UPDATED.")
  }

  if(build) {
  
    msg("Building the package ...")
    devtools::build(package$name, path='build')
    msg("Package built.")

    msg("Updating the 'latest' package file ...")
    files = file.info(list.files('build', full.names=TRUE))
    file_latest = rownames(files)[which.max(files$mtime)]
    file.copy(file_latest, sub(version_next, 'latest', file_latest, fixed=TRUE), overwrite=TRUE)
    
  } else {
    msg("PACKAGE NOT BEING BUILT.")
  }

  if(install_mode == 'no') {
  
    msg('NEWLY BUILT PACKAGE IS NOT BEING INSTALLED.')
    
  } else {
  
    msg("Installing the package...")
    devtools::install(package$name, quick = install_mode == 'quick')

    if(install_mode == 'quick') {
      file.copy(file.path(package$dir,'NAMESPACE'), '.', overwrite=TRUE)
    }

    msg("Installation finished.")
    
    msg("Checking installation ...")
    if(! 'utilbox' %in% installed.packages()[,'Package']) {
      warning("Package '",utilbox,"' not found among the installed packages.")
    } else {
      msg("Installation check successful.")
    }
    
  }
  
  if(attach) {
    library(pckg_name, character.only = TRUE)
  }
  
  if(add_path_to_global) {
  
    msg("Adding the package's source path variable to .GlobalEnv and to .Rprofile so that it is set at R startup ...")
    
    file_Rprofile = normalizePath(file.path(Sys.getenv("HOME"), ".Rprofile"), mustWork = FALSE)
    package_complete_path = file.path(path, package$dir_source)
    startup_code = paste0("assign('.",package$name,"_source_path', '", package_complete_path, "', envir=.GlobalEnv)")
    assign('.utilbox_source_path', package_complete_path, envir = .GlobalEnv)
    
    utilbox::R_del_code_startup(substr(startup_code, 1, 30), file_Rprofile)
    utilbox::R_add_code_startup(startup_code, file_Rprofile)
    
  }

}

#' @rdname build_package
#' @export
build_utilbox = function(pckg_name = 'utilbox', pckg_dir = 'utilbox', pckg_dir_source = 'source', 
    path = '.', create = TRUE, update_doc = TRUE, build = TRUE, install_mode = 'yes', attach = TRUE,
    add_path_to_global = TRUE, increase_version = 'minor3', install_prerequisities = TRUE) {
    
  build_package(pckg_name = pckg_name, pckg_dir = pckg_dir, pckg_dir_source = pckg_dir_source, 
                path = path, create = create, update_doc = update_doc, build = build, 
                install_mode = install_mode, add_path_to_global = add_path_to_global, 
                increase_version = increase_version, attach = attach,
                install_prerequisities = install_prerequisities)
    
}

#' @rdname build_package
#' @export
install_utilbox = function(pckg_name = 'utilbox', pckg_dir = 'utilbox', pckg_dir_source = 'source', 
    path = '.', create = FALSE, update_doc = FALSE, build = FALSE, install_mode = 'yes', attach = FALSE,
    add_path_to_global = FALSE, install_prerequisities = TRUE) {
    
  build_package(pckg_name = pckg_name, pckg_dir = pckg_dir, pckg_dir_source = pckg_dir_source, 
                path = path, create = create, update_doc = update_doc, build = build, 
                install_mode = install_mode, add_path_to_global = add_path_to_global, 
                attach = attach, install_prerequisities = install_prerequisities)

    
}
