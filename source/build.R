#' Build a package file
#'
#' `build_package()` takes a source code of a package and created the installation file
#' for the package using the devtools package. Depending on the way it is called, the 
#' the package version in the DESCRIPTION file is automatically increased (controlled by
#' `increase_version`), the documentation is updated (when `update_doc = TRUE`) using the
#' roxygen2 package (via `devtools::document()`), the installation file is build (when
#' `build = TRUE`) and the package is installed (regularly when `install_mode = 'full'`,
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
    create = TRUE, update_doc = TRUE, build = TRUE, install_mode = c('quick', 'full', 'no'), 
    file_desc = 'DESCRIPTION', increase_version = c('minor3', 'minor2', 'minor1', 'major', 'none'), 
    add_path_to_global = TRUE, attach = TRUE, install_prerequisities = TRUE) {

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
  
  create_description_file = function(file, name = NULL, title = 'Short Description', version = '0.0.0.1', 
      author = 'Doe, John',email = 'email@address.com', description = '<add package description>', url = '', 
      licence = 'GPL-3', encoding = 'UTF-8', depends = paste(unlist(R.version[c('major','minor')]), collapse='.'), 
      suggests = NULL, imports = NULL, roxygen_note = utils::packageVersion("roxygen2")) {
    
    while(is.null(name)) { 
      name = readline('Package name (must be a valid package name): ')
      if(!grepl('^[a-zA-Z]+$', name))
        name = NULL
    }
  
    if(is.null(title))
      title = readline('Title (What the Package Does, One Line, Title Case): ')
  
    if(is.null(version))
      version = readline('Version: ')

    while(is.null(author)) {
      author = readline('Author (Last name, First name(s)): ')
      if(!grepl('^[^,]+,[^,]+$', author))
        author = NULL
    }
      
    if(is.null(description))
      description = readline('Description: ')
      
    if(is.null(licence))
      licence = readline('License type: ')
      
    if(is.null(encoding))
      encoding = readline('Encoding: ')
      
    if(is.null(url))
      url = readline('URL: ')
      
    if(is.null(depends))
      depends = readline('Depends: ')
      
    author = strsplit(author, split = ',')
    author = lapply(author, gsub, pattern = '^\\s+|\\s+$', replacement = '')[[1]]

    desc = c(
      'Package: ', name, '\n',
      'Title: ', title, '\n',
      'Version: ', version, '\n',
      'Authors@R: \n',
      '   person(given = "',author[1],'",\n',
      '          family = "',author[2],'",\n',
      '          role = c("aut", "cre"),\n',
      '          email = "',email,'",\n',
      '          comment = "")\n',
      'Description: ', description, '\n',
      'License: ', licence, '\n',
      'Encoding: ', encoding, '\n',
      'URL: ', url, '\n',
      'BugReports: \n',
      'Depends: \n',
      '  R (>= ', depends, ')\n', 
      'Roxygen: list(markdown = TRUE)\n',
      'RoxygenNote: 6.1.1\n',
      'Suggests: ', suggests, '\n',
      'Imports: ', imports, '\n'
    )
    
    cat(paste(desc, collapse = ''), file = file)
    
    message("File '", file, "' created.")
    
    return(invisible(file))
    
  }

  install_mode = match.arg(install_mode)
  increase_version = match.arg(increase_version)
  
  orig_dir = getwd()
  on.exit(orig_dir)
  setwd(path)
  path = normalizePath(getwd())
  
  package = list(name = pckg_name, dir = pckg_dir, dir_source = pckg_dir_source)
  
  dir_base = normalizePath(package$dir, mustWork = FALSE)
  
  msg("PACKAGE: ", package$name)
  msg("Execution path: ", path)
  msg("Package path: ", dir_base)
  
  if(!nzchar(package$dir))
    stop('Specify non-empty package directory.')
    
  if(identical(path, dir_base))
    stop('The package path and the installation path must be different.')

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
  }

  suppressMessages({
    trace('cat', exit = quote(if(identical(file, stdout())) flush.console()), 
          print = FALSE, where = asNamespace('base')) 
    })

  on.exit(suppressMessages(untrace('cat', asNamespace('base'))))
  
  if(create) {
  
    dir_src = normalizePath(package$dir_source, mustWork = FALSE)
    dir_R = normalizePath(paste0(dir_base, "/R/"), mustWork = FALSE)   
    file_desc = normalizePath(file_desc, mustWork = FALSE)
    
    if(grepl(dir_base, dir_src, fixed = TRUE))
      stop("Source code directory (",dir_src,") must not be inside the package path (",dir_base,").")
      
    if(grepl(dir_base, dirname(file_desc), fixed = TRUE))
      stop("DESCRIPTION file must not be inside the package path (",dir_base,").")

    msg("Listing all *.R files in '",dir_src,"' ...")
    files_src = list.files(pattern='^.*[.]R$', path = dir_src, full.names = TRUE)
    
    if(length(files_src)==0)
      stop("No *.R files found in the path '",dir_src,"'.")
    
    if(!dir.exists(dir_base)) {
      msg("Creating package directory '",dir_base,"' ...")
      dir.create(package$dir, recursive = TRUE)
    }
    
    msg("Checking for any existing files in '", dir_R, "' ...")
    files_old = list.files(dir_base, full.names = TRUE, recursive = TRUE)
    files_old = files_old[!file.info(files_old)$isdir]
    
    if(!update_doc) {
      files_old = files_old[!grepl('/man/',files_old)]
    }
    
    if(install_mode == 'quick') {
      files_old = files_old[!grepl('NAMESPACE',files_old)]
    }
    
    if(length(files_old)>0) {
      msg("Deleting any exiting files in '",dir_base,"' that might be present...")
      sapply(files_old, file.remove)
    }

    msg("Copying source files from '",dir_src,"' to '", dir_R, "' ...")
    
    if(!dir.exists(dir_R)) {
      dir.create(dir_R)
    }
    
    for(f in files_src) {
      stopifnot(file.copy(f, dir_R), overwrite = TRUE)
    }
    
    msg(length(files_src), " file(s) copied.")

    if(!file.exists(file_desc)) {
      message("The description file '", file_desc, "' does not exist.",
              " Creating barebones version of it ...")
      create_description_file(file_desc, package$name)
    }
    
    if(increase_version == 'none') {
    
      msg("Package version not updated.")
      
    } else {

      msg("Updating the version field in the description file '",file_desc,"' ...")
    
      DESC = readLines(file_desc)
      is_version = grep('Version: ',DESC)
      version_prev = sub('.*[ ]','',DESC[is_version])
      version_next = version_increase(version_prev)

      DESC[is_version] = sub('[ ].*',paste0(' ',version_next), DESC[is_version])
      msg('... from version ', version_prev, ' to ', version_next, ' ...')
      writeLines(DESC, file_desc)
      
    }

    msg("Placing the description file into the package directory '",dir_base,"' ...")
    stopifnot(file.copy(file_desc, dir_base, overwrite=TRUE))
    
    msg("Package created.")
    
  } else {
  
    msg("PACKAGE IS NOT BEING CREATED.")
    
  }

  if(update_doc) {
  
    msg("Creating package documentation...")
    devtools::document(dir_base)
    
  } else {
  
    msg("DOCUMENTATION IS NOT BEING CREATED OR UPDATED.")
    
  }

  if(build) {
  
    msg("Building the package ...")
    devtools::build(package$name, path = 'build')
    msg("Package built.")

    msg("Updating the 'latest' package file ...")
    files = file.info(list.files('build', full.names=TRUE))
    file_newest = rownames(files)[which.max(files$mtime)]
    file_latest = sub('[_].*[.]tar', '_latest.tar', file_newest)
    file.copy(file_newest, file_latest, overwrite = TRUE)
    
  } else {
  
    msg("PACKAGE IS NOT BEING BUILT.")
    
  }

  msg("Installation mode: ", switch(install_mode, 'no' = 'NOT INSTALLED', 'full' = 'FULL', 'quick' = 'QUICK'))
  
  if(install_mode == 'no') {
  
    msg('THE PACKAGE IS NOT BEING INSTALLED.')
    
  } else {

    msg("Installing the package ...")
    devtools::install(package$dir, quick = install_mode == 'quick')

    if(install_mode == 'quick') {
      file.copy(file.path(dir_base, 'NAMESPACE'), '.', overwrite=TRUE)
    }

    msg("Installation finished.")
    
    msg("Checking installation ...")
    if(! package$name %in% installed.packages()[,'Package']) {
      warning("Package '", package$name, "' not found among the installed packages.")
    } else {
      msg("Installation check successful.")
    }
    
  }
  
  if(attach) {
    msg("Attaching library '", package$name, "' ...")
    library(package$name, character.only = TRUE)
  }
  
  if(add_path_to_global) {
  
    if(isNamespace('utilbox')) {
      msg("Adding the package's source path variable to .GlobalEnv and to .Rprofile so that it is set at R startup ...")
      
      dir_src = normalizePath(package$dir_source, winslash = '/', mustWork = FALSE)
      file_Rprofile = normalizePath(file.path(Sys.getenv("HOME"), ".Rprofile"), mustWork = FALSE)
      startup_code = paste0("assign('.", package$name, "_source_path', '", dir_src, "', envir=.GlobalEnv)")
      assign(paste0('.', package$name, '_source_path'), dir_src, envir = .GlobalEnv)
      
      utilbox::R_del_code_startup(substr(startup_code, 1, 30), file_Rprofile)
      utilbox::R_add_code_startup(startup_code, file_Rprofile)
    } else {
      msg("Package's installation path cannot be added to .GlobalEnv and to .Rprofile without the package 'utilbox'.")
    }
    
  }

}

#' @rdname build_package
#' @export
build_utilbox = function(pckg_name = 'utilbox', pckg_dir = 'utilbox', pckg_dir_source = 'source', path = '.', 
    file_desc = 'DESCRIPTION', create = TRUE, update_doc = TRUE, build = TRUE, install_mode = 'full', attach = TRUE,
    add_path_to_global = TRUE, increase_version = 'minor3', install_prerequisities = TRUE) {
    
  build_package(pckg_name = pckg_name, pckg_dir = pckg_dir, pckg_dir_source = pckg_dir_source, path = path, 
                create = create, update_doc = update_doc, build = build, install_mode = install_mode, 
                add_path_to_global = add_path_to_global, file_desc = file_desc, attach = attach,
                increase_version = increase_version, install_prerequisities = install_prerequisities)

}

#' @rdname build_package
#' @export
install_utilbox = function(pckg_name = 'utilbox', pckg_dir = 'utilbox', path = '.', install_mode = 'full', 
    attach = FALSE, add_path_to_global = FALSE, install_prerequisities = TRUE, pckg_dir_source = 'source') {
    
  build_package(pckg_name = pckg_name, pckg_dir = pckg_dir, path = path, create = FALSE, update_doc = FALSE, 
                build = FALSE, install_mode = install_mode, add_path_to_global = add_path_to_global, attach = attach, 
                install_prerequisities = install_prerequisities, pckg_dir_source = pckg_dir_source)

}
