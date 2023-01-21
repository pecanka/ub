#' @title
#' Work with drives on Windows
#'
#' @description
#'
#' `map_drive()` maps a path to the specified drive.
#'
#' `unmap_drive()` unmaps the specified drive.
#'
#' `convert_to_network_path()` converts a path from the regular
#' regular format (e.g., 'c:\\Windows') to the network path format 
#' (e.g., '\\\\localhost\\c$\\Windows'), which can be used to map
#' any path to a drive.
#'
#' `list_all_drives()` returns a list of all existing drives
#' together with their mapped location in the network path format.
#' If supplied, a string in `append` is attached to the end of the
#' network path(s) (e.g., '\\.' can be useful).
#'
#' `list_all_network_drives()` lists drives that are mappings
#' of network-storage devices (which can include local paths
#' mapped as drives by relying on the network format of paths
#' (e.g., '\\\\localhost\\c$\\Windows').
#'
#' All of these functions work on Windows only and rely on the 
#' Windows system utilities such as `net` and/or `wmic` (which
#' has been deprecated in Windows 10, version 21H1, see here
#' https://docs.microsoft.com/en-us/windows/win32/wmisdk/wmic)
#'
#' @family file system function provided by ub
#' @name map_drives
#' @export
map_drive = function(drive = find_unmapped_drive(), path='.', persistent=FALSE, delete_existing=FALSE, force_delete=FALSE, 
  change_wdir=FALSE, normalize=FALSE, silent=FALSE) {
  
  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }
   
  msg = if(silent) function(...) {} else message

  call_delete = paste0('net use ',drive,': /Delete /',ifelse(force_delete, 'y', 'n'))
 
  if(dir.exists(paste0(drive,':/'))) {
    if(delete_existing) {
      msg("Unmapping existing connection to drive '",drive,"' ...")
      answer = shell(call_delete, intern=TRUE)
      msg("Message by shell: ",answer)
    } else {
      warning("Drive '",drive,"' already in use. To unmap first use `delete_existing=TRUE`.")
      return(FALSE)
    }
  }
 
  path = normalizePath(path)
 
  path_full = convert_to_network_path(path, normalize=normalize)

  msg("Mapping drive '",drive,"' to path '",path_full,"' ...")
  call_map = paste0('net use ',drive,': ',path_full,' /persistent:', ifelse(persistent, 'yes', 'no'))
  answer = shell(call_map, intern=TRUE)
  msg("Message by shell: ", answer)
 
  result = dir.exists(paste0(drive,':/'))
 
  if(result)
    msg('Mapping successful.')
 
  if(change_wdir) {
    setwd(paste0(drive,':/'))
    msg("Working directory changed to '",getwd(),"'.")
  }
 
  invisible(result)
 
}

#' @rdname map_drives
#' @export
unmap_drive = function(drive, force=FALSE, silent=FALSE) {

  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }

  msg = if(silent) function(...) {} else message

  if(dir.exists(paste0(drive,':/'))) {
 
    msg("Unmapping existing connection to drive '",drive,"' ...")
    call_delete = paste0('net use ',drive,': /Delete /',ifelse(force, 'y', 'n'))
    answer = shell(call_delete, intern=TRUE, wait=FALSE)
    msg("Message by shell: ",answer)
   
  } else {
    msg("Drive '",drive,"' not mapped.")
  }
 
  result = !dir.exists(paste0(drive,':/'))
 
  invisible(result)

}

#' Find an unused drive letter
#'
#' `find_unmapped_drive()` searches for an unmapped drive by going 
#' through the letters of the alphabet (starting with 'a', 'b', etc.)
#' and returns the first letter for which there is no mapped drive.
#' Otherwise, it returns `FALSE`.
#'
#' @export
find_unmapped_drive = function(candidates = letters) {

  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }

  free_drive = FALSE
  
  for(drive in candidates) {
    if(!dir.exists(paste0(drive,':\\'))) {
      free_drive = drive
      break
    }
  }  
  
  free_drive

}

#' @rdname map_drives
#' @export
list_all_drives = function(append = '') {

  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }

  answer = shell('wmic logicaldisk get caption,providername,drivetype', intern=TRUE)
  drives = gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", answer))
  drives = strsplit(drives, ',')
  drives = Filter(length, drives)
  drives = lapply(drives, `[`, 1:3)
  
  if(!identical(drives[[1]], c('Caption','DriveType','ProviderName')))
    warning("Unexpected format returned by the shell call to 'wmic'.")
   
  drives = as.data.frame(do.call(rbind, drives[-1]))
  drives = setNames(drives, c('Drive','Type','NetworkPath'))
  
  w = drives$Type=='3' & is.na(drives$NetworkPath)
  drives$NetworkPath[w] = paste0('\\\\localhost\\',substr(drives$Drive[w],1,1),'$')
 
  drives$NetworkPath = paste0(drives$NetworkPath, append)
 
  drives
 
}

#' @rdname map_drives
#' @export
list_all_network_drives = function(append = '') {

  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }
  
  drives = list_all_drives(append = append)
  
  drives[drives$Type != 3,]
  
}

#' @rdname map_drives
#' @export
list_all_network_drives2 = function(path) {

  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }

  answer = shell('wmic path win32_mappedlogicaldisk get deviceid, providername', intern=TRUE)
 
  drives = gsub("\\s+", ",", gsub("^\\s+|\\s+$", "", answer))
  drives = strsplit(drives, ',')
  drives = Filter(length, drives)
  
  if(!identical(drives[[1]], c('DeviceID','ProviderName')))
    warning("Unexpected format returned by the shell call to 'wmic'.")

  drives = as.data.frame(do.call(rbind, drives[-1]))
  drives = setNames(drives, c('Drive','NetworkPath'))
 
  drives

}

#' @rdname map_drives
#' @export
convert_to_network_path = function(path, normalize=FALSE) {

  if(normalize) 
    path = normalizePath(path)
 
  drives = list_all_drives(append = '\\.')
 
  path_no_drive = sub('^[a-z][:][\\/]',ifelse(nchar(path)>3, '\\\\', ''), path, ignore.case=TRUE)
 
  for(i in seq_along(drives$Drive)) {
    if(grepl(paste0('^',drives$Drive[i]), path, ignore.case=TRUE)) {
      path = paste0(drives$NetworkPath[i], path_no_drive)
      break
    }
  }
 
  if(normalize) 
    path = normalizePath(path)
 
  path
 
}

#' Evaluate an expression
#'
#' Evaluate `call` with the effective path (either `path` when supplied
#' or the current working directory when not) mapped as a Windows drive.
#' This is useful when working with long paths and/or file names.
#' With `path = NULL` eval_with_drive simply evaluates the call without
#' any mapping of drives (i.e., behaves simply as `base::eval()`).
#'
#' @examples
#' ## Use eval_with_drive() to work around long file names
#' # start by setting a long file name
#' long_file_name = paste(rep('a',255), collapse='')
#'
#' # change path to something non-trivially long
#' orig_wdir = getwd()
#' setwd('~')
#'
#' # Try opening png device with the long file name
#' try({png(long_file_name); dev.off()})
#' print(file.exists(long_file_name))        # FALSE
#'
#' # Do the same but with the curren working directory mapped as drive T
#' eval_with_drive({png(long_file_name); dev.off()}, silent = TRUE)
#' # A regular call to file.exists still fails to find the file
#' print(file.exists(long_file_name))        # FALSE
#' print(eval_with_drive(file.exists(long_file_name), silent = TRUE))
#'
#' # Clean up
#' eval_with_drive(file.remove(long_file_name), silent = TRUE)
#' setwd(orig_wdir)
#'
#' @export
eval_with_drive = function(call, path = '.', drive = find_unmapped_drive(), unmap=TRUE, dir_create=FALSE, 
    ask_new_dir=TRUE, silent=FALSE) {

  msg = if(silent) function(...) {} else message
 
  if(!is.null(path)) {
 
    msg("Evaluating a call within the path '",path,"' via drive '",drive,"' ...")

    if(!dir.exists(path)) {
      if(dir_create) {
        dir_create(path, ask=ask_new_dir)
      } else {
        stop("Path '",path,"' does not exist.")
      }
    }

    wdir_init = getwd()
    on.exit(setwd(wdir_init))
    setwd(path)

    msg("... mapping drive '",drive,"' to the path ...")
    map_drive(drive=drive, change_wdir=TRUE, delete_existing=TRUE, silent=TRUE)
   
  }

  msg("... evaluating the call ...")
  res = eval(call)

  if(!is.null(path)) {
 
    Sys.sleep(0.01)

    setwd(wdir_init)
   
    if(unmap) {
      msg("... unmapping drive '",drive,"' ...")
      unmap_drive(drive, silent=TRUE)
    }
   
  }
 
  msg('Done.')

  return(res)
 
}
