#' Extract file name and path
#'
#' `dir_name()` is the analogue of `base::dirname()` which does not 
#' fail on long paths. `base_name()` is the equivalent analogue of
#' `base::basename()`.
#'
#' @examples
#' file = 'c:\\Windows\\System32\\cmd.exe'
#' dir_name(file)     # returns the part part, i.e., 'c:\\Windows\\System32'
#' base_name(file)    # returns the filename part, i.e., 'cmd.exe'
#'
#' @export
dir_name = function(files) {
  files = gsub('\\\\','/',files)
  files = strsplit(files, '/')
  sapply(files, function(x) paste(head(x,-1), collapse='/'))
}

base_name = function(files) {
  files = gsub('\\\\','/',files)
  files = strsplit(files, '/')
  sapply(files, tail, 1)
}

#' @title
#' List files matching regular pattern
#'
#' @description
#'
#' A wrapper for [`base::list.files()`] that allows multiple patterns 
#' at once and files that match at least on of the patterns are 
#' returned. Optionally, files can be sorted according to attribute in 
#' `attrib`.
#'
#' `list_files_by_date()` and `list_files_by_size()` are shortcuts to 
#' sorting according to attributes "modification time" and "size", 
#' respectively.
#'
#' @family file system functions provided by utilbox
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
    fs1 = if(full.names) fs else file.path(path,fs)
    info = unlist(file.info(fs1)[attrib])
    fs = fs[order(info, decreasing=decreasing)]
  }
  
  if(!full.names && append_path) file.path(path,fs) else fs
  
}

#' @rdname list_files
#' @export
list_files_by_date = function(...) {
  list_files(..., sort=TRUE, attrib='mtime', decreasing=TRUE)
}

#' @rdname list_files
#' @export
list_files_by_size = function(...) {
  list_files(..., sort=TRUE, attrib='size', decreasing=TRUE)
}

#' @title
#' Work with files and paths
#'
#' @description
#'
#' `is_absolute_path()` checks whether the supplied path (`path`), 
#' which may or may not include a file name, is absolute or not. On 
#' Windows, absolute means that it begins with '\<drive letter\>:/', on 
#' Linux/Unix it checks for '/' at the beginning of the path.
#'
#' `file_path()` binds the supplied path (`path`) with the supplied 
#' file name (`file`). If `file` contains an absolute path, or if 
#' `path` is an an empty string, the argument `path` is ignored. 
#' Otherwise, it concatenates the two using `fsep` as separator.
#'
#' @examples
#' file_path('home','text.txt')
#'
#' @family file system functions provided by utilbox
#' @export
file_path = function(path, file, normalize=TRUE, fsep = .Platform$file.sep) {
  
  p = if(is_absolute_path(file) || str_is_empty(path)) {
    file
  } else {
    file.path(path, file, fsep=fsep)
  }
  
  if(normalize) 
    normalizePath(p, mustWork=FALSE) else p
  
}

#' @rdname file_path
#' @export
is_absolute_path = function(path) {
  (is_win() && path %likei% '^[a-z][:](/|\\\\)') || (is_linux() && path %like% '^[/]')
}

#' @title
#' Complete partial file names
#'
#' @description
#'
#' `file_match()` takes a list of files (`files`) and looks whether 
#' they exist relative to the given path (`path`). If they do, they 
#' remain unchanged. If they do not exist, an attempt is made to 
#' identify an existing file whose name matches the pattern defined by 
#' the supplied file name. If multiple files match the pattern, an error 
#' is thrown, unless `allow_multiple=TRUE`.
#'
#' @export
file_match = function(files, path, type=NULL, full.names=FALSE) {

  exist = lapply(files, file_exists_check, path)
  files = lapply(patterns, list_file_match, path, type, full.names)
  `names<-`(files, patterns)
  
}
  
list_file_match = function(p, path, type=NULL, full.names=FALSE, ...) {
  
  type = type %|||% c('exact','partial','free')

  p = list(exact = paste0('^', str_patternize(p), '$'), 
           partial = paste0('^', str_patternize(p)), 
           free = str_patternize(p))

  lf = hijack(list.files, path=path, full.names=full.names)
  
  lapply(p[type], function(p1) lf(pattern=p1))
  
}

#' @title
#' Create/remove a directory
#'
#' @description
#'
#' `dir_create()` is a verbose version of \code{base::dir.create()}.
#'
#' `dir_remove()` removes (unlinks) supplied directory/ies.
#'
#' `dir_slash_check()` checks whether the argument (`dir`) ends with 
#' a forward/backward slash and if not it appends one.
#'
#' @name dir_create
#' @family file system functions provided by utilbox
#' @export
dir_create = function(dir, ask=interactive(), verbose=FALSE) {

  if(length(dir)>1) {
  
    res = sapply(dir, dir_create, ask, verbose)
    
  } else {
  
    if(!dir.exists(dir)) {
      
      if(ask) {
        wait("The directory '",dir,"' does not exist relative to the current path ('",
             getwd(),"') and will be created ...")
      }
      
      res = dir.create(dir, showWarnings=FALSE, recursive=TRUE)
      
      if(!dir.exists(dir) && verbose) {
        warn("Directory '",dir,"' could not be created.")
      }
        
    } else {
    
      res = FALSE
      
      if(verbose) {
        msgf("No need to create, directory '",dir,"' already exists.")
      }
      
    }
      
  }
  
  invisible(res)
    
} 

#' @rdname dir_create
#' @export
dir_remove = function(dir) {
  sapply(dir, unlink, recursive=TRUE)
}

#' @rdname dir_create
#' @export
dir_slash_check = function(dir, fsep=.Platform$file.sep) {
  ifelse(grepl("[/\\]$",dir), dir, paste0(dir,fsep))
}
  
#' @title
#' Check file/directory existence
#'
#' @description
#'
#' `dir_exist_check()` checks if directory `dir` exists (relative to 
#' the current working directory) and if not it tries to create it. On 
#' success the value supplied in `dir` is returned. If directory 
#' creation fails, then an error is thrown, unless `stop_if_fail=FALSE`
#' was supplied, in which case the value `./` is returned. Verboseness 
#' of the function is controled via `trace` (with 0 being least verbose).
#'
#' `file_exists()` checks if a file or multiple files exist(s) relative to 
#' the path in `path`. If the existence check returns any `FALSE`, the 
#' behaviour depends on `stop_on_missing` and `warn_on_missing`. If the 
#' former is `TRUE`, an error is printed and the execution halts. If only  
#' the former is `TRUE`, a warning is shown but the execution continues. 
#' When both are `FALSE`, no message is shown and a logical indicating 
#' each file's existence is returned.
#'
#' To achieve a clear distinction in code between what is expected in 
#' terms of existence, it is advised to use one of the three following 
#' aliases:
#'
#' `file_exists_checks()` shows no messages and only returns the 
#' logicals.
#'
#' `file_exists_should()` warns on any files missing but proceeds and 
#' returns the logicals.
#'
#' `file_exists_must()` is a strict checker that halts on non-existence
#' of any of the files.
#'
#' @family file system functions provided by utilbox
#' @export
dir_exist_check = function(dir, stop_if_fail=TRUE, create_on_missing=TRUE, trace=1) {
  
  if(dir.exists(dir)) return(dir)
  
  if(trace>0) 
    cat0("Directory '",dir," does not exist",if(create_on_missing) " and will be created",".\n")

  if(create_on_missing) dir.create(dir, showWarnings=FALSE)

  if(!dir.exists(dir)) {
    msg = paste0("Directory '",dir,"' could not be created.")
    if(stop_if_fail) stop(msg) else if(trace>0) warn(msg)
    dir = "./"
  }
  
  dir
}

#' @rdname dir_exist_check
#' @export
file_exists = function(files, path, stop_on_missing=FALSE, warn_on_missing=TRUE) {

  if(missing(path)) path = getwd()
  
  if(is.list(files)) unlist(files)
  if(is.list(path)) unlist(path)
  
  files = file_path(path, files)
  
  fexists = `names<-`(file.exists(files), files)

  msg = `if`(all(fexists), NULL, msg_files_missing(files[!fexists], path))

  if(!is.null(msg)) {
    if(stop_on_missing) stop(msg)
    if(warn_on_missing) warn(msg)
  }

  invisible(fexists)
  
}

msg_files_missing = function(files, path='.') {
  paste0("The following files are missing (relative to path '", path, "'):\n", paste(files, "\n", collapse='\n'), '\n')
}
  
#' @rdname dir_exist_check
#' @export
file_exists_check = function(files, path, stop_on_missing=FALSE, warn_on_missing=FALSE) {
  file_exists(files, path, stop_on_missing=stop_on_missing, warn_on_missing=warn_on_missing) 
}

#' @rdname dir_exist_check
#' @export
file_exists_should = function(files, path, stop_on_missing=FALSE, warn_on_missing=TRUE) {
  file_exists(files, path, stop_on_missing=stop_on_missing, warn_on_missing=warn_on_missing) 
}

#' @rdname dir_exist_check
#' @export
file_exists_must = function(files, path, stop_on_missing=TRUE, warn_on_missing=TRUE) {
  file_exists(files, path, stop_on_missing=stop_on_missing, warn_on_missing=warn_on_missing) 
}

#' @title
#' Rename or remove multiple files
#'
#' @description
#'
#' `file_rename()` renames one or multiple files.
#'
#' `file_rename_via_drive()` also renames file except it does it by
#' mapping the destination folder as a drive ('T' by default). This
#' is useful primarily when dealing with very long file names and
#' paths on Windows (with their file length limit of 259 characters),
#' where replacing the long path with a short one (e.g., 'T:/') 
#' can help avoiding the file name length errors.
#'
#' `file_remove()` removes one or multiple files.
#'
#' `file_move()` moves the file to the supplied path (`destination`). 
#' It is a wrapper around `fine_rename()`.
#'
#' `file_rename_timestamp()` renames a single file by adding a time 
#' stamp based on the file's modification time.
#'
#' `file_timestamp()` produces a time stamp based on attributes of 
#' the supplied file.
#'
#' In case of a failure, these functions re-attempt up to `nretry` 
#' times.
#'
#' @name file_rename
#' @family file system functions provided by utilbox
#' @export
file_rename = function(from, to, nretry=1, time_to_sleep_prior=0) {

  Sys.sleep(time_to_sleep_prior)
  
  if (!isTRUE(file.info(dirname(to))$isdir)) {
    dir.create(dirname(to), recursive=TRUE)
  }

  msg = tryCatch(file.rename(from, to), warning=function(w) invisible(w))
  
  if(is_error(msg) && nretry>0) {
    msg = Recall(from, to, nretry-1, time_to_sleep_prior+0.05)
  }
    
  if(is_error(msg) && nretry==0) {
    warn("Problem with renaming file '",from,"' to '",to,"'.", skip1=0, skip2=0)
  }

  invisible(msg)
  
}

#' @rdname file_rename
#' @export
file_rename_via_drive = function(from, to, drive='T', unmap=TRUE, nretry=2, time_to_sleep_prior=0.01) {

  if(!is_win()) {
    warnings('`file_rename_via_drive()` works only on Windows. No drives will be mapped for file renaming.')
    msg = file_rename(file_from, to, nretry=nretry, time_to_sleep_prior=time_to_sleep_prior)
    return(invisible(msg))
  }

  wdir_init = getwd()

  file_from = base_name(from)
  file_to = base_name(to)
  dir_from = dir_name(from)
  dir_to = dir_name(to)

  to = normalizePath(paste0(drive,':/', file_to), mustWork=FALSE)

  setwd(dir_to)

  map_drive(drive=drive, change_wdir=FALSE, delete_existing=TRUE, silent=TRUE)

  setwd(wdir_init)
  setwd(dir_from)

  file_rename(file_from, to, nretry=nretry, time_to_sleep_prior=time_to_sleep_prior)

  setwd(wdir_init)

  if(unmap) unmap_drive(drive, silent=TRUE)
  
}

#' @rdname file_rename
#' @export
file_remove = function(file, nretry=10) {
  invisible(sapply(file, file_remove_one, nretry=nretry))
}

file_remove_one = function(file, nretry=10, time_to_sleep_prior=0) {

  if(!file.exists(file)) return(TRUE)

  Sys.sleep(time_to_sleep_prior)
  
  msg = tryCatch(file.remove(file), warning=function(w) invisible(w))
  
  if(is_error(msg, TRUE) && nretry>0) {
    msg = Recall(file, nretry-1, time_to_sleep_prior+0.02)
  }
    
  if(is_error(msg, TRUE) && nretry==0) {
    warn("Problem with removing file '",file,"' (", msg$message,").")
  }

  invisible(msg)
  
}

#' @rdname file_rename
#' @export
file_rename_timestamp = function(filename, attrib='mtime', nretry=10) {
  newname = paste0(filename, '_', file_timestamp(filename))
  file_rename(filename, newname, nretry=nretry)
}

#' @rdname file_rename
#' @export
file_move = function(files, destination, strip.dir=TRUE) {
  sapply(files, file_rename, file.path(destination, if(strip.dir) sub(".*[/\\]$","",f) else f))
}

#' @rdname file_rename
#' @export
file_timestamp = function(filename, attrib='mtime') {
  times = .POSIXct(unlist(file.info(filename)[attrib]))
  names(times) = filename
  gsub("-","",gsub(":","",gsub("[ ]","",times)))
}

#' @title
#' Sort files names
#'
#' @description
#'
#' `file_sort()` sorts files (specified in `files`) by their 
#' attributes such as time or size. The attribute for sorting is 
#' specified via `by`. The logical `decreasing` switches between 
#' descending and ascending order.
#'
#' `file_sort_time()` is a shortcut to sorting according to attribute 
#' "mtime".
#'
#' @name file_sort
#' @family file system function provided by utilbox
#' @export
file_sort = function(files, by=c("time","name","isdir","mode","mtime","ctime","atime","exe"), 
                     decreasing=switch(by, time=, mtime=, ctime=, atime=TRUE, FALSE),  nget=NULL) {
                     
  if(!length(files)) return(files)
  
  if(!length(by)) stop("Missing value in 'by'!")
  
  by = by[1]
  
  if(by=="time") by = "ctime"
  
  if(by=="name") {
    f = sort(files, decreasing=decreasing)
  } else {
    Info = file.info(files)
    if(all(colnames(Info)!=by)) 
      stop("Unknown sorting attribute '",by,"'.")
    f = files[order(Info[,by], decreasing=decreasing)]
  }
  
  if(!is.null(nget)) f = head(f, nget)
  
  f
  
}

#' @rdname file_sort
#' @export
file_sort_time = function(files, by="mtime", decreasing=TRUE) {
  if(!length(files)) return(files)
  files[order(file.info(files)[,by], decreasing=decreasing)]
}

#' @title
#' Check file existence and emptiness
#'
#' @description
#'
#' `file_empty` checks if file exists, tries to read it to see 
#' whether there is any data in it.
#'
#' @family file system function provided by utilbox
#' @export
file_empty = function(file) {

  file_exists_must(file)
  
  res = try(read.table(file, nrow=1), silent=TRUE)
  
  return(class(res)=="try-error")
  
}

#' @title
#' Get file sizes
#'
#' @description
#'
#' `file_size()` returns the file sizes in appropriate units.
#'
#' @family file system function provided by utilbox
#' @export
file_size = function(files) {
  convert_unit(file.info(files)[,"size"])
}

#' @title
#' File open check
#'
#' @description
#'
#' `file_can_open_check()` checks whether a file can be opened.
#'
#' @family file system function provided by utilbox
#' @export
file_can_open_check = function(filename) {

  #do_nothing = function(x) invokeRestart("muffleWarning")
  #zz = withCallingHandlers(try(close(file(filename, open="ab")), silent=TRUE), warning=do_nothing)
  attempt_call(close(file(filename, open="ab")))
  
}

#' @title
#' File lock check
#'
#' @description
#'
#' Check if a given file is locked by another application (e.g. by 
#' Excel). Currently relies on a call to `wmic` and works on Windows 
#' only.
#'
#' @family file system function provided by utilbox
#' @export
check_file_locked = function(file) {

  if(missing(file)) 
    stop("Supply file name via `file`.")
  
  if(is_win()) {
  
    x = try(system("wmic process get commandline", intern=TRUE, show.output.on.console=FALSE))
    if(is_error(x)) return(-1)
    
    as.numeric(any(grepl(str_patternize(file), x)))
    
  } else {
    NA
  }
}

#' @title
#' Drop dots from file names
#'
#' @description
#'
#' `clean_filename()` replaces dots in file names with the value in 
#' `chr`. Useful for instance for changing file names of plots that are 
#' to be included in a latex file where the dots cause trouble.
#'
#' @family file system function provided by utilbox
#' @export
clean_filename = function(x, chr="-", keep_last_dot=TRUE) {

  p = if(keep_last_dot) str_pos(x, ".", last=TRUE) else -1
  
  if(p<=0) {
    gsub("[.]", chr, x)
  } else {
    paste0(gsub("[.]", chr, substr(x,1,p-1)), substr(x,p,nchar(x)))
  }
  
}

#' @title
#' Split file names
#'
#' @description
#'
#' Separates the path portions from file names. Returns the paths and 
#' the proper file names in a list.
#'
#' @examples
#' separate_path("c:/Windows/system.dat")
#' separate_path("system.dat")
#'
#' @family file system function provided by utilbox 
#' @export
separate_path = function(files, path0="./") {
  
  files = sub("/+$","",gsub("\\\\","/",files))
  paths = rep(path0, length(files))
  w = grepl("/",files)
  
  if(any(w)) {
    x = strsplit(files[w],"/")
    paths[w] = paste0(sapply(x, function(x1) paste0(x1[-length(x1)], collapse="/")), "/")
    files[w] = sapply(x, tail, 1)
  }
  
  list(path=paths, filename=files)

}

#' @title
#' Generate a random file name
#'
#' @description
#'
#' `random_filename()` generates a random file name of length `nchar` 
#' by drawing from the set defined via `chars`.
#'
#' @family file system function provided by utilbox
#' @export
random_filename = function(path=".", nchar=3, chars=c(letters, LETTERS, 0:9, "_-%#@")) {
  
  file = paste(sample(chars, nchar, replace=TRUE), collapse='')
  
  if(file_exists_check(file, path)) {
    file = Recall(path, char, chars)
  }
  
  file
  
}

#' @title
#' Backup a file
#'
#' @description
#'
#' `file_backup()` creates a backup of the given file by creating a copy
#' in the path specified in `path_backup`. The original file is preserved.
#' Return code 1 means success, 0 means 'no file to backup', -1 means failure.
#'
#' @family file system function provided by utilbox
#' @export
file_backup = function(file, path, path_backup, pid=FALSE, announce=TRUE, fun_msg=msgf) {
  
  file_bak = paste0(file, '_', timest(add_pid=pid), '.bak')
  
  if(!missing(path)) {
    file = file_path(path, file)
  }
  
  if(!missing(path_backup)) {
    file_bak = file_path(path_backup, file_bak)
  } else if(!missing(path)) {
    file_bak = file_path(path, file_bak)
  }
  
  if(announce) {
    if(file.exists(file)) {
      fun_msg("Creating backup of file '",file,"' (backed up as '",file_bak,"')")
    } else {
      fun_msg("File '",file,"' does not exist and therefore cannot be backed up.")
    }
  }
  
  res = if(!file.exists(file)) {
    0
  } else {
    
    st = file.copy(file, file_bak)
  
    if(!st && announce) fun_msg("File backup failed.")
    
    ifelse(st, 1, -1)
    
  }
  
  invisible(res)
  
}

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
#' regular format (e.g., 'c:\Windows') to the network path format 
#' (e.g., '\\localhost\\c$\Windows'), which can be used to map
#' any path to a drive.
#'
#' `list_all_drives()` returns a list of all existing drives
#' together with their mapped location in the network path format.
#'
#' `list_all_network_drives()` lists drives that are mappings
#' of network-storage devices (which can include local paths
#' mapped as drives by relying on the network format of paths
#' (e.g., '\\localhost\\c$\Windows').
#'
#' All of these functions work on Windows only and rely on the 
#' Windows system utilities such as `net` and/or `wmic` (which
#' has been deprecated in Windows 10, version 21H1, see here
#' https://docs.microsoft.com/en-us/windows/win32/wmisdk/wmic)
#'
#' @family file system function provided by utilbox
#' @name map_drives
#' @export
map_drive = function(drive='t', path='.', persistent=FALSE, delete_existing=FALSE, 
  force_delete=FALSE, change_wdir=FALSE, normalize=FALSE, silent=FALSE) {
  
  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }
   
  call_delete = paste0('net use ',drive,': /Delete /',ifelse(force_delete, 'y', 'n'))
 
  if(dir.exists(paste0(drive,':/'))) {
    if(delete_existing) {
      if(!silent) message("Unmapping existing connection to drive '",drive,"' ...")
      answer = shell(call_delete, intern=TRUE)
      if(!silent) message("Message by shell: ",answer)
    } else {
      warning("Drive '",drive,"' already in use. To unmap first use `delete_existing=TRUE`.")
      return(FALSE)
    }
  }
 
  if(path=='.') path = getwd()
 
  path_full = convert_to_network_path(path, normalize=normalize)

  if(!silent) message("Mapping drive '",drive,"' to path '",path_full,"' ...")
  call_map = paste0('net use ',drive,': ',path_full,' /persistent:', ifelse(persistent, 'yes', 'no'))
  answer = shell(call_map, intern=TRUE)
  if(!silent) message("Message by shell: ", answer)
 
  result = dir.exists(paste0(drive,':/'))
 
  if(result)
    if(!silent) message('Mapping successful.')
 
  if(change_wdir) {
    setwd(paste0(drive,':/'))
    if(!silent) message("Working directory changed to '",getwd(),"'.")
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

  if(dir.exists(paste0(drive,':/'))) {
 
    if(!silent) message("Unmapping existing connection to drive '",drive,"' ...")
    call_delete = paste0('net use ',drive,': /Delete /',ifelse(force, 'y', 'n'))
    answer = shell(call_delete, intern=TRUE, wait=FALSE)
    if(!silent) message("Message by shell: ",answer)
   
  } else {
    if(!silent) message("Drive '",drive,"' not mapped.")
  }
 
  result = !dir.exists(paste0(drive,':/'))
 
  invisible(result)

}

#' @rdname map_drives
#' @export
list_all_drives = function(path) {

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
 
  drives
 
}

#' @rdname map_drives
#' @export
list_all_network_drives = function(path) {

  if(!is_win()) {
    warning('`',this_fun_name(),'()` works only on Windows.')
    return(invisible(FALSE))
  }
  
  drives = list_all_drives()
  
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

  if(normalize) path = normalizePath(path)
 
  drives = list_all_drives()
 
  for(i in seq_along(drives$Drive)) {
    if(grepl(paste0('^',drives$Drive[i]), path, ignore.case=TRUE)) {
      path = paste0(drives$NetworkPath[i], substring(path, 3))
      break
    }
  }
 
  if(normalize) path = normalizePath(path)
 
  path
 
}
