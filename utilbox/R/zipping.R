#' @title
#' Lists files in a zip archive
#'
#' @description
#'
#' Lists files within a zip archive that match the supplied regular 
#' expression pattern or patterns. Keeps only files that match at least 
#' one of the patterns in `pattern` or those that do not match any of 
#' the patterns when `exclude` is `TRUE`.
#'
#' @param zip Name of the zip archive(s) to read files from
#' @param pattern Regular expression pattern(s) against which the 
#' discovered file names are matched
#' @param exclude If `TRUE`, the pattern is used to exclude the 
#' discovered files, otherwise files that match `pattern` are kept. 
#' Defaults to `FALSE`
#'
#' @return A character vector or list depending on whether the input 
#' in `zipfiles` was of type `list`.
#'
#' @family compression-related utilities provided by utilbox
#' @export
list_zip = function(zipfiles, pattern=".*", exclude=FALSE) {

  # List all files in all archives
  files = lapply(zipfiles, function(z) unzip(z, list=TRUE)$Name)

  # Keep only files that match at least one of the patterns in 'pattern' or
  # those that do not match either of the patterns when exclude is TRUE
  pattern = paste(pattern, collapse="|")
  
  f = function(x) {
    ifelse(exclude, `!`, I)(grepl(pattern, x))
  }
  
  files = lapply(files, Filter, f=f)
  
  return(if(is.list(zipfiles)) files else unlist(files))
  
}

#' @title
#' Compress files individually
#'
#' @description
#'
#' A collection of functions useful for creating zip archives and 
#' unzipping them.
#'
#' @details
#'
#' `zip_file` compresses files each into its own archive. Optionally 
#' adding time stamps to the output files.
#'
#' @param files List of files to compress. Exact file names, not 
#' regular patterns.
#' @param extras Modifiers sent to the zip archiver. Defaults to 
#' '-m', which moves the files into the archive.
#' @param appendix File name extension added to the name of each file 
#' by the archiver.
#' @param add_timestamp Sets whether a unique time stamp is also 
#' added.
#' @param check_status Determines whether a check for zero status 
#' code is performed.
#'
#' @return List of file names of the created archive and the 
#' corresponding status codes returned by the archiver.
#'
#' @family compression-related utilities provided by utilbox
#' @export
# Old name: zipupf
zip_file = function(files, extras="-m", appendix=".zip", add_timestamp=FALSE, check_status=TRUE) {

  if(missing(files)) 
    stop("Supply names of files to zip up.")
  
  if(length(files)==0) return(-1)
  
  odir = getwd()
  on.exit(setwd(odir))
  
  fs = separate_path(files, path0="./")
  
  ts = if(!add_timestamp) "" else paste0("_",file_timestamp(files))
  ofiles = paste0(fs$filename,ts,appendix)
  
  ios = NULL
  for(i in 1:length(files)) {
  
    setwd(fs$path[i])
    ios[i] = zip(ofiles[i], fs$filename[i], extras=extras)
    setwd(odir)
    
    if(check_status) check_zip_ios(ios[i], fs$filename[i])

  }
  
  return(list(outfile=paste0(fs$path,ofiles), ios=ios))
  
}

#' @title
#' Compress multiple files into single archive
#'
#' @description
#'
#' `zip_files` compresses files into a single archive supplied via 
#' `zipfile`.
#'
#' @param files List of files to compress. Exact file names, not 
#' regular patterns.
#' @param path Path to the files.
#' @param zipfile Name of the zip archive to create.
#' @param extras Modifiers sent to the zip archiver. Defaults to 
#' '-m', which moves the files into the archive.
#'
#' @return List of file names of the created archive and the 
#' corresponding status codes returned by the archiver.
#'
#' @family compression-related utilities provided by utilbox
#' @export
zip_files = function(files, path=".", zipfile, extras="-m") {

  if(missing(zipfile)) zipfile = paste0(random_filename(),".zip")
  
  odir = getwd()
  on.exit(setwd(odir))
  
  setwd(path)
  ios = zip(zipfile, files, extras=extras)
  setwd(odir)
  
  check_zip_ios(ios)
  
  nlist(zipfile, ios)
  
}

#' @title
#' Compress all files matching a pattern
#'
#' @description
#'
#' `zip_files_pattern` compresses files matching the pattern in 
#' `mask` (and not matching the pattern in `mask_exclude`) each into its 
#' own archive.
#'
#' @param mask Regular pattern to match the files found in `path` 
#' against (for inclusion).
#' @param mask_exclude Regular pattern to match the files found in 
#' `path` against (for exclusion).
#' @param outfile name of the output zip archive. Can be omitted.
#' @param path Path to the files to be archived.
#' @param appendix File extension of the output archive. Relevant 
#' when `outfile` omitted or when `single_archive` is `TRUE`.
#' @param extras Modifiers sent to the zip archiver. Defaults to 
#' '-m', which moves the files into the archive.
#' @param patternize Determines whether the mask is treated as given 
#' or whether the special characters in it are wrapped to be matched 
#' accurately.
#' @param chunk Batch size for the archiver.
#' @param announce Determines whether details are announced.
#' @param single_archive Determines whether a single archive is 
#' produced (`TRUE`) or whether each file is placed into its own archive 
#' (`FALSE`). In the latter case the archive name is based on the file 
#' name and `appendix`.
#' @param retry Determines whether repeated attempts are made to when 
#' the archiver returns a non-zero status code.
#' @param nretries How many repeated attempts are made.
#' @param browser_on_error Launch the `browser` upon non-zero status 
#' (for debugging).
#'
#' @return List of file names of the created archive and the 
#' corresponding status codes returned by the archiver.
#'
#' @family compression-related utilities provided by utilbox
#' @export
# Old name: zipup
zip_files_pattern = function(mask=".*", mask_exclude, outfile, path=".", appendix=".zip", 
  extras="-m", patternize=TRUE, chunk=Inf, announce=FALSE, single_archive=TRUE,
  retry=FALSE, nretries=5, browser_on_error=FALSE) {

  odir = getwd()
  on.exit(setwd(odir))
  
  if(patternize) mask = str_patternize(mask)
  files = list.files(path=path, pattern=mask)
  
  if(length(files)==0) return(list(ios=-1, zipfile=NULL))
  
  ## If multiple files found, throw an error. Otherwise base the output
  ## file name on the single file found.
  if(missing(outfile) && single_archive) {
    if(length(files)>1) 
      stop("The archive name must be supplied when multiple files",
           " match the mask and single_archive is TRUE.")
    outfile = paste0(files, appendix)
  }

  ## Exclude the files that match mask_exclude
  if(!missing(mask_exclude)) {
    if(patternize) mask_exclude = str_patternize(mask_exclude)
    files = files[!grepl(mask_exclude, files)]
  }
  
  ## Announce how many files will be zipped up
  if(length(files)==0) {
    if(announce) note("No files found using mask '",mask,"' (after possible exclusion via 'mask_exclude').")
    return(list(ios=1))
  } else {
    if(announce) msgf("Total of ",length(files)," found using mask '",mask,"'.\nZipping up ...")
  }
  
  ## Zip them up into a single archive (if single_archive is TRUE) or each individually (otherwise)
  ichunk = 0
  zf = NULL
  chunk = if(!single_archive) 1 else max(1, chunk)
  while(length(files)>0) {
    
    # Define a random (short) file name (if not given on input)
    if(is.null(zf) || !single_archive) zf = paste0(random_filename(),".zip")
    
    # If single archive should be produced, add the flag 'g' (from the 2nd file on)
    ichunk = ichunk + 1
    if(single_archive && ichunk==2) extras = paste0("-", sub("-","",extras), "g")
    
    # Zip the current file up. Possibly try a few times in case a file access error occurred
    iis = c(1,rep(2,5))[c(TRUE, rep(retry,nretries))]
    for(i in iis) {
      
      setwd(path)
      ios = zip(zf, head(files, chunk), extras=extras)
      setwd(odir)
      
      check_zip_ios(ios)
      
      if(ios==12) break
      if(ios==15 && i<max(iis)) {
        Sys.sleep(i^2*0.05)
        next
      }
      
      if(ios!=0 && browser_on_error) browser()
      
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

#' @title
#' Compress the entire path
#'
#' @description
#'
#' `zip_all_in_path` compresses all files in the supplied path 
#' (`path`) each into its own archive
#'
#' @param path Path to the files to zip up.
#' @param check_status Determines whether a check for zero status 
#' code is performed.
#' @param extras Modifiers sent to the zip archiver. Defaults to 
#' '-m', which moves the files into the archive.
#' @param disable_warning Disables warning about archiving all files 
#' in the given path. Caution advised.
#'
#' @return List of names of archived files, list of the created 
#' archives and the corresponding status codes returned by the archiver.
#'
#' @family compression-related utilities provided by utilbox
#' @export
zip_all_in_path = function(path=".", check_status=FALSE, extras="-m", disable_warning=FALSE) {

  odir = getwd()
  on.exit(setwd(odir))
  
  if(!disable_warning)
    wait("WARNING: This will zip up all files in the path '",path,"' relative",
         " to the current working directory '",odir,"'. Be careful!")
  
  ds = setdiff(list.dirs(), "..")
  files = zipfiles = Ios = NULL
  for(d in ds) {
    
    setwd(d)
    
    fs = setdiff(setdiff(list.files(), list.dirs(full.names=FALSE)), list.files(pattern="[.]zip$"))
    
    for(f in fs) {
      of = paste0(f,".zip")
      ios = zip(of, f, extras=extras)
      if(check_status) check_zip_ios(ios, f)
      files = c(files, paste0(d, f))
      zipfiles = c(zipfiles, paste0(d, of))
      Ios = c(Ios, ios)
    }
    
    setwd(odir)
    
  }
  
  msgf("Finished.")
  
  invisible(nlist(files, zipfiles, ios=Ios))
  
}

#' @title
#' `unzip_files` decompresses from `zipfile` all files that match the 
#' pattern
#' @description
#'
#' in `mask`, or those that do not match it if `mask_exclude` is 
#' `FALSE`.
#'
#' @param zipfiles List of files to decompress. Actual file names, 
#' not regular patterns.
#' @param mask Regular pattern applied to the files inside the 
#' archives and only the files that match this pattern (or do not match 
#' this pattern when `mask_exclude` is `FALSE`) and only files meeting 
#' these conditions are decompressed.
#' @param mask_exclude Determines how the `mask` is applied.
#' @param patternize Determines whether the mask is treated exactly 
#' (when \code{patternize=TRUE}) or whether it is treated as a regular 
#' pattern (when \code{patternize=FALSE}).
#'
#' @return List of file names of the decompressed files and the 
#' corresponding status codes returned by the archiver.
#'
#' @family compression-related utilities provided by utilbox
#' @export
# Old name: un_zip
unzip_files = function(zipfiles, mask=".*", mask_exclude=FALSE, patternize=TRUE) {
  lapply(zipfiles, unzip_files_single, mask, mask_exclude, patternize)
}

#' @rdname zip_file
unzip_files_single = function(zipfile, mask=".*", mask_exclude=FALSE, patternize=TRUE) {
  
  if(!file.exists(zipfile)) 
    stop("File '",zipfile,"' does not exist.")
  
  ## List the files matching mask
  if(patternize) mask = str_patternize(mask)
  files_in_zip = unlist(list_zip(zipfile, pattern=mask, exclude=mask_exclude))

  ## Nothing to unzip
  if(is_empty(files_in_zip)) return(1)

  ## Unzip the files
  unzipped_files = unzip(zipfile, files_in_zip)
  
  ## Check success
  patterns = str_patternize(files_in_zip)
  ok = all(sapply(patterns, function(p) any(grepl(p, unzipped_files))))
  
  return(list(ios=ok-1, files=unzipped_files))
  
}

#' @title
#' Read file inside a zip archive
#'
#' @description
#'
#' `read_zip()` reads file from within a zip archive. The contents
#' are read using the function in `fun_read` (`utils::read.table` 
#' by default). Specific files to read from the zip archive can be
#' supplied via `files`. Use other arguments to control which files
#' get read such as `pattern` (only file matching the pattern are 
#' read from the archive), `maxnfiles` (to limit the number of files 
#' to read), `skipnfiles` (to skip reading some files). The contents
#' are returned as a named list (unless `nonames=TRUE`, when the list
#' is unnamed) sorted by name (unless `sort_by_name=FALSE`). Attempts
#' are made to deal with excessively long file names (unless
#' `solve_long_name=FALSE`), while `maxnchar` sets the cut-off for 
#' what is considered a long file name and `long_action` determines
#' whether this happens via renaming (when `long_action='rename'`)
#' or by copying (when `long_action='copy'`).
#'
#' @family compression-related utilities provided by utilbox
#' @export
read_zip = function(zipfiles, files=NULL, pattern=NULL, maxnfiles=Inf, skipnfiles=0, 
  nonames=FALSE, sort_by_name=TRUE, solve_long_name=TRUE, maxnchar=128, 
  long_action=c("copy","rename"), attempt_read_plain=FALSE, trace=0, 
  fun_read=read.table, ...) {
  
  # Process what to do to avoid long file name problems
  long_action = match.arg(long_action)
  fun_action = if(long_action=="rename") file_rename else file.copy
  
  # Read the zip file(s)
  RES = NULL
  for(zipfile in zipfiles) {
  
    # Extract the extension
    zip_type = sub(".*[.]","",zipfile)
		
		if(all(zip_type!=c("zip","gz")) && attempt_read_plain) zip_type = "plain"
		
    # Get the set of active connections
    list_connections = getAllConnections()
    
    # If a list of files inside zipfiles is missing, list them all
    file_list = if(!is.null(files)) files else 
                if(zip_type=="zip") unzip(zipfile, list=TRUE)$Name else 
                if(zip_type=="plain") zipfile else "'*'"
    
    # Transmit the file size info as an attribute if reading is to
    # be done using function 'read_char'
    if(zip_type=="zip" && identical(fun_read, read_char)) {
      file_list = list_set_attr(file_list, 'size', unzip(zipfile, list=TRUE)$Length)
      #sizes = unzip(zipfile, list=TRUE)$Length
      #file_list = lapply(1:length(file_list), 
      #                   function(i) { f = file_list[i]; attr(f, 'size') = sizes[i]; f })
    }

    # Skip the desired number of files and limit the number of files to the given maximum
    if(skipnfiles>0) {
      file_list = tail(file_list, -skipnfiles)
    }
    if(maxnfiles>0 && maxnfiles<length(file_list)) {
      file_list = head(file_list, maxnfiles)
    }
    
    # Match the mask
    if(!is.null(pattern)) {
      file_list = file_list[grepl(pattern, file_list)]
    }

    # Make sure the file names are not too long
    restore_file = FALSE
    if(nchar(zipfile) > maxnchar && any(zip_type==c("zip","gz"))) {

      warn("Zip file name is long (",nchar(zipfile)," characters), which might",
           " cause errors while unzipping (Use arguments 'solve_long_name' and",
           " 'long_action' to enable a solution).", skip1=1, skip2=0)
      
      if(solve_long_name) {
      
        if(trace>0) msgf("Shortening the zip file name by ",long_action,"ing ...")
        fn = paste0(random_filename(),".zip")
        if(nchar(fn) > maxnchar) {
          root = sub("/.*","/",getwd())
          fn = paste0(root,fn)
        }
        
        msgf(toupperfirst(ifelse(long_action=="rename", "renam", long_action)),"ing file '",
             zipfile,"' to file '",fn,"' (in path '",getwd(),"') ...")
        
        fun_action(zipfile, fn)
        zipfile_orig = zipfile
        zipfile = fn
        restore_file = TRUE

        if(trace==0) cat("Reading ",length(file_list)," file(s) from archive '",zipfile,"' ...\n")

      } # if(solve_long_name)
      
    }
    
    # Loop over file names in the file list inside the archive
    for(file in file_list) {
      RES = append(RES, read_zip_single(file, zipfile, zip_type, list_connections, fun_read, trace, ...))
    }
        
    # Remove the extra copy
    if(restore_file) {
      msgf("Restoring original file names (by ",ifelse(long_action=="rename","renaming","deletion"),") ...")
      ios = try(if(long_action=="rename") file_rename(zipfile, zipfile_orig) else file_remove(zipfile), silent=TRUE)
    }
          
  } # for(zipfile in zipfiles)
  
  # Sort the result list according to names
  if(sort_by_name && length(RES)>0) {
    RES = sort_by_names(RES)
  }
  
  # Assign names to the elements in the result list
  if(nonames) names(RES) = NULL
  
  return(RES)

} # read_zip

#' @title
#' Read a table from a single zip archive
read_zip_single = function(file, zipfile, zip_type, list_connections, fun_read, trace=0, ...) {
  
  if(missing(file) || length(file)!=1) 
    stop('Supply exactly one file name to read from the zip archive.')
  
  if(missing(zipfile) || length(zipfile)!=1) 
    stop('Supply exactly one zip archive file name.')

  if(missing(fun_read))
    stop('Supply a value for the argument fun_read (e.g. read_char or read_table)')
    
  if(!is.function(fun_read))
    stop('The object in fun_read must be a function.')
    
  if(missing(list_connections))
    stop('Supply a list of open connections for comparison.')
  
  # Open connection
  if(zip_type=="zip") {
  
    infile = unz(zipfile, file)
    nam = paste0(zipfile,":",file)
  
  } else if(zip_type=="gz") {
  
    infile = gzfile(zipfile)
    nam = zipfile
  
  } else if(zip_type=="plain") {
    
    warn("Unknown compression format of file ",zipfile,". Attempting to read as plain text ...")
    
    infile = file
    nam = paste0(zipfile,":",file)
    
  } else stop("Unknown compression format of file ",zipfile,".")

  if(identical(fun_read, read_char)) 
    attr(infile, 'size') = attr(file, 'size')
  
  # Find the newest connection
  active_connection = setdiff(getAllConnections(), list_connections)

  # Read the file using function 'fun_read'
  if(trace>0) msgf("Reading file ",file," from inside ",zipfile," ...")
  
  res = list(fun_read(infile, ...))
  names(res) = nam
  
  # Close the active connection (if still open)
  if(any(getAllConnections()==active_connection)) 
    close(getConnection(active_connection))
  
  res
  
}

#' @title
#' Process the status of a zip call
check_zip_ios = function(ios, file) {

  if(ios==0) return(invisible(nlist(ios, msg='OK')))
  
  msg = paste0(
    'Status code ', ios, ': There was a problem during zipping', 
    if(!missing(file)) paste0('(file: ', file, ')'),'.',
    if(ios==127) {
      paste0('The zip archiver appears to be missing.', if(is_win()) ' (Hint: Install Rtools)')
    } else if(ios==12) {
      paste0('The zip archiver reported an error "name not matched". This could occurs for ',
             'instance when file names are too long. Attempting to continue regardless ...')
    }
  )
           
  note(msg)
  return(invisible(nlist(ios, msg)))
      
}
