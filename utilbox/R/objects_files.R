#' @title
#' List and load objects from a file
#'
#' @description
#'
#' `list_all_objects()` lists all objects inside given files or 
#' inside files that match pattern in `pattern` relative to the path in 
#' `dir`.
#'
#' `find_object_in_files()` searches for an object with name in `object_name` 
#' inside files in `files` relative to the path supplied in `dir`. If a 
#' pattern is given (via `pattern`) instead of a list of file names, 
#' then all files that match the pattern are searched through looking 
#' for the object in `object_name`.
#'
#' `list_objects_in_files()` returns a list of objects in the given files.
#'
#' `load_objects()` is a more verbose version of `base::load()`. 
#' `load_objects()` can announce which objects have been loaded, the 
#' user can also specify which objects are expected and an error is 
#' thrown if any of these expected objects are not found inside the file.
#'
#' `loadAs()` allows to loads objects from a file and assigns their 
#' contents into variables listed in `as` inside the environment in 
#' `envir` (the parent frame of the call to `loadAs()` by default).
#'
#' @export
load_objects = function(file, announce=FALSE, list_new=FALSE, expected_objects=NULL, 
                        quit_on_miss=FALSE, envir=parent.frame()) {
                        
  message = if(silent) function(...) {} else msgf
                        
  if(length(file) != 1) 
    stop("Supply one file name.")
  
  message("All objects from file '",file,"' will be loaded ...")
  
  # Check for missing file
  if(!file.exists(file)) 
    stop("File '",file,"' does not exist.")
  
  # Environment "local" means this function
  if(identical(envir, "local"))
    envir = environment()

  .rme(expected_objects, envir=envir)
  
  message("Loading file (size ",file_size(file),") ... ", appendLF = FALSE)
    
  loaded = load(file, envir=envir)
  
  message("done.\n")
  
  # List new objects loaded from the file
  if(list_new) {
    message("Getting sizes of loaded objects ...")
    sizes = object_sizes(list=loaded, with_unit=TRUE, envir=envir)
    message("Loaded objects: ",paste(loaded," (",sizes,")",sep="",collapse=", "))
  } else {
    sizes = rep(NA, length(loaded))
  }
  
  # Check for missing objects
  if(!is.null(expected_objects)) {
  
    miss = setdiff(expected_objects, loaded)
    
    if(length(miss)>0) {
      msg = paste0("The following expected objects were not loaded: '",paste(miss, collapse="' '"),"'")
      if(quit_on_miss) {
        stop(msg) 
      } else {
        warn(msg)
      }
    } else {
      message("All expected objects were successfully loaded.")
    }
    
  }

  return(invisible(cbind("Object"=loaded, "Size"=sizes)))

} # load_objects

#' @rdname load_objects
#' @export
find_object_in_files = function(object_name, files=NULL, dir=".", pattern="^.*[.]RData$", 
  stop_on_found=TRUE, announce=TRUE) {
                       
  odir = getwd()
  on.exit(setwd(odir))
  
  stopifnot(!missing(object_name) && length(object_name)) 
  
  if(missing(files) || !length(files)) 
    files = list.files(dir, pattern=pattern)

  setwd(dir)
  cat("Searching for object '",object_name,"' ...\n", sep=""); .fc()
  identified_files = NULL
  for(file in files) {
    loaded = load_objects(file, list_new=TRUE, announce=announce, envir="local")
    if(object_name %in% loaded[,1]) {
      identified_files = c(identified_files, file)
      if(stop_on_found) break
    }
  }
  
  if(announce) {
    if(!length(identified_files)) {
      msgf("Unfortunately the object '",object_name,"' could not be find in any of the files.")
    } else if(stop_on_found) {
      msgf("Object '",object_name,"' found in file '",identified_files,"'. Search stopped.")  
    } else {
      msgf("Object '",object_name,"' found in ",length(identified_files)," files.")  
    }
  }
  
  return(identified_files)
} 

#' @rdname load_objects 
#' @export
list_objects_in_files = function(files = NULL, dir=".", pattern="^.*[.]RData$") {

  orig_dir = getwd()
  on.exit(setwd(orig_dir))
  setwd(dir)
  
  if(missing(files)) 
    files = list.files(pattern=pattern)
    
  cat("Obtaining names of all objects ...\n"); .fc()
  loaded = list()
  
  for(file in files)
    loaded[[file]] = load_objects(file, list_new=TRUE, announce=TRUE, envir="local")
  
  cat("\nThe files contain the following objects (per file):\n\n")  
  print(loaded)
  
  return(invisible(NULL))

} 

#' @rdname load_objects
#' @export
loadAs = function(file, as, what, envir=parent.frame()) {
  
  loaded = load(file, envir=environment())
  
  if(missing(as)) 
    as = loaded
  if(missing(what)) 
    what = loaded
  
  stopifnot(length(what)==length(as))
  
  if(any(what %notin% loaded))
    stop("Object(s) '",what[which(what %notin% loaded)],"' were not found in file '",file,"'.")
  
  for(i in 1:length(what)) 
    assign(as[i], get(what[i]), envir=envir)
  
  for(x in setdiff(loaded, what)) 
    assign(x, get(x), envir=envir)
  
  invisible(cbind(what=what, as=c(as,setdiff(loaded, what))))
  
}

