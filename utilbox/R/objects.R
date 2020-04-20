#' @title
#' Remove all objects
#'
#' @description
#' `.rma()` is a shorthand for removal of all objects (including 
#' hidden ones whose names start with a dot) from the environment in 
#' `envir` (`.GlobalEnv` by default). This function is basically an 
#' alias for \code{rm(list=ls(envir=envir))}.
#'
#' Optionally, names of objects that should be excluded from removal 
#' can be supplied via `keep`.
#'
#' @family object listing, object loading and removing functions provided by utilbox
#' @export
.rma = function(all.names=FALSE, keep=".utilbox", envir=.GlobalEnv) {
  rm(list=setdiff(ls(envir=envir, all.names=all.names), keep), envir=envir)
}

#' @title
#' Delete objects
#'
#' @description
#' `rme()` deletes all supplied objects in the environment specified 
#' in `envir`. Unlike the basic \code{base::rm} function, the current 
#' function checks if the supplied objects exist in order to avoid 
#' errors. Objects can be supplied as character strings or as symbols.
#'
#' @examples
#' random_object = '1'
#' rme(random_object)      # delete successfully
#' rme(random_object)      # nothing to delete, but no error/warning
#'
#' @export
.rme = function(..., envir=parent.frame()) {
  dots = dots_to_nlist()
  suppressWarnings(do.call(rm, append(dots, list(envir=envir))))
  #suppressWarnings(rm(..., envir=envir))
}

#.rme = function(y, envir=parent.frame(), treat_as_symbol=FALSE) {
#  nam = if(treat_as_symbol) as.character(substitute(y)) else y
#  if(exists(nam, envir=envir)) rm(list=nam, envir=envir)
#  #if(length(ls(envir=envir, pattern=paste0("^",y,"$")))>0) rm(list=y, envir=envir)
#}

#' @title
#' Same object check
#'
#' @description
#' `is_same_object()` checks whether all of the supplied arguments 
#' correspond to the same object in memory. Arguments can be given as 
#' variables or by name (as character).
#'
#' @examples
#' a = 1; b = a; c = 2
#' is_same_object(a, b)     # returns TRUE
#' is_same_object(a, b, c)  # returns FALSE
#' is_same_object(a, "a")   # returns TRUE
#'
#' @export
is_same_object = function(..., envir = parent.frame()) {
  dots = match.call(expand.dots = FALSE)$`...`
  args = as.character(dots)
  mems = sapply(args, function(arg) tracemem(get(arg, envir=envir)))
  nunique(mems) == 1
}

#' @title
#' Functions for memory unit conversion
#'
#' @description
#' `get_unit()` determines the appropriate unit of memory for the 
#' size of the object `x`.
#'
#' `de_unit()` returns the size of the unit in 'unit'.
#'
#' `convert_unit()` converts from unitary units (e.g. number of 
#' bytes) to an appropriate other unit.
#'
#' @examples
#' get_unit(rep(1,1e7))
#'
#' @export
get_unit = function(n) {
  w = pmax(1, pmin(trunc(log10(n)/3)+1, 5))
  ifelse(is.na(w), w, switch(w, "B", "kB", "MB", "GB", "TB"))
  #sapply(w, function(y) ifelse(is.na(y), y, switch(y, "B", "kB", "MB", "GB", "TB")))
}

#' @rdname get_unit
#' @export
de_unit = function(unit) {
  ifelse(is.na(unit), NA, switch(unit,"B"=1,"kB"=1e3,"MB"=1e6,"GB"=1e9,"TB"=1e12))
  #return(if(is.na(unit) || is.nan(unit)) NA else switch(unit, "B"=1, "kB"=1e3, "MB"=1e6, "GB"=1e9, "TB"=1e12))
}

#' @rdname get_unit
#' @export
convert_unit = function(x, unit, append_unit=TRUE, ndigit=3) {

  if(missing(unit)) unit = get_unit(x)

  s = rsignif(x / sapply(unit, de_unit), ndigit)
  
  if(append_unit) as.character(s) %.% unit else s

}

#' @title
#' List all objects and their sizes
#'
#' @description
#' `lsos()` lists objects in the given environment and returns their 
#' names and sizes.
#'
#' `obj_size()` returns the sizes of supplied objects (in memory unit 
#' given in 'unit').
#'
#' @export
lsos = function(..., envir=parent.frame(), n=10) {
  .ls.objects(..., envir=envir, order.by="Size in bytes", decreasing=TRUE, head=TRUE, n=n)
}

#' @rdname lsos
#' @export
obj_size = function(..., list=character(), unit="B", with_unit=TRUE, envir=parent.frame(), ndigit=2) {
  dots = match.call(expand.dots = FALSE)$`...`
  
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || is.character(x)))) 
    error("'...' must contain names or character strings.")
    
  names = c(list, sapply(dots, as.character))
  
  # Get object sizes
  s = try(sapply(names, function(x) if(exists(x, envir=envir)) object.size(get(x, envir=envir)) else NA))
  
  # Convert the sizes in bytes to different units (unless error occured)
  if(class(s)=="try-error") {
  
    s = rep(NA, length(names))
    
  } else if(length(s)>0) {
    
    # Determine suitable unit
    if(missing(unit) && with_unit) unit = get_unit(s)
    
    # Convert units
    s = convert_unit(s, unit, append_unit=with_unit)
    
  }
  
  return(s)
}

.ls.objects = function(pos=1, pattern, envir=parent.frame(), order.by, decreasing=FALSE, 
  head=FALSE, n=5, all.names=TRUE) {
  
  napply = function(names, fn) sapply(names, function(x) fn(try(get(x, envir=envir))))
  
  names = ls(pos=pos, pattern=pattern, envir=envir, all.names=all.names)
  
  obj.class = napply(names, function(y) as.character(class(y))[1])
  obj.mode = napply(names, mode)
  obj.type = ifelse(is.na(obj.class), obj.mode, obj.class)
  #obj.size = napply(names, function(y) object.size(get(y, envir=envir)))
  obj.size = napply(names, function(y) object.size(y))
  #obj.size = obj_size(list=names, with_unit=TRUE)
  #obj.dim = t(napply(names, function(y) as.numeric(dim(get(y, envir=envir)))[1:2]))
  obj.dim = t(napply(names, function(y) as.numeric(dim(y))[1:2]))
  vec = is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] = napply(names, length)[vec]
  out = data.frame(obj.type, obj.size, obj.dim)
  names(out) = c("Type", "Size in bytes", "Rows", "Columns")
  
  if(!missing(order.by))
    out = out[order(out[[order.by]], decreasing=decreasing),]
  
  if(head) out = head(out, n)
  
  return(out)

}

#' @title
#' List and load objects from a file
#'
#' @description
#' `load_objects()` is a more verbose version of `base::load()`. 
#' `load_objects()` can announce which objects have been loaded, the 
#' user can also specify which objects are expected and an error is 
#' thrown if any of these expected objects are not found inside the file.
#'
#' `loadAs()` allows to loads objects from a file and assigns their 
#' contents into variables listed in `as` inside the environment in 
#' `envir` (the parent frame of the call to `loadAs()` by default).
#'
#' `list_all_objects()` lists all objects inside given files or 
#' inside files that match pattern in `pattern` relative to the path in 
#' `dir`.
#'
#' `find_object()` searches for an object with name in `object_name` 
#' inside files in `files` relative to the path supplied in `dir`. If a 
#' pattern is given (via `pattern`) instead of a list of file names, 
#' then all files that match the pattern are searched through looking 
#' for the object in `object_name`.
#'
#' @export
load_objects = function(file, announce=FALSE, list_new=FALSE, expected_objects=NULL, 
                        quit_on_miss=FALSE, envir=parent.frame()) {
                        
  # Check for non-scaler file name
  if(length(file)!=1) 
    error("Supply a single file name.")
  
  # Announce loading and file name
  if(announce) 
    catn("All objects from file '",file,"' will be loaded ...")
  
  # Check for missing file
  if(!file.exists(file)) 
    error("File '",file,"' does not exist.")
  
  # Environment "local" means this function
  if(class(envir)=="character" && envir=="local") 
    envir = environment()

  ## Remove the objects that are in the file from environment 'envir'
  rme(expected_objects, envir=envir)
  
  # Load the file
  if(announce) cat0("Loading file (size ",file_size(file),") ... ")
  loaded = load(file, envir=envir)
  if(announce) cat0("done.\n")
  
  # List new objects loaded from the file
  if(list_new) {
    catn("Getting sizes of loaded objects ...")
    sizes = obj_size(list=loaded, with_unit=TRUE, envir=envir)
    catn("Loaded objects: ",paste(loaded," (",sizes,")",sep="",collapse=", "))
  } else 
    sizes = rep(NA, length(loaded))
  
  # Check for missing objects
  if(!is.null(expected_objects)) {
    miss = setdiff(expected_objects, loaded)
    if(length(miss)>0) {
      msg = paste0("The following expected objects were not loaded: '",paste(miss,collapse="' '"),"'")
      if(quit_on_miss) error(msg) else warn(msg)
    } else {
      catn("All expected objects were successfully loaded.")
    }
  }

  return(invisible(cbind("Object"=loaded, "Size"=sizes)))

} # load_objects

#' @rdname load_objects
#' @export
loadAs = function(file, as, what, envir=parent.frame()) {
  
  loaded = load(file, envir=environment())
  
  if(missing(as)) as = loaded
  if(missing(what)) what = loaded
  
  stopifnot(length(what)==length(as))
  
  if(any(what %notin% loaded))
    error("Object(s) '",what[which(what %notin% loaded)],"' were not found in file '",file,"'.")
  
  for(i in 1:length(what)) assign(as[i], get(what[i]), envir=envir)
  for(x in setdiff(loaded, what)) assign(x, get(x), envir=envir)
  
  invisible(cbind(what=what, as=c(as,setdiff(loaded, what))))
  
}

#' @rdname load_objects 
#' @export
get_all_objects = function(files=NULL, dir=".", pattern="^.*[.]RData$") {

  setwd(dir)
  if(missing(files)) files = list.files(pattern=pattern)
  cat("Obtaining names of all objects ...\n"); .fc()
  loaded = list()
  
  ## Load all files
  for(file in files)
    loaded[[file]] = load_objects(file, list_new=TRUE, announce=TRUE, envir="local")
  
  cat("\nThe files contain the following objects (per file):\n\n")  
  print(loaded)
  
  return(invisible(NULL))

} 

#' @rdname load_objects
#' @export
find_object = function(object_name, files=NULL, dir=".", pattern="^.*[.]RData$", 
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
      catn("Unfortunately the object '",object_name,"' could not be find in any of the files.")
    } else if(stop_on_found) {
      catn("Object '",object_name,"' found in file '",identified_files,"'. Search stopped.")  
    } else {
      catn("Object '",object_name,"' found in ",length(identified_files)," files.")  
    }
  }
  
  return(identified_files)
} 

