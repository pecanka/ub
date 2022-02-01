#' @title
#' Remove objects
#'
#' @description
#'
#' `.rma()` is a shorthand for removal of all objects (including 
#' hidden ones whose names start with a dot) from the environment in 
#' `envir` (`.GlobalEnv` by default). This function is basically an 
#' alias for `rm(list=ls(envir=envir))`. Optionally, names of 
#' objects that should be excluded from removal can be supplied via `keep`.
#'
#' `rme()` is a selective delete, which removes all supplied objects in the
#' environment specified 
#' in `envir`. Unlike the basic `base::rm` function, the current 
#' function checks if the supplied objects exist in order to avoid 
#' errors. Objects can be supplied as character strings or as symbols.
#'
#' @examples
#' random_object = '1'
#' rme(random_object)      # delete successfully
#' rme(random_object)      # nothing to delete, but no error/warning
#'
#' @name remova_objects
#' @family object listing, object loading and removing functions provided by utilbox
#' @export
.rma = function(all.names=FALSE, keep=".utilbox", envir=.GlobalEnv) {
  base::rm(list=setdiff(ls(envir=envir, all.names=all.names), keep), envir=envir)
}

#' @rdname remova_objects
#' @export
.rme = function(..., envir=parent.frame()) {
  dots = as.character(dots_to_nlist(keep_symbolic=TRUE))
  suppressWarnings(do.call(base::rm, list(list=dots, envir=envir)))
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
#'
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
#'
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
#' de_unit(c('kB','GB')
#'
#' @export
get_unit = function(n) {

  units = c("B", "KB", "MB", "GB", "TB", "PB")
  
  w = pmax(1, pmin(trunc(log10(n)/3)+1, length(units)))
  
  ifelse(is.na(w), w, units[w])
  #ifelse(is.na(w), w, switch(w, "B", "kB", "MB", "GB", "TB"))
  #sapply(w, function(y) ifelse(is.na(y), y, switch(y, "B", "kB", "MB", "GB", "TB")))
  
}

#' @rdname get_unit
#' @export
de_unit = function(unit) {

  units = c("B"=1, "K"=1e3, "KB"=1e3, "M"=1e6, "MB"=1e6, "G"=1e9, "GB"=1e9, "T"=1e12, "TB"=1e12, "P"=1e15, "PB"=1e15)
  
  ifelse(is.na(unit), NA, units[toupper(unit)])
  #ifelse(is.na(unit), NA, switch(toupper(unit),"B"=1,"K"=, "KB"=1e3, "M"=, "MB"=1e6, "G"=, "GB"=1e9, "T"=, "TB"=1e12))
  #return(if(is.na(unit) || is.nan(unit)) NA else switch(unit, "B"=1, "kB"=1e3, "MB"=1e6, "GB"=1e9, "TB"=1e12))
  
}



#' @rdname get_unit
#' @export
convert_unit = function(x, unit, append_unit=TRUE, ndigits=3, sep=" ") {

  if(missing(unit)) unit = get_unit(x)

  s = round(x / sapply(unit, de_unit), ndigits)
  
  if(append_unit) paste(format(s), unit, sep=sep) else s

}

#' @title
#' List all objects and their sizes
#'
#' @description
#'
#' `lsos()` lists objects in the given environment and returns their 
#' names and sizes. It does so via `list_object_sizes()`, which has
#' a few more optional arguments and can also be called directly.
#'
#' `object_sizes()` returns the sizes of supplied objects (in memory unit 
#' given in 'unit').
#'
#' @export
lsos = function(..., envir=parent.frame(), n=10, ndigits=2, size_fun=NULL) {
  list_object_sizes(..., envir=envir, order_by="Size in bytes", decreasing=TRUE, 
                    head=TRUE, n=n, ndigits=ndigits, size_fun=size_fun)
}

#' @rdname lsos
#' @export
object_size = function(name, envir=parent.frame(), size_fun=object.size) {

  if(!is.character(name))
    error("The argument 'name' of 'object_size' must be of class character.")
    
  if(missing(size_fun) || is.null(size_fun))
    size_fun = object.size

  if(!is.function(size_fun))
    error("The value supplied to 'object_size' via 'size_fun' must be a function.")
    
  mem = try(size_fun(get(name, envir=envir)), silent=package_is_installed('pryr'))
  
  if(is_error(mem) && package_is_installed('pryr')) {
      mem = try(pryr::object_size(get(name, envir=envir)))
  }
  
  if(is_error(mem))
    error("A memory query on the object '",name,"' failed.")
    
  as.numeric(mem) 
   
}

#' @rdname lsos
#' @export
object_sizes = function(..., list=character(), unit="B", with_unit=TRUE, envir=parent.frame(), 
  ndigits=2, size_fun=NULL) {
  
  dots = match.call(expand.dots = FALSE)$`...`
  
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || is.character(x)))) 
    error("'...' must contain names or character strings.")
    
  names = c(list, sapply(dots, as.character))
  
  # Get object sizes
  s = try(sapply(names, function(x) 
                          if(exists(x, envir=envir)) {
                            object_size(x, envir=envir, size_fun=size_fun) 
                          } else NA))
  
  # Convert the sizes in bytes to different units (unless error occured)
  if(is_error(s)) {
  
    s = rep(NA, length(names))
    
  } else if(length(s)>0) {
    
    # Determine suitable unit
    if(missing(unit) && with_unit) {
      unit = get_unit(s)
    }
    
    # Convert units
    s = data.frame(bytesize=s, size=convert_unit(s, unit, append_unit=with_unit, ndigits=ndigits))
    
  }
  
  return(s)
}

#' @rdname lsos
#' @export
list_object_sizes = function(pos=1, pattern, envir=parent.frame(), order_by, decreasing=FALSE, 
  head=FALSE, n=5, all.names=TRUE, ndigits=2, size_fun=NULL) {
  
  napply = function(names, fn) sapply(names, function(x) fn(try(get(x, envir=envir))))
  
  names = ls(pos=pos, pattern=pattern, envir=envir, all.names=all.names)
  
  if(length(names)==0) {
    message("No (matching) objects found in ",print2var(envir),".")
    return(invisible(NULL))
  }
  
  obj_class = napply(names, function(y) as.character(class(y))[1])
  obj_mode = napply(names, mode)
  obj_type = ifelse(is.na(obj_class), obj_mode, obj_class)
  
  #obj_size = napply(names, function(y) object.size(get(y, envir=envir)))
  #obj_size_byte = napply(names, function(y) object_size(y, size_fun=size_fun))
  #obj_size_byte = sapply(names, function(y) object_size(y, size_fun=size_fun))
  obj_size = object_sizes(list=names, with_unit=TRUE, ndigits=ndigits, size_fun=size_fun)
  
  #obj_dim = t(napply(names, function(y) as.numeric(dim(get(y, envir=envir)))[1:2]))
  #obj_dim = t(napply(names, function(y) as.numeric(dim(y))[1:2]))
  #vec = is.na(obj_dim)[, 1] & (obj_type != "function")
  #obj_dim[vec, 1] = napply(names, length)[vec]

  obj_len = napply(names, length)
  obj_dim = napply(names, function(y) paste(dim(y), collapse=' x '))
  
  name_envir = if(length(obj_class)==0) NULL else print2var(envir)

  out = data.frame(obj_type, obj_size, obj_len, obj_dim, name_envir)
  names(out) = c("Type", "Size in bytes", "Size", "Length", "Dimensions", "Location")
  
  if(!missing(order_by))
    out = out[order(out[[order_by]], decreasing=decreasing),]
  
  if(head) {
    out = head(out, n)
  }
  
  return(out)

}

#' @title
#' List and load objects from a file
#'
#' @description
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
    message("All objects from file '",file,"' will be loaded ...")
  
  # Check for missing file
  if(!file.exists(file)) 
    error("File '",file,"' does not exist.")
  
  # Environment "local" means this function
  if(class(envir)=="character" && envir=="local") 
    envir = environment()

  ## Remove the objects that are in the file from environment 'envir'
  .rme(expected_objects, envir=envir)
  
  # Load the file
  if(announce) cat0("Loading file (size ",file_size(file),") ... ")
  loaded = load(file, envir=envir)
  if(announce) cat0("done.\n")
  
  # List new objects loaded from the file
  if(list_new) {
    message("Getting sizes of loaded objects ...")
    sizes = object_sizes(list=loaded, with_unit=TRUE, envir=envir)
    message("Loaded objects: ",paste(loaded," (",sizes,")",sep="",collapse=", "))
  } else 
    sizes = rep(NA, length(loaded))
  
  # Check for missing objects
  if(!is.null(expected_objects)) {
    miss = setdiff(expected_objects, loaded)
    if(length(miss)>0) {
      msg = paste0("The following expected objects were not loaded: '",paste(miss,collapse="' '"),"'")
      if(quit_on_miss) error(msg) else warn(msg)
    } else {
      message("All expected objects were successfully loaded.")
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
      message("Unfortunately the object '",object_name,"' could not be find in any of the files.")
    } else if(stop_on_found) {
      message("Object '",object_name,"' found in file '",identified_files,"'. Search stopped.")  
    } else {
      message("Object '",object_name,"' found in ",length(identified_files)," files.")  
    }
  }
  
  return(identified_files)
} 

