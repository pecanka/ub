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
  list_object_sizes(..., envir=envir, order_by="bytesize", decreasing=TRUE, 
                    head=TRUE, n=n, ndigits=ndigits, size_fun=size_fun)
}

#' @rdname lsos
#' @export
object_size = function(name, envir=parent.frame(), size_fun=object.size) {

  if(!is.character(name))
    stop("The argument `name` of `object_size` must be of class character.")
    
  if(missing(size_fun) || is.null(size_fun))
    size_fun = object.size

  if(!is.function(size_fun))
    stop("The value supplied to `object_size` via `size_fun` must be a function.")
    
  mem = try(size_fun(get(name, envir=envir)), silent=package_is_installed('pryr'))
  
  if(is_error(mem) && package_is_installed('pryr')) {
    mem = try(pryr::object_size(get(name, envir=envir)))
  }
  
  if(is_error(mem))
    stop("A memory query on the object `",name,"` failed.")

  as.numeric(mem) 
   
}

#' @rdname lsos
#' @export
object_sizes = function(..., list=character(), unit="B", with_unit=TRUE, envir=parent.frame(), 
  ndigits=2, size_fun=NULL, sort_by=c('size','name')) {
  
  sort_by = match.arg(sort_by)
  
  dots = match.call(expand.dots = FALSE)$`...`
  
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || is.character(x)))) 
    stop("'...' must contain names or character strings.")
    
  obj_names = unname(unlist(c(list, sapply(dots, as.character))))
  
  # Get object sizes
  s = try(sapply(obj_names, function(x) 
                          if(exists(x, envir=envir)) {
                            object_size(x, envir=envir, size_fun=size_fun) 
                          } else NA, USE.NAMES=FALSE))
  
  # Convert the sizes in bytes to different units (unless error occured)
  if(is_error(s)) {
  
    s = rep(NA, length(obj_names))
    
  } else if(length(s)>0) {
    
    # Determine suitable unit
    if(missing(unit) && with_unit) {
      unit = get_unit(s)
    }

    # Combine results and convert units
    s = data.frame(object_name=obj_names, 
                   bytesize=s, 
                   size=convert_unit(s, unit, append_unit=with_unit, ndigits=ndigits))
                   
    s = s[order(if(sort_by=='name') s$object_name else -s$bytesize),]
    
  }
  
  return(s)
}

#' @rdname lsos
#' @export
list_object_sizes = function(pos=1, pattern, envir=parent.frame(), order_by, decreasing=FALSE, 
  head=FALSE, n=5, all.names=TRUE, ndigits=2, size_fun=NULL) {
  
  obj_nams = ls(pos=pos, pattern=pattern, envir=envir, all.names=all.names)
  
  if(length(obj_nams)==0) {
    msgf("No (matching) objects found in ",print2var(envir),".")
    return(invisible(NULL))
  }
  
  GetObj = function(nam) {
    if(nam == '...') {
      eval(quote(list(...)), envir=envir)
    } else {
      get0(nam, envir=envir, ifnotfound=NULL)
    }
  }  
  
  obj_class = sapply(obj_nams, function(nam) as.character(class(GetObj(nam)))[1])
  
  obj_mode = sapply(obj_nams, function(nam) mode(GetObj(nam)))
  obj_type = ifelse(is.na(obj_class), obj_mode, obj_class)
  
  fun_size = if(namespace_exists('pryr')) pryr::object_size else utils::object.size
  obj_size = sapply(obj_nams, function(nam) try(fun_size(GetObj(nam)), silent=TRUE) %ERR% NA_real_)
  obj_size_pretty = convert_unit(obj_size, get_unit(obj_size), ndigits=ndigits)
  
  obj_len = sapply(obj_nams, function(nam) length(GetObj(nam)))
  obj_dim = sapply(obj_nams, function(nam) paste(dim(GetObj(nam)), collapse=' x '))
  
  obj_env_name = if(length(obj_class)==0) NULL else print2var(envir)

  out = data.frame(name = obj_nams, size = obj_size_pretty, bytesize = obj_size, type = obj_type, 
                   length = obj_len, dimension = obj_dim, env_name = obj_env_name)
  
  if(!missing(order_by))
    out = out[order(out[[order_by]], decreasing=decreasing),]
  
  if(head) {
    out = head(out, n)
  }
  
  rownames(out) = NULL
  
  return(out)

}

#' List and get objects in parent frames
#'
#' `ls_parents()` returns a list of all objects in the sequence of parent 
#' frames.
#'
#' `get_in_parents()` retrieves an object by the specified name from all 
#' parent frams where it is found.
#'
#' @export
ls_parents = function(..., stop_at_global=FALSE, envir=parent.frame()) {

  parents = parent_frames(stop_at_global = stop_at_global, envir=envir)
  lapply(parents$frames, function(e) ls(envir=e, ...))
  
}

#' @rdname ls_parents
#' @export
get_in_parents = function(x, envir=parent.frame()) {

  objs = ls_parents(x, all.names=TRUE, envir=envir)
  is_in = unlist(lapply(objs, `%in%`, x=x))
  parents = parent_frames(envir=envir)
  lapply(parents$frames[is_in], function(e) get(x=x, envir=e))
  
}




