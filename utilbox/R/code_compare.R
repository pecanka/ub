#' @title
#' Compare functions
#'
#' @description
#' Compares two functions by looking at their arguments (via 
#' [`base::args()`]) and their bodies (via [`base::body()`]).
#'
#' @examples
#' f1 = function (x) .Internal(which.max(x))
#' identical(f1, base::which.max)                              # returns FALSE, since they are defined in different environments
#' compare_functions(f1, base::which.max)                      # returns TRUE
#' compare_functions('f1', 'base::which.max', by_name=TRUE)    # returns TRUE
#'
#' @family coding-related functions provided by utilbox
#' @export
compare_functions = function(fun1, fun2, by_name=FALSE, envir1, envir2) {
  
  if(by_name && is.character(fun1)) {
    fun1 = get2(fun1, envir, mode=mode)
  }
  if(by_name && is.character(fun2)) {
    fun2 = get2(fun2, envir, mode=mode)
  }
  
  #if(by_name) {
  
    #if(is.character(fun1)) {
    #  fun1 = if(missing(envir1)) {
    #    get(fun1, mode='function') 
    #  } else {
    #    get(fun1, envir=envir1, mode='function')
    #  }
    #}
    
    #if(is.character(fun2)) {
    #  fun2 = if(missing(envir2)) {
    #    get(fun2, mode='function') 
    #  } else {
    #    get(fun2, envir=envir2, mode='function')
    #  }
    #}
    
  #}
  
  stopifnot(is.function(fun1), is.function(fun2))
  
  identical(args(fun1), args(fun2)) && identical(body(fun1), body(fun2))
  
}  

#' @title
#' Compare code in R scripts
#'
#' @description
#' `compare_code()` takes two R scripts and performs a comparison. 
#' First, the actual codes are compared as character strings. If they 
#' fail this sameness check, the two scripts are sourced into separate 
#' environments, which are then compared. In this comparison, the names 
#' of all objects are compared (check 1), then the classes of all 
#' objects (check 2), then functions are checked for identical arguments 
#' and bodies (check 3), and finally non-function objects are compared 
#' in terms of values (check 4).
#'
#' @returns A logical indicating whether the two codes were found to 
#' to be identical together with the number of the first failed check 
#' (attribute `check_failed`, where value 0 indicates 'no failure') and 
#' the names of the objects that were found to be different (attribute 
#' `objects`)..
#'
#' @export
compare_script_code = function(file1, file2, verbose=TRUE) {

  if(verbose) {
    cat0("Comparing files '",file1,"' and '",file2,"' ... ")
    on.exit(catn(if(are_same) 'PASS' else 'DIFFERENCES FOUND!'))
  }
  
  # read the file sources
  C1 = readLines(file1)
  C2 = readLines(file2)
  
  # remove all commented lines
  C1 = C1[C1 %notlike% '^\\s*#']
  C2 = C2[C2 %notlike% '^\\s*#']
  
  # check for identical code
  if(identical(C1, C2)) {
    return(are_same <- structure(TRUE, check_failed = 0))
  }
  
  # if not, execute the code and compare the environments
  writeLines(C1, f1 <- '.~C1.R.tmp')
  writeLines(C2, f2 <- '.~C2.R.tmp')
  sys.source(f1, envir=e1 <- new.env())
  sys.source(f2, envir=e2 <- new.env())
  file.remove(f1, f2)

  # if the object names do not match, return severe difference
  list1 = ls(envir=e1, all=TRUE, sorted=TRUE)
  list2 = ls(envir=e2, all=TRUE, sorted=TRUE)
  if(!identical(list1, list2)) {
    objects = setdiffsym(list1, list2, labels=c(file1, file2))
    return(are_same <- structure(FALSE, check_failed=1, objects=objects))
  }
  
  # compare classes of objects
  class1 = sapply(list1, function(obj) class(get(obj, envir=e1)))
  class2 = sapply(list2, function(obj) class(get(obj, envir=e2)))
  if(!identical(class1, class2)) {
    objects = list(list1[class1!=class2], list2[class1!=class2])
    names(objects) = c(file1, file2)
    return(structure(FALSE, check_failed=2, objects=objects))
  }
  
  # compare all functions by code
  is_fun = sapply(class1, function(cls) identical(cls, 'function'))
  funs_equal = sapply(list1[is_fun], function(obj)
    compare_functions(obj, obj, by_name=TRUE, envir1=e1, envir2=e2))
    
  # compare all non-functions by value
  rest_equal = sapply(list1[!is_fun], function(obj) 
    identical(get(obj, envir=e1), get(obj, envir=e2)))
  
  # final verdict
  if(all(funs_equal) && all(rest_equal)) {
    are_same <- structure(TRUE, check_failed=0)
  } else {
    objects1 = c(list1[is_fun][!funs_equal], list1[!is_fun][!rest_equal]) 
    objects2 = c(list2[is_fun][!funs_equal], list2[!is_fun][!rest_equal])
    objects = structure(list(objects1, objects2), names=c(file1, file2))
    are_same <- structure(FALSE, check_failed=3, objects=objects)
  }
  
  are_same
    
}

