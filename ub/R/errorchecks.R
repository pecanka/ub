#' @title
#' Checks for an error
#'
#' @description
#' Checks whether an object is a result of a failed (i.e. error throwing) 
#' call by checking whether it is of class `try-error` (which is 
#' produced by [`base::try()`]). The argument `ok_value` allows the user 
#' to require that `x` has certain value (the value in `ok_value`) in 
#' order for `x` to not be classified as an error. `ok_set` allows to 
#' give multiple such values. In other words, the content of `ok_value` 
#' is compared directly to `x`, while `ok_set` is gone through and 
#' compared with `x` as a set (via `base::sapply`).
#'
#' @returns Logical indicating whether `x` was classified as an error.
#'
#' @examples
#' is_error(try(x = .this.is.a.missing.variable))
#' is_error(FALSE, ok_value=TRUE)
#'
#' # using the specific value requirement: single value
#' is_error(1, 1)                 # no error
#' is_error(1, 2)                 # an error
#' 
#' # using the specific value requirement: set
#' is_error(1, ok_set=1:2)        # beware: still an error, since the argument is taken
#'                                # as type 'numeric' whereas 1:2 is type 'integer'
#' # define the ok set differently
#' is_error(1, ok_set=c(1,2))
#' # or use a different comparison function
#' is_error(1, ok_set=1:2, comp=equals)
#' 
#' # careful though (equals compares elements)
#' is_error(1, ok_set=list(1), comp=equals)
#' is_error(1, ok_set=list(diag(2)), comp=equals) 
#'
#' # stick with identical for multidimensional objects
#' is_error(diag(2), diag(2))
#' is_error(diag(2), ok_set=list(diag(2)))
#' sapply(list(1, diag(2), diag(3)), is_error, ok_set=list(diag(2), diag(3)))
#'
#' # careful again
#' \dontrun{
#' is_error(diag(2), ok_set=diag(2))
#' }
#'
#' @family coding-related functions provided by ub
#' @export
is_error = function(x, ok_value, ok_set, compare_via=identical, 
    error_classes=c('try-error','simpleError','error'), 
    warning_is_error=TRUE, warning_classes=c('simpleWarning','warning')) 
{
  
  cond1 = !is_empty(intersect(class(x), error_classes))
  cond2 = warning_is_error && !is_empty(intersect(class(x), warning_classes))
  cond3 = !missing(ok_value) && !identical(x, ok_value)
  cond4 = !missing(ok_set) && !any(sapply(ok_set, compare_via, x))
  
  cond1 || cond2 || cond3 || cond4
  
}

#' @title
#' Hide input
#'
#' @description
#'
#' `hide()` is a simple funciton which takes input and returns it back invisibly. 
#' It is useful for instance for hiding errors and warnings within `base::tryCatch` 
#' (see the examples).
#'
#' @examples
#' tryCatch(as.numeric('a'), warning=hide)
#' tryCatch(sin('a'), error=hide)
#'
#' @family coding-related functions provided by ub
#' @export
hide = function(x, ...) {
  invisible(x)
}

#' @title
#' Attempt to call
#'
#' @description
#' Make an attempt to call a supplied call and evaluate whether an 
#' error occurred.
#'
#' @examples
#' attemp_call(sin(0))     # call runs ok, returns TRUE
#' attemp_call(sin('a'))   # call fails, returns FALSE
#'
#' @export
attemp_call = function(expr) {
  is_error(tryCatch(expr, error=hide, warning=hide))
}

#put_trace = function(..., tracer, at) 
  
