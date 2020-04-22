#' @title
#' Between check
#'
#' @description
#'
#' `is_between()` is a shortcut for `x>=a & x<=b` (for 
#' `sharp_left=TRUE` and `sharp_right=TRUE`). Other combinations of 
#' `TRUE/FALSE for `sharp_left,sharp_right` lead to non-sharp 
#' comparisons.
#'
#' `or_between()` performs is a between check with many intervals. 
#' Checks whether `x` is in at least one of the supplied intervals.
#'
#' @examples
#' between(1:2, 1, 2))                # both
#' between(1:2, 1, 2, TRUE))          # second only
#' between(1:2, 1, 2, FALSE, TRUE)    # first only
#' between(1:2, 1, 2, TRUE, TRUE)     # none
#'
#' or_between(runif(3), list(c(0,0.1), c(0.9,1)))
#'
#' @family sequence-related functions provided by utilbox
#' @export
is_between = function(x, a, b, sharp_left=FALSE, sharp_right=FALSE) {
  
  f1 = if(sharp_left) `>` else `>=`
  f2 = if(sharp_right) `<` else `<=`
  
  f1(x,a) & f2(x,b)
  
}

#' @rdname is_between
#' @export
or_between <- function(x, intervals, sharp_left=FALSE, sharp_right=FALSE) {
  
  stopifnot(is.list(intervals))
  
  is_in = sapply(intervals, function(int) is_between(x, int[[1]], int[[2]]))
  
  if(is.vector(is_in)) any(is_in) else apply(is_in, 1, any)
  
}

#' @title
#' Starts and ends of run
#'
#' @description
#'
#' Indicators of the beginnings (`is_start_of_run`) and ends 
#' (`is_end_of_run`) of "runs", where "runs" are subsequences with 
#' unchanging values.
#'
#' @examples
#' x=1:10
#' is_end_of_run(x)           # all TRUE
#' is_start_of_run(x)         # all TRUE
#'
#' x = c(1,2,2,2,3,3,4)
#' is_end_of_run(x)           # 1 4 6 are TRUE
#' is_start_of_run(x)         # 1 2 5 7 are TRUE
#'
#' @name is_run
#'
#' @family sequence-related functions provided by utilbox
#' @export
is_start_of_run = function(x, is_first_start=TRUE, tol) {

  if(!is.numeric(x)) {
    x = as.numeric(as_factor(x))
  }
  
  if(missing(tol)) tol = .Machine$double.eps
  
  c(is_first_start, abs(diff(x))>tol)
  
}

#' @rdname is_run
#' @export
is_end_of_run = function(x, is_last_end=TRUE, tol) {

  if(!is.numeric(x)) {
    x = as.numeric(as_factor(x))
  }
  
  if(missing(tol)) tol = .Machine$double.eps
  
  c(abs(diff(x))>tol, is_last_end)
  
}

