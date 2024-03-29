#' @title
#' Various means
#'
#' @description
#'
#' Various mean functions. Arithmetic, geometric, weighted geometric, 
#' harmonic and logistic means are implemented. Compares to 
#' [`base::mean`] these functions have remove `NA`s by default and 
#' return zero-length responses on zero-length input or input with no 
#' non-`NA` values.
#'
#' `amean()` gives the arithmetic mean of `x`.
#'
#' `gmean()` and `gmeanw` calculate the geometric mean of `x`, while 
#' the latter permits weighing. `gmean2()` is a version that processes
#' inputs with negative values without warnings as well as drops zeros
#' (unless `zero_propagate=TRUE`). `gmean2()` inspired by this:
#' https://stackoverflow.com/a/25555105
#'
#' `hmean()` calculates the harmonic mean (single value) of a single 
#' vector created as a combination of all its input arguments.
#'
#' `lmean()` calculates the (weighted) logistic mean.
#'
#' `mean_na()` is an alias for [`base::mean`] with `na.rm=TRUE`.
#'
#' @examples
#' x = c(1,NA); mean(x); mean2(x)
#'
#' @name means
#' @export
mean_na = function(..., na.rm=TRUE) {
  base::mean(..., na.rm=na.rm)
}

#' @rdname means
#' @export
amean = function(x, na.rm=TRUE) {
  if(is_na(x)) {
    NULL 
  } else {
    base::mean(x, na.rm=na.rm)
  }
}

#' @rdname means
#' @export
hmean = function(..., na.rm=TRUE) {
  args = c(...)
  if(is_empty(args)) {
    args
  } else {
    amean(args^{-1}, na.rm=na.rm)^{-1}
  } 
}

#' @rdname means
#' @export
gmean = function(x, na.rm=TRUE) {

  if(is_na(x))
    return(NULL)

  if(na.rm) x = x[!is.na(x)]
  exp(sum(log(x)/length(x)))

}

#' @rdname means
#' @export
gmean2 = function(x, na.rm=TRUE, zero.propagate = FALSE) {

  if(any(x < 0, na.rm = TRUE))
    return(NaN)
  
  if(zero.propagate){

    if(any(x == 0, na.rm = TRUE))
      return(0)

    exp(mean(log(x), na.rm = na.rm))
    
  } else {
    
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    
  }
  
}

#' @rdname means
#' @export
gmeanw = function(x, w, na.rm=TRUE, ...) 
  if(is_na(x)) {
    NULL 
  } else {
    exp(stats::weighted.mean(log(x), w, na.rm=na.rm, ...))
  }

#' @rdname means
#' @export
lmean = function(x, w, na.rm=TRUE, ...) {
  if(is_na(x)) {
    NULL 
  } else {
    plogis(stats::weighted.mean(qlogis(x), w, na.rm=na.rm, ...))
  }
}
