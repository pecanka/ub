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
#' the latter permits weighing.
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
  if(is_na(x)) {
    NULL 
  } else {
    if(na.rm) x = x[!is.na(x)]
    #prod(x)^(1/length(x))
    exp(sum(log(x)/length(x)))
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
