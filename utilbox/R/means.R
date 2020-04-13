#' Various means
#'
#' `amean` gives the arithmetic mean of `x`. It has the same 
#' functionality as \code{base::mean} except with 
#' different output when input has zero length.
#'
#' `amean2` is an alias for [base::mean] with a different
#' default for `na.rm` (unlike `base::mean`, it removes them by default).
#'
#' `gmean` and `gmeanw` calculate the geometric mean of `x`, while the latter
#' permits weighing.
#'
#' `hmean` calculates the harmonic mean (single value) of a single vector created as a
#' combination of all its input arguments.
#'
#' `lmean` calculates the (weighted) logistic mean.
#'
#' @examples
#' x = c(1,NA); mean(x); mean2(x)
#'
#' @name means
#' @export
amean = function(x, na.rm=FALSE) {
  if(is_na(x)) NULL else base::mean(x, na.rm=na.rm)
}

#' @rdname means
#' @export
amean2 = function(..., na.rm=TRUE) {
  base::mean(..., na.rm=na.rm)
}

#' @rdname means
#' @export
hmean = function(...) {
  if(length(c(...))) 1/mean(1/c(...)) else c(...)
}

#' @rdname means
#' @export
gmean = function(x, na.rm=FALSE) {
  if(na.rm) x = x[!is.na(x)]
  #prod(x)^(1/length(x))
  exp(sum(log(x)/length(x)))
}

#' @rdname means
#' @export
gmeanw = function(x, w, ...) 
  exp(weighted.mean(log(x), w, ...))

#' @rdname means
#' @export
lmean = function(x, w, ...) {
  plogis(weighted.mean(qlogis(x), w, ...))
}
