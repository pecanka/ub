#' Number of digits
#'
#' Determines the number of digits for an integer or 
#' a real (for which the output reflects only the 
#' integer part).
#'
#' @export
ndigits = function(x) {
  floor(log10(pmax(1,abs(x)))) + 1
}

#' Check for integers
#'
#' Checks if values in `x` are integers
#'
#' @family check-performing functions provided by utilbox
#' @export
is_round = function(x, eps=.Machine$double.eps) {
  if(is.na(x) || is.infinite(x)) {
    FALSE 
  } else {
    abs(round(x)-x) <= eps
  }
}

#' Rounding
#' 
#' Returns the integer part of the supplied vector.
#'
#' @family numeric functions provided by utilbox
#' @export
int_part = function(x) {
  ifelse(x>=0, floor(x), ceiling(x))
}

#' Rounding
#' 
#' Returns the integer part of the supplied vector.
#'
#' @family numeric functions provided by utilbox
#' @export
frac_part = function(x) {
  sign(x) * (abs(x) - floor(abs(x)))
}

#' Rounding
#'
#' Rounds the elements of vector \code{x} to \code{nd} digits while
#' making sure \code{x} does not become non-unique (in case it was 
#' unique).
#'
#' @family numeric functions provided by utilbox
#' @export
round2 = function(x, nd=0) {
  x_is_unique = anyDuplicated(x)==0
  for(d in seq(nd,nd+15)) {
    y = as.numeric(sprintf(paste0('%.',d,'f'), x))
    if(!x_is_unique || anyDuplicated(y)==0) break
  }
  return(y)
}

#' Rounding
#'
#' Rounds a number (can be a vector) to the nearest multiple of \code{b}.
#'
#' @family numeric functions provided by utilbox
#' @export
round_nearest = function(x, b=1) {
  round(a/b)*b
}

#' Rounding
#'
#' Rounds a number (can be a vector) to the nearest power of \code{base}.
#'
#' @family numeric functions provided by utilbox
#' @export
round_nearest_power = function(x, base=10) {
  base^round(log(x,base))
}

#' Rounding
#'
#' Rounds \code{x} to \code{ndigit} digits after the decimal point without
#' losing precision to the left of the decimal point.
#'
#' @family numeric functions provided by utilbox
#' @export
rsignif = function(x, ndigit=0) {
  ix = integer_part(x)
  rx = ix + signif(x-ix, digits=ndigit)
  if(abs(x)<=.Machine$double.xmin) return(x)
}

#' Round numbers
#'
#' Round numbers by leveraging \code{base::signif} except with a wrapper which
#' it allows a non-numeric input without throwing an error.
#'
#' @family numeric functions provided by utilbox
#' @export
signif2 = function(x, ...) {
  if(is.numeric(x)) signif(x, ...) else x
}
