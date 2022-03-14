#' @title
#' Number of digits
#'
#' @description
#'
#' `ndigits()` determines the number of digits for an integer or a 
#' real (for which the output reflects only the integer part).
#'
#' `ndecimals()` determines how many non-zero decimal places are
#' needed to print a number.
#'
#' `nsignif()` calculates how many significant digits the supplied 
#' numbers are rounded to.
#'
#' `is_round()` checks if values in `x` are round (i.e. equal to 
#' integers).
#'
#' `int_part()` returns the integer parts of the values in in the 
#' supplied vector.
#'
#' `frac_part()` returns the fractional parts of the values in the 
#' supplied vector.
#'
#' `round2()` rounds the elements of vector `x` to `nd` digits while 
#' making sure `x` does not become non-unique (in case it was unique).
#'
#' `round_nearest()` rounds a number (can be a vector) to the nearest 
#' multiple of `b`.
#'
#' `round_nearest_power()` rounds a number (can be a vector) to the 
#' nearest power of `base`.
#'
#' `rsignif()` rounds `x` to `ndigit` digits after the decimal point 
#' without losing precision to the left of the decimal point.
#'
#' `signif2()` round numbers by leveraging \code{base::signif} except 
#' with a wrapper which it allows a non-numeric input without throwing 
#' an error.
#'
#' `print_full()` prints with options()$ndigits maximized.
#'
#' `format_full()` formats numbers in maximum precision.
#'
#' @name rounding
#' @family check-performing functions provided by utilbox
#' @export
ndigits = function(x) {
  floor(log10(pmax(1,abs(x)))) + 1
}

#' @name rounding
#' @export
ndecimals = function(x) {
  pmax(0, nchar(format(abs(x-as.integer(x))))-2)
}

#' @name rounding
#' @export
nsignif = function(x) {
  nchar(sub('0+$', '', sub('[.]', '', sub('^0+','', sprintf('%.14f', x)))))
}

#' @rdname rounding
#' @export
is_round = function(x, eps=.Machine$double.eps) {
  if(is.na(x) || is.infinite(x)) {
    FALSE 
  } else {
    abs(round(x)-x) <= eps
  }
}

#' @rdname rounding
#' @export
int_part = function(x) {
  ifelse(x>=0, floor(x), ceiling(x))
}

#' @rdname rounding
#' @export
frac_part = function(x) {
  sign(x) * (abs(x) - floor(abs(x)))
}

#' @rdname rounding
#' @export
round_nearest = function(x, b=1) {
  round(a/b)*b
}

#' @rdname rounding
#' @export
round_nearest_power = function(x, base=10) {
  base^round(log(x,base))
}

#' @rdname rounding
#' @export
rsignif = function (x, ndig = 0) {
  ifelse(is.na(x) | ndigits(x) >= ndig | ndig == 0,
         round(x),
         ifelse(x == 0, x, int_part(x) + signif(frac_part(x), digits = ndig)))
}

#' @rdname rounding
#' @export
round2 = function(x, nd=0) {
  x_is_unique = anyDuplicated(x)==0
  for(d in seq(nd,nd+15)) {
    y = as.numeric(sprintf(paste0('%.',d,'f'), x))
    if(!x_is_unique || anyDuplicated(y)==0) break
  }
  y
}

#' @rdname rounding
#' @export
signif2 = function(x, ...) {
  if(is.numeric(x)) signif(x, ...) else x
}

#' @rdname rounding
#' @export
print_full = function(x, digits=20) {
  ndig = getOption('digits')
  on.exit(options(digits=ndig))
  options(digits=digits)
  print(x)
  return(invisible(x))
}

#' @rdname rounding
#' @export
format_full = function(x, digits=20) {
  sprintf(paste0('%.',digits,'f'), x)
}
