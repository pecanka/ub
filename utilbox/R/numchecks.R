#' Non-NA value check
#' 
#' Check whether a row of \code{x} contains at least one non-NA value. Works for vectors
#' and 2-arrays.
#'
#' @examples
#' x = rbind(c(1,NA),c(NA,NA)); anyNonNA(x)
#'
#' @family check-performing functions provided by utilbox
#' @export
anyNonNA = function(x, rowwise=TRUE) {
  if(rowwise) {
    stopifnot(is.null(dim(x)) || length(dim(x))<=2)
    if(NCOL(x)<=1) !is.na(t(t(x))[,1]) else !apply(is.na(x),1,all)
  } else {
    Recall(t(x), rowwise=TRUE)
  }
}

#' Check for \code{NA/NaN} values
#'
#' Checks if a reduction of elements of \code{x} is \code{NA}. The default 
#' reduction function is \code{all}, which returns \code{TRUE} whenever all elements 
#' of its argument are \code{NA} or `x` is 0-length. A useful alternative reduction function is 
#' \code{any} supplied via the argument \code{freduce}.
#'
#' @family check-performing functions provided by utilbox
#' @export
is_na = function(x, freduce=all) {
  freduce(is.na(x))
}

#' Check if a sequence in 'x' has only unique elements
#'
#' @family check-performing functions provided by utilbox
#' @export
is_unique = function(x) {
  !anyDuplicated(x)
}

#' Check if all elements in 'x' have the same value
#'
#' @family check-performing functions provided by utilbox
#' @export
is_all_same = function(x) {
  length(unique(x))<=1
}

#' Check if a sequence in 'x' is decreasing
#'
#' @family check-performing functions provided by utilbox
#' @export
is_decreasing = function(x, strictly=FALSE) {
  !is.unsorted(rev(x), strictly=strictly) 
}

#' Check if a sequence in 'x' is increasing
#'
#' @family check-performing functions provided by utilbox
#' @export
is_increasing = function(x, strictly=FALSE) {
  !is.unsorted(x, strictly=strictly)
}

#' Check if elements in 'x' are effectively integers (i.e. close enough to one)
#'
#' @family check-performing functions provided by utilbox
#' @export
is_integer = function(x, tol=.Machine$double.eps) {
  abs(x - round(x)) < 2*tol
  #x==floor(x+2*tol)
}

#' Check if elements of 'x' are odd
#'
#' @family check-performing functions provided by utilbox
#' @export
is_odd = function(x, tol=.Machine$double.eps) {
  abs((x+1) %% 2) < tol
}

#' Check if elements of 'x' are even
#'
#' @family check-performing functions provided by utilbox
#' @export
is_even = function(x, tol=.Machine$double.eps) {
  abs(x %% 2) < tol
}

#' Check if a string contains a number
#'
#' Checks if the elements of a character vector contain numerical values. 
#' The check is done by matching regular patterns against the supplied
#' vector. The argument \code{freduce} expects a function-type object, 
#' which is applied to the vector. Specifying of the decimal point and
#' thousand separator is possible, although no checks on the sensibility
#' of their positions within the character strings are performed.
#'
#' @examples
#' is_number('343')
#' is_number(c('343','x2'))
#' is_number('3,443.0')
#' is_number('343')
#'
#' @family check-performing functions provided by utilbox
#' @export
is_number = function(x, freduce=all, dec='.', sep_thousand="") {

  pattern_only_digits = "^[+-]?[0-9]+$"
  pattern_real = paste0("^[+-]?[0-9]*[",dec,"][0-9]*$")
  pattern_some_digits = "[0-9]"
  
  fits_num = if(is.numeric(x)) {
    rep(TRUE, length(x)) 
  } else if(is.character(x)) {
    if(nchar(sep_thousand)>0) x = gsub(paste0('[',sep_thousand,']',''),'',x)
    c1 = regexpr(pattern_only_digits, x)>0 
    c2 = regexpr(pattern_real, x)>0 | regexpr(pattern_some_digits, x)>0
    c1 | c2
  } else {
    rep(FALSE, length(x))
  }
  
  freduce(fits_num)
  
}

#' Check if 'x' is in scientific notation (e.g. 1.234E5)
#'
#' @family check-performing functions provided by utilbox
#' @export
is_scientific = function(x, freduce=all) {
  freduce(sapply(x, is.numeric) & regexpr("^[0-9.]+[Ee][-0-9]+$", x)>0)
}
