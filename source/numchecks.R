#' @title
#' Check for non-NA values
#'
#' @description
#'
#' `anyNonNA()` checks whether a row of `x` contains at least one 
#' non-NA value. Works for vectors and 2-arrays.
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

#' @title
#' Check
#'
#' @description
#'
#' `is_na(x)` checks if a reduction of elements of `x` is `NA`. The 
#' default reduction function is `all`, which returns `TRUE` whenever 
#' all elements of its argument are `NA` or `x` is 0-length. A useful 
#' alternative reduction function is `any` supplied via the argument 
#' `freduce`.
#'
#' `n_na(x)` counts the number of NA elements in `x`.
#'
#' `is_unique(x)` checks whether a sequence in 'x' has only unique 
#' elements.
#'
#' `is_all_same(x)` checks if all elements in 'x' have the same value.
#'
#' `is_decreasing(x)` checks if a sequence in 'x' is decreasing.
#'
#' `is_increasing(x)` checks if a sequence in 'x' is increasing.
#'
#' `equals_integer(x)` checks if elements in 'x' are effectively integers 
#' (i.e. close enough to one).
#'
#' `is_odd(x)` checks if elements of 'x' are odd
#'
#' `is_even(x)` checks if elements of 'x' are even
#'
#' @name numchecks
#' @family check-performing functions provided by utilbox
#' @export
is_na = function(x, freduce=all) {
  freduce(is.na(x))
}

#' @name numchecks
#' @export
n_na = function(x) {
  sum(is.na(x))
}

#' @rdname numchecks
#' @export
is_unique = function(x) {
  !anyDuplicated(x)
}

#' @rdname numchecks
#' @export
is_all_same = function(x) {
  length(unique(x))<=1
}

#' @rdname numchecks
#' @export
is_decreasing = function(x, strictly=FALSE) {
  !is.unsorted(rev(x), strictly=strictly) 
}

#' @rdname numchecks
#' @export
is_increasing = function(x, strictly=FALSE) {
  !is.unsorted(x, strictly=strictly)
}

#' @rdname numchecks
#' @export
equals_integer = function(x, tol=.Machine$double.eps) {
  abs(x - round(x)) < 2*tol
  #x==floor(x+2*tol)
}

#' @rdname numchecks
#' @export
is_odd = function(x, tol=.Machine$double.eps) {
  abs((x+1) %% 2) < tol
}

#' @rdname numchecks
#' @export
is_even = function(x, tol=.Machine$double.eps) {
  abs(x %% 2) < tol
}

#' @title
#' Check if two objects have equal values (simultaneous NAs are considered equal)
#'
#' @description
#'
#' `is_same_value()` checks if the elements of two equally sizes objects are either 
#' simultaneously `NA` or have equal non-`NA` values.
#'
#' @examples
#' is_same_value(1:5, 1:5)               # TRUE
#' is_same_value(c(NA,2:5), c(NA,2:5)    # also TRUE
#' is_same_value(1:5, c(NA,2:5)          # FALSE
#' is_same_value(0:4, 1:5)               # FALSE
#'
#' @family check-performing functions provided by utilbox
#' @export
is_same_value = function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & equals(x,y))
}

#' @title
#' Check if a string contains a number
#'
#' @description
#'
#' `is_number()` checks if the elements of a character vector contain 
#' numerical values. The check is done by matching regular patterns 
#' against the supplied vector. The argument `freduce` expects a 
#' function-type object, which is applied to the vector. Specifying of 
#' the decimal point and thousand separator is possible, although no 
#' checks on the sensibility of their positions within the character 
#' strings are performed.
#'
#' `is_scientific()` checks if 'x' is in scientific notation (e.g. 
#' 1.234E5).
#'
#' @examples
#' is_number('343')
#' is_number(c('343','x2'))
#' is_number('3,443.0')
#' is_number('343')
#'
#' is_scientific('100')
#' is_scientific('1e2')
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
    c1 = grepl(pattern_only_digits, x)
    c2 = grepl(pattern_real, x) | grepl(pattern_some_digits, x)
    c1 | c2
  } else {
    rep(FALSE, length(x))
  }
  
  freduce(fits_num)
  
}

#' @rdname is_number
#' @export
is_scientific = function(x, freduce=all) {

  freduce(sapply(x, is.numeric) & grepl("^[0-9.]+[Ee][-0-9]+$", x))
  
}
