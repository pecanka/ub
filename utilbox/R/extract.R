#' Get every n-th element of an object
#'
#' `nth` extracts the n-th element (single value) of an object 
#' `x` (vector, list, etc.). It relies on `\`[[\`` for the
#' extraction.
#'
#' `nthm` allows for multivariate input in `n`. It relies on 
#' `\`[\`` for the extraction.
#'
#' `nthr` and `nthmr are the analogue of `nth` and `nthm` which 
#' determine index the position in reversed order.
#'
#' @examples
#' nth(1:10, 2)       # second
#' nthm(1:10, 2:3)    # second and third
#' nthr(1:10, 2)      # second from the end
#' nthmr(1:10, 2:3)   # second and third from the end
#'
#' @name filter_by_position
#' @family sequence-related functions provided by utilbox
#' @export
nth = function(x, n) {
  x[[n]]
}

#' @rdname filter_by_position
#' @export
nthm = function(x, n) {
  x[n]
}

#' @rdname filter_by_position
#' @export
nthr = function(x, n) {
  x[[length(x)+1-n]]
}

#' @rdname filter_by_position
#' @export
nthmr = function(x, n) {
  if(n>0) x[length(x)+1-n] else x[-(length(x) + 1 - abs(n))]
}

#' Get every n-th element of an object
#'
#' Extracts every n-th element of an object (vector, list, etc.)
#' starting from 'start'.
#'
#' @examples
#' every_nth(1:10, 2)
#' every_nth(1:10, 2, 2)
#'
#' @family sequence-related functions provided by utilbox
#' @export
every_nth = function(x, n, start=1L) {
  x[seq.int(as.integer(start), length(x), as.integer(n))]
}

#' Get the last element
#'
#' Returns the last element in an vector or list.
#'
#' @family sequence-related functions provided by utilbox
#' @export
last_element = function(x) {
  x[length(x)]
}

#' Locate value
#'
#' `first_nonzero` finds the value of the last non-zero element in `x`.
#'
#' `nth_nonzero` finds the value of the n-th non-zero element in `x`.
#'
#' `last_nonzero` finds the value of the last non-zero element in `x`.
#'
#' `first_positive` finds the value of the first positive element in `x`.
#'
#' `last_positive` finds the value of the last positive element in `x`.
#'
#' `first_above` (`first_above_soft`) finds the value of the first element 
#' in `x` with value larger than (larger than or equal to) `y`.
#'
#' `last_above` (`last_above_soft`) finds the value of the last element 
#' in `x` with value larger than (larger than or equal to) `y`.
#'
#' `first_below` (`first_below_soft`) finds the value of the first element 
#' in `x` with value smaller than (smaller than or equal to) `y`.
#'
#' `last_below` (`last_below_soft`) finds the value of the last element 
#' in `x` with value smaller than (smaller than or equal to) `y`.
#'
#' @family numeric functions provided by utilbox
#' @name value_locale
#' @export
#' @export
first_nonzero = function(x) {
  x[w_first_nonzero(x)]
}

#' @rdname value_locale
#' @export
nth_nonzero = function(x, n) {
  x[w_nth_nonzero(x)]
}

#' @rdname value_locale
#' @export
last_nonzero = function(x) {
  x[w_last_nonzero(x)]
}

#' @rdname value_locale
#' @export
first_positive = function(x) {
  w[w_first_nonzero(x)]
}

#' @rdname value_locale
#' @export
last_positive = function(x) {
  x[w_last_positive(x)]
}

#' @rdname value_locale
#' @export
first_above = function(x, y) {
  x[w_first_above(x, y)]
}

#' @rdname value_locale
#' @export
first_above_soft = function(x, y) {
  x[w_first_above_soft(x, y)]
}

#' @rdname value_locale
#' @export
last_above = function(x, y, ...) {
  x[w_last_above(x, y, ...)]
}

#' @rdname value_locale
#' @export
last_above_soft = function(x, y) {
  x[w_last_above_soft(x, y)]
}

#' @rdname value_locale
#' @export
first_below = function(x, y) {
  x[w_first_below(x, y)]
}

#' @rdname value_locale
#' @export
first_below_soft = function(x, y) {
  x[w_first_below_soft(x, y)]
}

#' @rdname value_locale
#' @export
last_below = function(x, y) {
  x[w_last_below(x, y)]
}

#' @rdname value_locale
#' @export
last_below_soft = function(x, y) {
  x[w_last_below_soft(x, y)]
}

#' @export
first_nonempty = function(x) {
  x[w_first_nonempty(x)]
}

  
#' Locate position of a value
#'
#' `w_first_nonzero` finds the position of the last non-zero element in `x`.
#'
#' `w_nth_nonzero` finds the position of the n-th non-zero element in `x`.
#'
#' `w_last_nonzero` finds the position of the last non-zero element in `x`.
#'
#' `w_first_positive` finds the position of the first positive element in `x`.
#'
#' `w_last_positive` finds the position of the last positive element in `x`.
#'
#' `w_last_above` finds the position of the last element in `x` with value larger than `y`.
#'
#' `w_first_above` finds the position of the first element in `x` with value larger than `y`.
#'
#' `w_last_below` finds the position of the last element in `x` with value below `y`.
#'
#' `w_first_below` finds the position of the first element in `x` with value below `y`.
#'
#' `w_first_nonempty` finds the position of the first non-empty element in `x`.
#' It is designed as a method, which currenlty only operates on lists.
#'
#' @family numeric functions provided by utilbox
#' @name value_locale_position
#' @export
w_first_nonzero = function(x) {
  h1(which(x!=0))
}

#' @rdname value_locale
#' @export
w_nth_nonzero = function(x, n) {
  stopifnot(n>=1)
  if(n==1) {
    h1(which(x!=0))
  } else {
    h1(tail(which(x!=0),-n+1))
  }
}

#' @rdname value_locale
#' @export
w_last_nonzero = function(x) {
  length(x) - w_first_nonzero(rev(x)) + 1
}

#' @rdname value_locale
#' @export
w_first_positive = function(x) {
  w_first_nonzero(x>0)
}

#' @rdname value_locale
#' @export
w_last_positive = function(x) {
  length(x) - w_first_nonzero(rev(x)>0) + 1
}

#' @rdname value_locale
#' @export
w_first_above = function(x, y) {
  w_first_positive(x>y)
}

#' @rdname value_locale
#' @export
w_first_above_soft = function(x, y) {
  w_first_positive(x>=y)
}

#' @rdname value_locale
#' @export
w_last_above = function(x, y) {
  w_last_positive(x>y)
}

#' @rdname value_locale
#' @export
w_last_above_soft = function(x, y) {
  w_last_positive(x>=y)
}

#' @rdname value_locale
#' @export
w_first_below = function(x, y) {
  w_first_positive(x<y)
}

#' @rdname value_locale
#' @export
w_first_below_soft = function(x, y) {
  w_first_positive(x<=y)
}

#' @rdname value_locale
#' @export
w_last_below = function(x, y) {
  w_last_positive(x<y)
}

#' @rdname value_locale
#' @export
w_last_below_soft = function(x, y) {
  w_last_positive(x<=y)
}

#' @export
w_first_nonempty = function(...) {
  UseMethod("w_first_nonempty")
}

#' @export
w_first_nonempty.list = function(x) {
  h1(which(sapply(x, is_nonempty)))
}

#' Drop elements based on their positiveness/negativeness
#'
#' `positive` drops all non-positive elements in `x`.
#' 
#' `negative` drops all non-negative elements in `x`.
#'
#' `nonpositive` drops all negative elements in `x`.
#' 
#' `nonnegative` drops all positive elements in `x`.
#'
#' @name drop_positive_negative
#' @family check-performing functions provided by utilbox
#' @export
positive = function(x) {
  x[!is.na(x) & x>0]
}

#' @rdname drop_positive_negative
#' @export
negative = function(x) {
  x[!is.na(x) & x<0]
}

#' @rdname drop_positive_negative
#' @export
nonnegative = function(x) {
  x[!is.na(x) & x>=0]
}

#' @rdname drop_positive_negative
#' @export
nonpositive = function(x) {
  x[!is.na(x) & x<=0]
}

#' Filter an object based
#'
#' `filter_by_value` filters `x` based on whether its
#' values match the pattern in `pattern`.
#'
#' `filter_by_name` filters `x` based on whether its
#' names match the pattern in `pattern`.
#'
#' `filter_by_bool` filters `x` according to logical
#' values in `keep`. It is just an alias for `x[keep]`.
#'
#' `filter_by_call` filters an object (`x`) based on whether the
#' results of a function (call given in `call`) match the pattern 
#' in `pattern`. The `call` argument can be
#' a standard R function (class `function`) or a symbolic
#' `tidyverse`-style representation of a function, which uses the 
#' formula sign \code{~} to represent a function with a single argument 
#' \code{.x} and which is evaluated with \code{x} supplied to it as 
#' the argument \code{.x}.
#'
#' @examples
#' v = c(first='hello',second='amsterdam')
#'
#' filter_by_value(v, 'ams')
#' 
#' filter_by_name(v, 'ams')
#'
#' filter_by_bool(v, c(TRUE,FALSE))
#' 
#' filter_by_call(v, ~nchar(.x)>6)
#'
#' filter_by_call(1:10, ~.x<5)
#'
#' @name filter_by
#' @family sequence-related functions provided by utilbox
#' @export
filter_by_value = function(...) {
  UseMethod("filter_by_value")
}

#' @rdname filter_by
#' @export
filter_by_value.default = function(x, pattern, fixed=FALSE, exclude=FALSE) {
  x[`%m%`(pattern, x, fixed=fixed, exclude=exclude)]
}
  
#' @rdname filter_by
#' @export
filter_by_name = function(x, pattern, fixed=FALSE, exclude=FALSE) {
  x[`%m%`(pattern, names(x), fixed=fixed, exclude=exclude)]
}
  
#' @rdname filter_by
#' @export
filter_by_bool = function(x, keep, exclude=FALSE) {
  if(exclude) x[!keep] else x[keep]
}
  
#' @export
filter_by_pattern = function(x, pattern, fixed=FALSE, exclude=FALSE) {
  x[`%m%`(pattern, x, exclude=exclude)]
}
  
#' @rdname filter_by
#' @export
filter_by_call = function(...) {
  
  lazy_dots_to_args_and_call(...)
  nargs = length(args)
  
  if(nargs==0) return(NULL)
  
  #names(args) = symbolic_call_names(length(args))
  fun_call = process_symbolic_call(call, nargs)
  fltr = do.call(fun_call, args)
  args[[1]][fltr]
  
}

lazy_dots_to_args_and_call = function(..., envir=parent.frame()) {

  args = list(...)
  
  if(length(args)==0) error('Supply a call.')
  
  assign('call', nthr(args,1), envir=envir)
  assign('args', nthmr(args, -1), envir=envir)
  
}
