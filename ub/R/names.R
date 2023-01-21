#' @title
#' Names functions
#'
#' @description
#'
#' `coalesce_names()` fills in the missing (zero-length) names in `x` 
#' with the corresponding names in `y`.
#'
#' `unempty_names()` works similarly except it takes directly 
#' (through `names`) the vector names from which to source the 
#' replacements for the missing names in `x`.
#'
#' `is_names_empty()` checks for empty names. For an object without 
#' `names` (i.e. when `names(x)` returns `NULL`) it returns a vector of 
#' `FALSE` of the same length as `x`.
#'
#' `has_any_names()` is an alias for `!is.null(names(x))`.
#'
#' `has_all_names()` checks that `x` has any names and that all those 
#' names are non-empty strings.
#'
#' @examples
#' x = c(a=1, b=2, 3, d=4)
#' y = c(NA, NA, c=NA, NA)
#' coalesce_names(x, y)
#'
#' unempty_names(x, names(y))
#'
#' is_names_empty(x)
#' is_names_empty(y)
#' is_names_empty(1:5)
#'
#' has_any_names(1:2)
#' has_any_names(x)
#'
#' @family names-related functions provided by ub
#' @export
coalesce_names = function(x, y) {
  if(is_empty(x)) {
    x
  } else {
    `names<-`(x, ifelse(is_names_empty(x), names(y), names(x)))
  }
}

#' @export
unempty_names = function(x, names) {
  if(is_empty(x)) {
    x
  } else {
    `names<-`(x, ifelse(is_names_empty(x), names, names(x)))
  }
}

#' @export
is_names_empty = function(x) {
  if(is.null(names(x))) rep(TRUE, length(x)) else str_is_empty(names(x))
}

#' @export
has_any_names = function(x) {
  !is.null(names(x))
}

#' @export
has_all_names = function(x) {
  has_any_names(x) || all(!is_names_empty(x))
}

