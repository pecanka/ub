#' @title
#' Size checks
#'
#' @description
#'
#' `isempty()` checks whether the supplied object has zero elements.
#' It is similar to `rlang::is_empty()`, except that it assesses
#' zero-row and/or zero-column data frames as empty by looking at 
#' the product of the its dimensions instead of its length. 
#'
#' `has_empty()` is an element-wise check for emptiness. It applies
#' `is_empty()` to the elements of its argument via `base::sapply`. Useful
#' for checking emptiness within a `list`-class object (e.g., list, data frame).
#'
#' `is_not_empty()` is a negation of `is_empty()`.
#'
#' `is_scalar()` checks whether the supplied is a scalar, i.e. 
#' unidimensional of size 1 (e.g., length-one vector, length-one list, etc.).
#'
#' `is_dimone()` checks for unidimensionality (e.g. vector, list, etc.).
#'
#' `is_dimtwo()` checks for bidimensionality (e.g. matrix, data frame).
#'
#' `is_dimtwoplus()` checks whether the dimensionality is at least 2 (e.g. 
#' matrix, data frame, 2D array, 3D array).
#'
#' @name sizechecks
#' @family sequence-related functions provided by utilbox
#' @export
is_empty = function(...) {
  UseMethod("is_empty")
}

#' @rdname sizechecks
#' @export
is_empty.default = function(x) {
  length(x)==0
}

#' @rdname sizechecks
#' @export
is_empty.data.frame = function(x) {
  prod(dim(x))==0
}

#' @rdname sizechecks
#' @export
has_empty = function(x) {
  any(sapply(x, is_empty))
}

#' @rdname sizechecks
#' @export
is_not_empty = function(...) {
  !is_empty(...)
}

#' @rdname sizechecks
#' @export
is_scalar = function(x) {
  is.null(dim(x)) && length(x)==1
}

#' @rdname sizechecks
#' @export
is_dimone = function(x) {
  is.null(dim(x))
}

#' @rdname sizechecks
#' @export
is_dimtwo = function(x) {
  !is.null(dim(x)) && length(dim(x)==2)
}

#' @rdname sizechecks
#' @export
is_dimtwoplus = function(x) {
  !is.null(dim(x)) && length(dim(x)>=2)
}

