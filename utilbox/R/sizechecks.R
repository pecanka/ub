#' @title
#' Size checks
#'
#' @description
#'
#' `is_empty()` checks whether the supplied object has zero no 
#' elements. It is similar to `rlang::is_empty()`, except applicable 
#' also to data frames, for which it derives length from the production 
#' of the dimension (obtained via `dim`). 
#'
#' `is_any_empty()` is a element-wise check for emptiness. It applies
#' `is_empty()` to the elements of its argument via `base::sapply`. Useful
#' for checking emptiness within a `list`-class object.
#'
#' `is_nonempty()` is a negation of `is_empty()`.
#'
#' `is_scalar()` checks whether the supplied is a scalar (vector, list, 
#' etc.).
#'
#' `is_dimtwo()` checks for two-dimensionality (e.g. matrix, data 
#' frame).
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
is_any_empty = function(x) {
  any(sapply(x, is_empty))
}

#' @rdname sizechecks
#' @export
is_nonempty = function(...) {
  !is_empty(...)
}

#' @rdname sizechecks
#' @export
is_scalar = function(x) {
  is.null(dim(x)) && length(x)==1
}

#' @rdname sizechecks
#' @export
is_vector = function(x) {
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

