#' Zero-length check
#'
#' 1is_empty` checks whether the supplied object has zero
#' no elements.
#'
#' `is_nonempty` is a negation of `is_empty.
#'
#' The function `is_empty` is similar to [rlang::is_empty], 
#' except applicable also to data frames, for which it 
#' derives length from the production of the dimension 
#' (obtained via `dim`).
#'
#' @family sequence-related functions provided by utilbox
#' @export
is_empty = function(...) {
  UseMethod("is_empty")
}

#' @export
is_empty.default = function(x) {
  length(x)==0
}

#' @export
is_empty.data.frame = function(x) {
  prod(dim(x))==0
}

#' @export
is_nonempty = Negate(is_empty)

#' Dimension check
#'
#' `is_scalar` checks whether the supplied is a scalar (vector, list, etc.).
#' 
#' `is_dimtwo` checks for two-dimensionality (e.g. matrix, data frame).
#'
#' `is_dimtwoplus` checks for dimensionality 2 or more (e.g. matrix, data frame, 2D array, 3D array).
#'
#' @name dim_check
#' @family sequence-related functions provided by utilbox
#' @export
is_scalar = function(x) {
  is.null(dim(x))
}

#' @rdname dim_check
#' @family sequence-related functions provided by utilbox
#' @export
is_dimtwo = function(x) {
  !is.null(dim(x)) && length(dim(x)==2)
}

#' @rdname dim_check
#' @family sequence-related functions provided by utilbox
#' @export
is_dimtwoplus = function(x) {
  !is.null(dim(x)) && length(dim(x)>=2)
}

