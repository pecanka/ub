#' Upper-triangular
#'
#' Returns the elements on the upper-triangular as a vector
#'
#' @family numeric functions provided by utilbox
#' @export
lower_tri = function(x, diag=TRUE) 
  x[lower.tri(x, diag=diag)]

#' Lower-triangular
#'
#' Returns the elements on the lower-triangular as a vector
#'
#' @family numeric functions provided by utilbox
#' @export
upper_tri = function(x, diag=TRUE) {
  x[upper.tri(x, diag=diag)]
}

