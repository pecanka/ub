#' @title
#' Upper- and lower-triangular
#'
#' @description
#' `lower_tri()` and `upper_tri()` return the elements on the upper- 
#' and lower-triangular as a vectors.
#'
#' @family numeric functions provided by utilbox
#' @export
lower_tri = function(x, diag=TRUE) 
  x[lower.tri(x, diag=diag)]

#' @rdname lower_tri
#' @export
upper_tri = function(x, diag=TRUE) {
  x[upper.tri(x, diag=diag)]
}

#' @title
#' Matrix checks
#'
#' @description
#' `is_diag()` checks if 'x' is a diagonal matrix.
#'
#' `is_posdef()` checks if `M` is a positive-definite matrix.
#'
#' @name matchecks
#' @export
is_diag = function(x) {
  is.matrix(x) && nrow(x)==ncol(x) && 
    (length(x)==1 || max(abs(x[upper.tri(x)]))<.Machine$double.eps)
}

#' @rdname matchecks
#' @export
is_posdef = function(x) {

  stopifnot(is.numeric(x))
  if(length(x)==1 && x>0) return(TRUE)

  stopifnot(is.matrix(x), nrow(x)==ncol(x))
  
  check_namespace('mnormt')
  x = try(mnormt::rmnorm(1, mean=rep(0,nrow(x)), varcov=x), silent=TRUE)
  
  if(!is_error(x)) {
    TRUE 
  } else {
    structure(FALSE, msg=as.character(attr(x, "condition")))
  }
  
}

