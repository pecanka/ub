#' @title
#' Upper- and lower-triangular
#'
#' @description
#'
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
#'
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

#' Diagonal matrix
#'
#' `diag_matrix()` creates a diagonal matrix with `x` on the diagonal and 
#' `offdiag` off of the diagonal in a row-wise fashion. If `offdiag` 
#' has only half the required number of elements, it gets symmetrically 
#' placed both above and below the diagonal, thereby leading to a 
#' symmetric matrix.
#'
#' @examples
#' # diagonal matrix with 1:4 on the diagonal
#' diag_matrix(1:4)
#' # symmetric matrix with 1:4 on the diagonal and 3 everywhere else
#' diag_matrix(1:4, offdiag = 3)
#'
#' @export
diag_matrix = function(x, offdiag=0) {

  n = length(x)
  offd = unlist(offdiag)
  
  if(length(offd) == n*(n-1)/2) {
    d = diag(x/2)
    d[lower.tri(d)] = offd
    d = d + t(d)
  } else {
    d = diag(x)
    d[lower.tri(d) | upper.tri(d)] = offd
  }
  return(d)
}

#' Create multiple copies of a matrix and bind them together
#'
#' `rep_diag()` makes `times` copies of a matrix and produces a block 
#' diagonal matrix with the copies.
#'
#' `rep_matrix()` returns a list of `times` copies of a matrix.
#'
#' `rep_matrix_rbind()` and `rep_matrix_cbind` each return a matrix 
#' made up of `times` copies of a matrix bound together by rows and
#' columns respectively.
#'
#' @export
rep_diag = function(x, times=1, sparse=FALSE) {
  
  m = Matrix::bdiag(rep_matrix(x, times, TRUE))
  
  if(!sparse) {
    m = as.matrix(m)
  }
    
  m
}

#' @rdname rep_diag
#' @export
rep_matrix = function(x, times=1, sparse=FALSE) {
  
  if(sparse) {
    x = Matrix::Matrix(x, sparse=TRUE)
  }
  
  lapply(rep(1,times), function(i) x)
  
}

#' @rdname rep_diag
#' @export
rep_matrix_rbind = function(x, times=1, sparse=FALSE) {
  do.call("rbind", rep_matrix(x, times=times, sparse=sparse))
}

#' @rdname rep_diag
#' @export
rep_matrix_cbind = function(x, times=1, sparse=FALSE) {
  do.call("cbind", rep_matrix(x, times=times, sparse=sparse))
}
