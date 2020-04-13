#' Invert non-zero elements in an object
#'
#' @family numeric functions provided by utilbox
#' @export
Invert1 = function(x) {
  y = rep(0, length(x))
  y[nz <- x!=0] = 1. / x[nz]
  y
}

#' Find pseudoinverse, square root, and sqrt-inverse matrices
#'
#' @family numeric functions provided by utilbox
#' @export
Invert = function(M, inv=FALSE, sq=FALSE, sqinv=FALSE, eps=1e-12, drop=FALSE) {
  INV = SQ = SQINV = NULL
  
  if(length(M)==1) {
  
    if(M==0) {
      if(sq) SQ = M
      if(inv) INV = M
      if(sqinv) SQINV = M
    } else {
      if(sq) SQ = sqrt(M)
      if(inv) INV = 1/M
      if(sqinv) SQINV = 1/sqrt(M)
    }
  
  } else {
    
    e = eigen(M)
    V = e$vectors
    d = e$values
    d[abs(d)<eps] = 0
    if(sq) SQ = V %*% diag(sqrt(d)) %*% t(V)
    d = 1/d
    d[is.infinite(d)] = 0
    if(inv) INV = V %*% diag(d) %*% t(V)
    if(sqinv) SQINV = V %*% diag(sqrt(d)) %*% t(V)
  
  }
  
  L = list_clean(list(INV=INV, SQ=SQ, SQINV=SQINV))
  if(length(L)==1 && drop) L = L[[1]]
  return(L)
  
}

