#' @title
#' Weighted chi-square distribution
#'
#' @description
#'
#' `qchisqw()` calculates the quantile function of the weighted 
#' chisquare distribution, which is the distribution that arises when 
#' independent chisquare-distributed variables with `df0` degrees of 
#' freedom are summed on the logarithmic scale with weights in `w`.
#'
#' `pchisqw()` calculates the probability function of that same 
#' distribution. It gives the left tail (lower.tail==TRUE) or the right 
#' tail (lower.tail=FALSE) probability. The weights are specified via 
#' `weights`. When some (but not all!) weights are equal, it gives an 
#' error.
#'
#' @name weighted_chisquare
#' @export
qchisqw = function(p, weights, lower.tail=TRUE, epsw=1e-5, epsv=1e-12, df0=2) {

  if(any(weights<0)) 
    error("All values in 'weights' must be non-negative.")
  
  # Check the input
  np = length(p)
  if(np==1) {
    weights = matrix(weights, nrow=1)
  } else if(is.vector(weights)) {
    weights = matrix(rep(weights, e=np), nrow=np)
  }
  stopifnot(is.matrix(weights), nrow(weights)==np)
  
  # Get the chisquare quantile
  q0 = qchisq(p, df=df0*ncol(weights), lower.tail=FALSE) * apply(t(weights), 2, mean)
  
  # If weights are different, use pchisqw
  if(diff(range(weights)) < epsw) {
    q0
  } else {
    logp = log(p)
    fun = function(x) (log(pchisqw(x, weights=weights, lower.tail=lower.tail, epsw=epsw, df0=df0)) - logp)^2
    value = 1
    iter = 0
    max_iter = 10
    while(value>epsv && iter<max_iter) {
      iter = iter + 1
      x = optim(q0, fun, method="Brent", lower=0, upper=5*q0)
    }
    
    if(x$value>epsv) 
      error("Non-convergent optimization (value ",x$value,").")
      
    x$par
  }
  
}

#' @rdname weighted_chisquare
#' @export
pchisqw = function(x, weights, lower.tail=TRUE, epsw=1e-5, df0=2) {

  nx = length(x)
  if(nx==1) {
    weights = matrix(weights, nrow=1)
  } else if(is.vector(weights)) {
    weights = matrix(rep(weights, e=nx), nrow=nx)
  }
  stopifnot(is.matrix(weights), nrow(weights)==nx)
  
  # If all equal, use standard chisquare distribution function
  if(diff(range(weights))<epsw) {
  
    p = pchisq(x, df=df0 * ncol(weights), lower.tail=lower.tail)
  
  # Otherwise use Box (1953), Theorem 2.4
  } else {

    # Calculate the probability of exceeding x
    wx = x / weights
    px = pchisq(wx, df=df0, lower.tail=FALSE)
    a = sapply(1:nx, function(i) 
          sapply(1:ncol(weights), function(j) prod(weights[i,j]/(weights[i,j]-weights[i,-j]))))
    p = apply(a * t(px), 2, sum)
    
    # Get the lower tail probability (i.e. probability of not exceeding x)
    if(lower.tail) p = 1. - p
    
  }
  
  return(p)
  
}      

