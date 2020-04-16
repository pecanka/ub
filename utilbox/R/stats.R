#' Mode of a sample
#'
#' Finds the mode, i.e. the value that occurs with highest frequency in \code{x}.
#' The function works for both numeric and character/factor data.
#'
#' @examples
#' Mode(c(1:7,7))    # the highest frequency value is '7'
#'
#' @export
Mode = function(x, all_modes=FALSE, exclude.na=TRUE) {
  ux = unique(x)
  if(exclude.na) ux = ux[!is.na(ux)]
  tab = tabulate(match(x, ux))
  if(all_modes) {
    ux[tab == max(tab)]
  } else {
    ux[which.max(tab)]
  }
}

#' Get autocorrelation
#'
#' Calculates autocorrelation of x with given lag
#' @export
acor = function(x, lag=1) {
  suppressWarnings(cor(x[-((length(x)-lag+1):length(x))], x[-(1:(lag))]))
}

#' Logit, probit, inverse logit functions
#' @export
logit = function(p) 
  stats::qlogis(p)
  #log(p/(1-p))

#' @export
probit = function(p)
  stats::qnorm(p)
#probit = stats::qnorm

#' @export
ilogit = stats::plogis
#ilogit = function(x) 1 / (1+exp(-x))
#invlogit = function(x) exp(x)/(1+exp(x))
#invlogit = function(x) exp(x)/(1+exp(-x))

#' Quantile function of a weighted chi-square distribution
#'
#' Returns the quantile of a distribution that arises when independent
#' chisquare-distributed variables with \code{df0} degrees of freedom
#' are summed on the logarithmic scale with weights in \code{w}
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

#' Get the probability of a weighted chi-square sum
#'
#' Calculates the probability of a weighted sum of chisquare-df0 variables
#' being below (lower.tail==TRUE) or above (lower.tail=FALSE) x, where
#' weights specifies the weights. Fails if some (but not all) weights are equal
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

#' Normal tail probability
#'
#' Returns an upper tail probability of the normal distribution 
#' @export
upnorm = function(..., lower.tail=FALSE) stats::pnorm(..., lower.tail=lower.tail)

#' Normal tail probability
#'
#' Returns an upper tail quantile of the normal distribution 
#' @export
uqnorm = function(..., lower.tail=FALSE) stats::qnorm(..., lower.tail=lower.tail)

#' Normal probability of a rectangle
#'
#' Calculates the probability of an arbitrary box with respect to an equi-correlated 
#' multivariate normal random vector of dimension given by the size of upper/lower
#' @export
pmnormrect = function(upper=c(Inf, Inf), lower=c(-Inf,-Inf), mean=rep(0, length(upper)), 
                      Sigma=diag(length(upper)), varcov) {
                      
  if(!missing(varcov) && !missing(Sigma))   
    error("Supply either Sigma or varcov.")
  
  if(!missing(varcov)) Sigma = varcov
  
  llibrary(mnormt)
  
  P = if(all(is.infinite(lower))) 
    mnormt::pmnorm(upper, mean=mean, varcov=Sigma) else
    mnormt::sadmvn(lower=lower, upper=upper, mean=mean, varcov=Sigma)
    
  return(as.numeric(P))
}

#' Normal distribution p-value
#'
#' Calculates the p-value relative to the normal distribution with parameters 
#' \code{m} (mean) and \code{s} (standard deviation).
#'
#' @examples
#' normpval(1.96)        # approx. 2.5%
#' normpval(1.96, TRUE)  # approx. 5.0%
#' @export
normpval = function(x, two.sided=FALSE, m=0, s=1) {
  pnorm(abs(x), lower.tail=ifelse(two.sided, FALSE, x<0)) * (1+two.sided)
  #if(two.sided) 2*pnorm(abs(x), m=m, s=s, lower.tail=FALSE) else pnorm(x, m=m, s=s, lower.tail=FALSE)
}

#' Samples n variables from an arbitrary density function specified in 'pdf'
#' @export
samplepdf = function(n, pdf, ..., spdf.lower = -Inf, spdf.upper = Inf) {

  endsign = function(f, sign = 1) {
      b = sign
      while (sign * f(b) < 0) b = 10 * b
      return(b)
  }

  vpdf = function(v) sapply(v, pdf, ...)  # vectorize
  cdf = function(x) integrate(vpdf, spdf.lower, x)$value
  invcdf = function(u) {
      subcdf = function(t) cdf(t) - u
      if (spdf.lower == -Inf) 
          spdf.lower = endsign(subcdf, -1)
      if (spdf.upper == Inf) 
          spdf.upper = endsign(subcdf)
      return(uniroot(subcdf, c(spdf.lower, spdf.upper))$root)
  }
  
  sapply(runif(n), invcdf)

}

#' Empirical probability frequency (density)
#'
#' @export
#' @examples 
#' ecf(sample(1:3, 100, repl=TRUE))
edf = function(x) {
  y = c(table(x))
  y / sum(y)
}
