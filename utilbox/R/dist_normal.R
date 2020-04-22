#' @title
#' Normal distribution
#'
#' @description
#'
#' `upnorm()` returns upper tail probabilities of a normal 
#' distribution.
#'
#' `uqnorm()` returns the upper tail quantiles of a normal 
#' distribution.
#'
#' `pmnormrect()` calculates the probability of an arbitrary box with 
#' respect to an equi-correlated multivariate normal random vector of 
#' dimension given by the size of upper/lower.
#'
#' `normpval()` calculates the p-value relative to the normal 
#' distribution with parameters `m` (mean) and `s` (standard deviation).
#'
#' @examples
#' normpval(1.96)        # approx. 2.5%
#' normpval(1.96, TRUE)  # approx. 5.0%
#'
#' @name normaldist
#' @export
upnorm = function(..., lower.tail=FALSE) {
  stats::pnorm(..., lower.tail=lower.tail)
}

#' @rdname normaldist
#' @export
uqnorm = function(..., lower.tail=FALSE) stats::qnorm(..., lower.tail=lower.tail)

#' @rdname normaldist
#' @export
pmnormrect = function(upper=c(Inf, Inf), lower=c(-Inf,-Inf), mean=rep(0, length(upper)), 
                      Sigma=diag(length(upper)), varcov) {
                      
  if(!missing(varcov) && !missing(Sigma))   
    error("Supply either Sigma or varcov.")
  
  if(!missing(varcov)) Sigma = varcov
  
  check_namespace('mnormt')
  
  P = if(all(is.infinite(lower))) {
    mnormt::pmnorm(upper, mean=mean, varcov=Sigma) 
  } else {
    mnormt::sadmvn(lower=lower, upper=upper, mean=mean, varcov=Sigma)
  }
    
  as.numeric(P)
  
}

#' @rdname normaldist
#' @export
normpval = function(x, two.sided=FALSE, m=0, s=1) {
  pnorm(abs(x), lower.tail=ifelse(two.sided, FALSE, x<0)) * (1+two.sided)
  #if(two.sided) 2*pnorm(abs(x), m=m, s=s, lower.tail=FALSE) else pnorm(x, m=m, s=s, lower.tail=FALSE)
}
