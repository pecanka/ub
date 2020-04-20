#' @title
#' Mode of a sample
#'
#' @description
#' `Mode()` finds the model of a sample `x`, which is the value that 
#' occurs with highest frequency in `x`. The function works for both 
#' numeric and character/factor data.
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

#' @title
#' Empirical probability frequency (density)
#'
#' @description
#' `edf()` gives the empirical probability frequency (i.e. the 
#' density).
#'
#' @examples 
#' ecf(sample(1:3, 100, repl=TRUE))
#'
#' @export
edf = function(x) {
  y = c(table(x))
  y / sum(y)
}

#' @title
#' Get autocorrelation
#'
#' @description
#' Calculates the autocorrelation of `x` with given `lag`.
#'
#' @export
acor = function(x, lag=1) {
  suppressWarnings(cor(x[-((length(x)-lag+1):length(x))], x[-(1:(lag))]))
}

#' @title
#' Logit, probit, inverse logit functions
#'
#' @description
#' `logit()` gives the logit function, which is also the quantile 
#' function of the logistic distribution. More exactly, it is the 
#' function *f(p)=log(p/(1-p))*.
#'
#' `probit()` is the probit function, which is also the quantile 
#' function of the standard normal distribution.
#'
#' `ilogit()` is the inverse logit function, i.e. the distribution 
#' function of the logistic distribution.
#'
#' @export
logit = function(p) {
  stats::qlogis(p)
  #log(p/(1-p))
}

#' @export
probit = function(p) {
  stats::qnorm(p)
}
#probit = stats::qnorm

#' @export
ilogit = function(q) {
  stats::plogis(q)
}
#ilogit = function(x) 1 / (1+exp(-x))
#invlogit = function(x) exp(x)/(1+exp(x))
#invlogit = function(x) exp(x)/(1+exp(-x))

#' @title
#' Produce a variance matrix
#'
#' @description
#' Produces a variance matrix with `rho` on the diagonal according to 
#' the specified type which can be either "random" (see `rvariance`) or 
#' has the structure according to `type`.
#'
#' @export
def_Sigma = function(rho, N, nb=1, random=FALSE, type=c("block", "band"), bandsize=0) {

  type = match.arg(type)
  
  if(N==1) return(diag(1))
  
  if(random) {
    Sigma = rvariance(N)
  } else {
  
    if(N%%nb!=0) 
      error("Number of blocks 'nb' must divide 'N'.")
      
    n = N%/%nb
    Sigma = matrix(rho, nrow=n, ncol=n)
    
    if(type=="band") {
      llibrary(Matrix)
      Sigma = band(Sigma, -abs(bandsize), abs(bandsize))
    }
    
    Sigma = 0.5*(Sigma+t(Sigma))
    diag(Sigma) = 1.
    if(nb>1) Sigma = drep(Sigma, nb)
    
  }

  Sigma
  
}
