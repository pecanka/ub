#' @title
#' Get the modal value of a vector 
#'
#' @description
#'
#' `Mode()` finds the modal value, or mode, of the vector `x`. 
#' Mode is the value that occurs with highest frequency in `x`.
#' Works for both numeric and character/factor `x`.
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
#'
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
#'
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
#'
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
#'
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

#' @title
#' Coefficient of determination
#'
#' @description
#'
#' Calculates the coefficient of determination (the so-called "R squared")
#' for linear regression. With `adjusted=TRUE` the adjusted R squared is
#' calculated (adjusted for `k` parameters). Supply the observed values
#' of the response in `y` and the fitted values in `yh`, where the latter
#' can also be a model obtained via `stats::lm` or `stats::glm`.
#'
#' @examples
#' x = rnorm(100)
#' y = 3*x + rnorm(100)
#' model = lm(y~x)
#' summary(model)[c('r.squared','adj.r.squared')]
#' r_squared(y, model$fitted)
#' r_squared(y, model$fitted, adjusted=TRUE, k=1)
#' r_squared(y, model)
#' r_squared(y, model, adjusted=TRUE)
#'
#' @export
r_squared = function(y, yh, na.rm=TRUE, adjusted=FALSE, k=NULL) {

  if('lm' %in% class(yh)) {
    m = yh
    yh = m$fitted
    k = max(length(m$coefficients) - 1, 1)
  }
  
  c_adj = 1
  
  if(adjusted) {
    if(is.null(k)) stop("With adjusted=TRUE a value for k (number of regressors) must be supplied.")
    n = length(y)
    c_adj = (n - 1) / (n - 1 - k)
  }
  
  1.0 - c_adj * sum((y - yh)^2, na.rm=na.rm) / sum((y - mean(y, na.rm=na.rm))^2, na.rm=na.rm)
  
}

#' @title
#' Test models for difference
#'
#' @description
#'
#' Calculates the p-value of a chisquare test of difference between
#' the log-likelihoods of two models (`m1` and `m2`) at `df` degrees
#' of freedom.
#'
#' @export
test_compare_models = function(m1, m2, df=1) {
  pchisq(2 * abs(logLik(m1) - logLik(m2)), df = df, lower.tail = FALSE)
}