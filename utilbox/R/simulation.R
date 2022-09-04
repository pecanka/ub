#' @title
#' Draw a positive-definite matrix
#'
#' @description
#'
#' `rposdefmat()` generates a \"random\" (more like arbitrary) 
#' positive-definite matrix with user-specified positive eigenvalues. If 
#' eigenvalues are not specified, they are generated from a uniform 
#' distribution on (1,10)
#'
#' `rvariance()` draws a \"random\" variance matrix with `sigma2` on 
#' the diagonal.
#'
#' @name simulate_matrix
#' @export
rposdefmat = function(n, ev=runif(n,1,10), all.positive=FALSE) {

  Z = matrix(rnorm(n^2), ncol=n)
  decomp = qr(Z)
  Q = qr.Q(decomp) 
  R = qr.R(decomp)
  d = diag(R)
  ph = d / abs(d)
  O = Q %*% diag(ph)
  Z = t(O) %*% diag(ev) %*% O
  if(all.positive) Z = abs(Z)
  
  return(Z)
  
}

#' @name simulate_matrix
#' @export
rvariance = function(n, sigma2=rep(1,n), ev=runif(n,1,200)) {
  if(n==1) {
    as.matrix(sigma2)
  } else {
    V = rposdefmat(n, ev=ev)
    D = diag(sqrt(sigma2 / diag(V)))
    V = D %*% V %*% D
    0.5*(V + t(V))
  }
}

#' @title
#' Draw random sample of p-values under the normal distribution
#'
#' @description
#'
#' `rnormpval()` draws a random sample of size `M` of p-values 
#' relative to the normal distribution with parameters `m` (mean) and 
#' `s` (standard deviation).
#'
#' @export
rnormpval = function(M, mu, Sigma=def_Sigma(rho, N=length(mu)), rho=0, two.sided=FALSE, trace=1, what=c("p","X","pX")) {

  #stopifnot(!missing(M), !missing(mu), !missing(Sigma))
  stopifnot(is.matrix(Sigma))
  what = match.arg(what)
  
  if(missing(mu)) mu = rep(0, nrow(Sigma))

  if(is_diag(Sigma)) {
  
    X = matrix(rnorm(M*length(mu), m=rep(mu, e=M), s=rep(sqrt(diag(Sigma)), e=M)), nrow=M)
  
  } else {
 
    check_namespace('mnormt')
    X = try(mnormt::rmnorm(M, mean=mu, varcov=Sigma))
    
    if(is_error(X)) return(rep_list(list(p=NA), M))
    
    if(M==1) X = matrix(X, nrow=1)
    
  }
  
  if(what=="p" || what=="pX") {
    #p = if(two.sided) 2*pnorm(abs(X), lower.tail=FALSE) else pnorm(X, lower.tail=FALSE)
    p = normpval(X, two.sided)
  }
  
  switch(what, "X"=X, "p"=p, list(X=X, p=p))
      
}

#rnormm = function(..., what="X") rnormpval(..., what=what)

#' @title
#' Sample from a distribution
#'
#' @description
#'
#' `samplepdf()` samples `n` variables from an arbitrary density 
#' function specified in `pdf`.
#'
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

#' @title
#' Simulate p-values
#'
#' @description
#'
#' `simulate_pval()` simulates p-values (from the normal model only 
#' so far).
#'
#' @export
simulate_pval = function(M, Sigma, N, n=1, n0=N, mu0=0, n1=0, mu1=0, alpha=0.05, type="bySigma", 
  model=c("norm", "unif"), two.sided=FALSE, evaluate_signif=FALSE, lambda, trace=1) {

  if(missing(M)) 
    stop("Missing argument 'M'.")
    
  if(missing(N)) {
    
    if(missing(Sigma) || !is.matrix(Sigma)) 
      stop("Either 'N' or 'Sigma' (matrix) must be supplied.")
      
    N = nrow(Sigma)
    
  }
  
  if(trace>0) cat("Simulating ",M*N," p-values ... ")
  
  if(!missing(Sigma) && is.null(Sigma) && type=="bySigma")
    stop("Either supply non-null Sigma or change the type to something other than 'bySigma'.")
          
  if(!missing(Sigma) && !is.null(Sigma)) {
  
    label = "bySigma"
    
    if(length(Sigma)==1) {
      Sigma = diag_matrix(rep(1, N), Sigma)
    }
    
    if(n0+n1 != nrow(Sigma)) 
      stop("There's a conflict between n0, n1 and Sigma.")    
      
    if(length(mu0) != 1 && length(mu0) != n0) 
      stop("Supply consistent mu0 and n0.")
      
    if(length(mu1) != 1 && length(mu1) != n1) 
      stop("Supply consistent mu1 and n1.")

    mu = sqrt(n) * c(rep(mu1, n1/length(mu1)), rep(mu0, n0/length(mu0)))
    
    model = head(model, 1)
    
    if(model=="norm") {
    
      if(is_diag(Sigma)) {
      
        nn = matrix(rnorm(M*length(mu), m=rep(mu, e=M), s=rep(sqrt(diag(Sigma)), e=M)), nrow=M)
      
      } else {
     
        llibrary(mnormt)
        nn = try(mnormt::rmnorm(M, mean=mu, varcov=Sigma))
        if(class(nn)=="try-error") return(list(p=NA))
        if(M==1) nn = matrix(nn, nrow=1)
        
      }
      
      p = if(two.sided) 2*pnorm(abs(nn), lower.tail=FALSE) else pnorm(nn, lower.tail=FALSE)
      rm(nn)
      
    } else stop("Unknown model '",model,"'.")
    
    if(!is_posdef(Sigma)) 
      stop("Sigma must be positive-definite.")
    
    N = nrow(Sigma)
    
    ### DEBUG ###
    add_dependent = FALSE
    if(add_dependent) {
      if(N==1) {
        l1 = l2 = 1/3; l3 = max(0,1 - l1 - l2)
        p0 = p
        p = cbind(p0, 1-p0)
      } else if(N==3) {
        p[,ncol(p)] = 1 - apply(p[,1:(ncol(p)-1)], 1, max)
      }
    }
    ### DEBUG ###
    
  } else if(type=="indep") {

    label = "indep"
    if(trace>0) cat("(independent) ... ")
    p = matrix(runif(N*M), nrow=M, ncol=N)

  } else if(type=="maxdep") {

    label = "maxdep"
    if(trace>0) cat("(max dependent) ... ")
    p = matrix(runif(N*M), nrow=M, ncol=N)
    p[,ncol(p)] = 1 - apply(p[,1:(ncol(p)-1),drop=FALSE], 1, mean)

  } else if(type=="complcounterdep") {

    label = "complcounterdep"
    if(trace>0) cat("(completely counter dependent) ... ")
    p = matrix(runif(M), nrow=M, ncol=1)
    p = if(N==2) cbind(p, 1-p) else if(N==3) cbind(p, p, 1-p)    

  } else if(type=="compldep") {
    
    label = "compldep"
    if(trace>0) cat("(completely dependent) ... ")
    p = matrix(runif(M), nrow=M, ncol=N)

  } else stop("Unknown type=",type,".")

  if(trace>0) cat("done.\n")

  # Evaluate significance
  if(evaluate_signif) {
    if(trace>0) cat("Evaluating significance ... ")
    lam = eval(parse(text=lambda))
    keep = p <= lam
    n2 = apply(keep, 1, sum)
    p2 = p / lam
    fwer = mean(apply(n2>0 & p2 <= alpha / n2, 1, any))
    if(trace>0) cat("done.\n")
  } else p2 = n2 = keep = fwer = NULL
  
  list_clean(list(p=p, p2=p2, n2=n2, keep=keep, fwer=fwer))
  
} # simulate_pval
