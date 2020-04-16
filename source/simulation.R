#' Check if 'x' is a diagonal matrix 
#' @export
is_diag = function(x) {
  is.matrix(x) && nrow(x)==ncol(x) && 
    (length(x)==1 || max(abs(x[upper.tri(x)]))<.Machine$double.eps)
}

#' Check if M is a positive-definite matrix
#' @export
is_posdef = function(M) {

  stopifnot(is.numeric(M))
  if(length(M)==1 && M>0) return(TRUE)
  stopifnot(is.matrix(M), nrow(M)==ncol(M))
  
  llibrary(mnormt)
  x = try(mnormt::rmnorm(1, mean=rep(0,nrow(M)), varcov=M), silent=TRUE)
  if(class(x)!="try-error") TRUE else structure(FALSE, msg=as.character(attr(x, "condition")))
  
}

#' Draw a positive-definite matrix
#'
#' Generates a "random" (more like arbitrary) positive-definite matrix with 
#' user-specified positive eigenvalues. If eigenvalues are not specified, 
#' they are generated from a uniform distribution on (1,10)
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

#' Draw a random variance matrix with sigma2 on the diagonal
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

#' Produce a variance matrix
#'
#' Produces a variance matrix with \code{rho} on the diagonal according to the 
#' specified type which can be either "random" (see \code{rvariance}) or has 
#' the structure according to \code{type}.
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

#' Draw a normal distribution p-value
#'
#' Draws a random sample of p-values relative to the normal distribution with 
#' parameters \code{m} (mean) and \code{s} (standard deviation).
#' @export
rnormpval = function(M, mu, Sigma=def_Sigma(rho, N=length(mu)), rho=0, two.sided=FALSE, trace=1, what=c("p","X","pX")) {

  #stopifnot(!missing(M), !missing(mu), !missing(Sigma))
  stopifnot(is.matrix(Sigma))
  what = match.arg(what)
  
  if(missing(mu)) mu = rep(0, nrow(Sigma))

  if(is_diag(Sigma)) {
  
    X = matrix(rnorm(M*length(mu), m=rep(mu, e=M), s=rep(sqrt(diag(Sigma)), e=M)), nrow=M)
  
  } else {
 
    llibrary(mnormt)
    X = try(mnormt::rmnorm(M, mean=mu, varcov=Sigma))
    if(class(X)=="try-error") return(list(p=NA))
    if(M==1) X = matrix(X, nrow=1)
    
  }
  
  if(what=="p" || what=="pX")
    #p = if(two.sided) 2*pnorm(abs(X), lower.tail=FALSE) else pnorm(X, lower.tail=FALSE)
    p = normpval(X, two.sided)
  
  return(switch(what, "X"=X, "p"=p, list(X=X, p=p)))
      
}

rnormm = function(..., what="X") rnormpval(..., what=what)

#' Simulate p-values
#'
#' A function that simulates p-values (from the normal model only so far)
#' @export
simulate_pval = function(M, Sigma, N, n=1, n0=N, mu0=0, n1=0, mu1=0, alpha=0.05, type="bySigma", 
  model=c("norm", "unif"), two.sided=FALSE, evaluate_signif=FALSE, lambda, trace=1) {

  if(missing(M)) 
    error("Missing argument 'M'.")
    
  if(missing(N)) {
    
    if(missing(Sigma) || !is.matrix(Sigma)) 
      error("Either 'N' or 'Sigma' (matrix) must be supplied.")
      
    N = nrow(Sigma)
    
  }
  
  if(trace>0) cat("Simulating ",M*N," p-values ... ")
  
  if(!missing(Sigma) && is.null(Sigma) && type=="bySigma")
    error("Either supply non-null Sigma or change the type to something other than 'bySigma'.")
          
  if(!missing(Sigma) && !is.null(Sigma)) {
  
    label = "bySigma"
    
    if(length(Sigma)==1) Sigma = diag2(rep(1, N), Sigma)
    
    if(n0+n1!=nrow(Sigma)) 
      error("There's a conflict between n0, n1 and Sigma.")    
    if(length(mu0)!=1 && length(mu0)!=n0) 
      error("Supply consistent mu0 and n0.")
    if(length(mu1)!=1 && length(mu1)!=n1) 
      error("Supply consistent mu1 and n1.")

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
      
    } else error("Unknown model '",model,"'.")
    
    if(!is_posdef(Sigma)) 
      error("Sigma must be positive-definite.")
    
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

  } else error("Unknown type=",type,".")

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
