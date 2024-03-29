#' @title
#' P-value combination
#'
#' @description
#'
#' Combines p-values using one of the available methods.
#'
#' @export
combine_pvalues = function(P, lam, method=c("jelle","jakub","fisher","stouffer"), summarize=FALSE, ...) {

  if(is.vector(P)) P = matrix(P, nrow=1, ncol=length(P))

  if(summarize) {
    msgf("Smallest raw p-value: ",min(P))
    w = which.min(apply(log10(P), 1, mean))
    msgf("Row with the smallest average p-values (on log scale): ",w)
    print(P[w,])
    c1 = do_pval_comb(P[w,], method="fisher")
    msgf("Fisher combination of these p-values: ",c1)
    c2 = do_pval_comb(P[w,], method="jelle")
    msgf("Jelle combination of these p-values: ",c2)
  }
  
  do_pval_comb(P, lam, method, ...)
  
}

#' @title
#' P-value combination using Fisher method
#'
#' @description
#'
#' Combines p-values using the Fisher combination methods.
#'
#' @export
combine_fisher = function(p, silent=TRUE) {
  do_pval_comb(p, method="fisher", trace=1*!silent)
}

#' @title
#' P-value combination using Fisher method
#'
#' @description
#'
#' Combines p-values using the Stouffer's combination methods
#' @export
combine_stouffer = function(p, silent=TRUE) {
  do_pval_comb(p, method="stouffer", trace=1*!silent)
}

#=======================================================================#
# Code by Jelle Goeman. Calculates the asymptotic p-value using methods #
# of Kotz, Johnson and Boyd (1967) and Box (1954)                       #
#=======================================================================#

.pAsymptotic = function(x, lams, bet, accuracy=1e-12) {

  m = length(lams)

  if (lams[1] == 0) {
    upper = 1.0
  } else {
    if (m == 1) {
      upper = pchisq(x / lams, df = 1, lower.tail = FALSE)
    } else {
      # get the tuning parameter beta
      if (missing(bet)) {
        lams = sort(lams)
        ruben = 2 * lams[1] * lams[m] / (lams[1] + lams[m])
        harmonic = 1/mean(1/lams)
        bet = min(ruben, harmonic) * (1. - 1e-15)
      }
      # get an upper bound to the number of iterations needed
      A = qnorm(.Machine$double.neg.eps)^2
      B = x/bet
      maxiter = trunc(0.5 * (A + B + sqrt(A*A + 2*A*B) - m))
      # starting values
      #maxiter = sum(maxiter)
      d = numeric(maxiter)
      c = numeric(maxiter+1)
      c[1] = prod(sqrt(bet / lams))
      sumc = c[1]
      chi = pchisq(x / bet, df = m, lower.tail = FALSE)
      partialsum = c[1] * chi
      dbase = (1 - bet /lams)
      ready = FALSE
      mixture = TRUE
      ix = 1
      # iterate!
      while (!ready) {
        d[ix] = 0.5 * sum(dbase^ix)
        c[ix+1] = mean(c[1:ix] * d[ix:1])
        if (c[ix+1] < 0)
          mixture = FALSE
        sumc = sumc + c[ix+1]
        partialsum = partialsum + c[ix+1] * chi
        chi = pchisq(x / bet, df = m + 2 * ix + 2, lower.tail = FALSE)
        lower = partialsum + (1.0 - sumc) * chi
        upper = partialsum + 1.0 - sumc
        if (mixture)
          ready = ((upper - lower) / (upper + lower) < 10^-5) || 
                   (ix == maxiter) || (upper < accuracy)
        else {
          ready = TRUE
          upper = Recall(x, lams, mean(c(bet, min(lams))))
        }
        ix = ix + 1
      }
    }
  }
  
  if (upper < accuracy) upper = accuracy / 10.0
  
  return(upper)
}

#' @title
#' Combine p-values
#'
#' @description
#'
#' Performs combination of p-values using Fisher method or Box method 
#' (the latter via Jelle's code in .pAsymptotic or Jakub's code below
#' @export
do_pval_comb = function(P, lam=1, method="jelle", eps_p=1e-3, fac_lam=1e-2, 
  Debug=FALSE, na.rm=TRUE, trace=1) {

  # Check for unknown combination method
  if(all(method!=c("fisher","stouffer","jakub","jelle")))
    stop("Unknown combination method '",method,"'.")
  
  # Replace non-sensical p-values with NA
  if(trace>0) msgf("Checking for NA values ...")
  P[P<0. | P>1.] = NA

  # Make sure P is a matrix
  if(is.vector(P)) P = matrix(P, nrow=1, ncol=length(P))
  
  # Store dimensions of P
  nres = ncol(P)
  np = nrow(P)
  
  # Make sure no weighing is done when lam=1
  if(length(lam)==1 && lam==1) lam = rep(lam, ncol(P))

  if(is.vector(lam)) {
    same_lam = TRUE
    mat_lam = matrix(lam, nrow=np, ncol=nres, byrow=TRUE)
  } else {
    same_lam = FALSE
    mat_lam = lam
  }
  
  # Check problems with P and lambda
  if(any(dim(mat_lam)!=dim(P)))
    stop("Incompatible sizes of lam and P!")
    
  if(all(lam==0)) 
    stop("Some values in lam must be non-zero!")
  
  # Transform p-values to chisquare-2 variables
  if(trace>0) cat("Getting the logarithm of the p-values ...\n")
  if(any(method==c("fisher","jakub","jelle"))) chi2 = -2*log(P)
  
  # Combine using Fisher's method
  if(tolower(method)=="fisher") {
    
    if(trace>0) cat("Combining p-values (",nrow(chi2)," sets) via Fisher's method (no weighing) ...\n", sep="")
    X0 = apply(chi2, 1, sum, na.rm=na.rm)
    nres = apply(!is.na(chi2), 1, sum)
    PP = pchisq(X0, df=2*nres, lower.tail=FALSE)
  
  } else if(tolower(method)=="stouffer") {
    
    if(trace>0) cat("Combining p-values (",nrow(chi2)," sets) via Stouffer's z-score method (with weighing) ...\n", sep="")
    Z = qnorm(P, lower.tail=FALSE)
    X0 = apply(mat_lam*Z, 1, sum, na.rm=na.rm) / sqrt(apply(mat_lam^2, 1, sum))
    PP = pnorm(X0, lower.tail=FALSE)
  
  # Combine using Box's method
  } else {

    if(missing(lam)) stop("Missing lam.")

    # Define lam as matrix and calculate the score
    X0 = apply(mat_lam*chi2, 1, sum, na.rm=na.rm)
    
    if(trace>0) 
      msgf("Combining p-values (",nrow(chi2)," sets) via Box's method (with weighing, ",method,"'s code) ...")
     
    # Use Jelle's general implementation (slow)
    if(tolower(method)=="jelle") {

      PP = sapply(1:np, function(i) .pAsymptotic(X0[i], lams=rep(mat_lam[i,], t=2), accuracy=1e-100))

    # Use Jakub's less general implementation (fast)
    } else {
      
      # If weights are too close, fuzzy them up a bit
      mindif = 0 #.005*diff(range(lam))
      mdif = apply(mat_lam, 1, min_dif)
      
      #print(summary(mdif))
      if(any(mdif <= mindif)) {
        if(trace>0) msgf("Randomizing weights ...")
        if(same_lam) {
          rlam = perturb(lam, mindif=mindif)
          mat_lam = matrix(lam, nrow=np, ncol=nres, byrow=TRUE)      
        } else {
          for(i in 1:np) {
            if(mdif[i]>mindif) next
            if((i==1 || i%%100==0) && trace>0) cat("Randomizing p-value ",i," out of ",np," ...\n",sep="")
            if(Debug) {
              print("--------------------------")
              print(min_dif(mat_lam[i,]))
            }
            mat_lam[i,] = perturb(mat_lam[i,], mindif=mindif)
            if(Debug) 
              print(min_dif(mat_lam[i,]))
          }
          #mat_lam = apply(mat_lam, 1, perturb, mindif=mindif)
        }
      }

      # Check for zeros (which there really should not be at this point)
      mdif = apply(mat_lam, 1, min_dif)
      
      if(any(mdif==0))
        stop("Some weights are equal.")
      
      Q = X0 / mat_lam
      PQ = pchisq(Q, df=2, lower.tail=FALSE)

      a = if(same_lam) {
        sapply(1:nres, function(j) prod(lam[j]/(lam[j]-lam[-j])))
      } else {
        sapply(1:np, function(i) sapply(1:nres, function(j) prod(mat_lam[i,j]/(mat_lam[i,j]-mat_lam[i,-j]))))
      }
      
      PP = apply(a * t(PQ), 2, sum)

      if(Debug) {
        print("summary(lam)"); print(summary(as.vector(lam)))
        print("summary(a)"); print(summary(as.vector(a)))
        print("summary(Q)"); print(summary(as.vector(Q)))
        print("summary(PP)"); print(summary(as.vector(PP)))
        print("min(PQ)"); print(min(PQ, na.rm=TRUE))
        print("min(PP)"); print(min(PP, na.rm=TRUE))
      }
    
    } # if(method=="jakub")

    if(any(is.na(PP) & !is.na(X0)))
      stop("Non-NA input p-values were combined into NA p-values.")
    if(any(!is.na(PP)) && min(PP, na.rm=TRUE)<0)
      stop("Some p-values are negative.")
      
  }

  # If any p-values are negative, correct them using Jelle's code
  if(any(PP<0.0, na.rm=TRUE)) {
    wn = which(PP<0.0)
    if(!exists(".pAsymptotic", mode="function"))
      stop("Missing function .pAsymptotic(). Look for Jelle's script that",
           " defines the function.")
    PP[wn] = sapply(seq_along(wn), function(i) 
               .pAsymptotic(X0[wn[i]], lams=rep(mat_lam[i,], t=2), accuracy=1e-100))
  }
  
  # Check for PP above 1 and trim them if they are very close
  if(any(PP>1.0, na.rm=TRUE))
    PP[PP>1.0 & PP<1+eps_p] = 1.0

  # Check again for PP above 1 even after correction
  if(any(PP>1.0, na.rm=TRUE)) { 
    cat("\nERROR: SOME P-VALUES ABOVE 1!\n\n")
    cat("Maximum p-value: ", max(PP), "\n", sep="")
    print(summary(PP))
    if(interactive()) browser() else stop()
  }

  # Announce the smallest combined p-value
  if(trace>0) msgf("Smallest combined p-value: ",min(PP, na.rm=TRUE))
  
  # Compare the output of Jakub's and Jelle's codes for small p-values
  eps = 1e-6
  if(FALSE && any(PP<eps, na.rm=TRUE)) {
    msgf("Checking results for small p-values (",sum(PP<eps)," p-values below ",eps,") ...")
    ws = which(PP<eps)
    PP2 = sapply(X0[ws], .pAsymptotic, lams=rep(lam, t=2), accuracy=1e-100)
    msgf("Smallest p-value found after re-check: ",min(PP2),"\n")
    md = max(abs(PP[ws]-PP2))
    msgf("Maximum difference between Jakub's and Jelle's code: ",md)
    mrd = max(abs((PP[ws]-PP2)/unzero(PP[ws]+PP2)*2-1))
    msgf("Maximum relative difference between Jakub's and Jelle's code: ",mrd)
    PP[ws] = PP2
  }

  # If there are still negative or NA p-values, stop
  if(any(PP<0.0, na.rm=TRUE))
    stop("Some p-values are negative.")
    
  if(any(is.na(PP) & !is.na(X0)))
    stop("Non-NA input p-values were combined into NA p-values.")

  return(PP)

}

#' Fuzzy up a numeric object
#'
#' `perturb()` adds a small perturbation to those elements that very close
#' together (as defined by `mindif`). By default, it only perturbs elements
#' that are exactly equal (i.e., `mindif=0`). When `preserve_sum=TRUE`, the 
#' perturbation is done in a way that preserves the sum of `x`.
#'
#' @examples
#' perturb(rep(1:10, e=3))
#'
#' @export
perturb = function(x, mindif = 0, preserve_sum = TRUE, maxrep = 100) {

  stopifnot(!missing(x)) 

  nx = length(x)
  
  if(nx==1) 
    return(x)

  mdif = min_dif(x)
  irep = 0
  
  while(mdif <= mindif && irep < maxrep) {
    irep = irep + 1
  
    for(i in 1:nx) {
      d = abs(x[i]-x)
      if(any(d[-i] <= mindif)) {
        w = which(d <= mindif)
        r = runif(length(w),0.95,1.05)
        if(preserve_sum) {
          r = r / sum(r) * sum(abs(x[w]))
          x[w] = r * sign(x[w])
        } else {
          r = r / sum(r) * length(w)
          x[w] = x[w] * r
        }
      }
    }
    
    mdif = min_dif(x)
  
  }
  
  return(x)
  
}

#' Density of the normal distribution p-values
#'
#' `dpval_norm()` returns the density of the distribution of the p-value associated
#' with the test statistic T distributed according to N(a,b^2), i.e., the normal 
#' distribution with the expectation "a" and the standard deviation "b".
#'
#' @examples
#' # Draw values of the statistic
#' m = 0.75
#' s = 1.4
#' X = rnorm(1e5, m, s)
#' # Calculate the p-values and the density
#' P = 1 - pnorm(X)
#' d = dpval_norm(p <- ppoints(1000), m, s)
#' # Plot
#' hist(P, freq = FALSE)
#' lines(d~p, col='red', type='l')
#'
#' @export
dpval_norm = function(p, m = 0, s = 1) {
  Zp = qnorm(1 - p)
  dnorm(Zp, m, s) / dnorm(Zp)
}

