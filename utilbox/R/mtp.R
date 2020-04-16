#' Perform multiple testing procedure
#'
#' Given a vector of p-values on input, performs a selected multiple testing
#' procedure.
#'
#' @return A vector of rejections.
#'
#' @export
test_pval = function(p, method="cbonf", alpha=0.05, lamSFG=0.5, lam, scale_up=TRUE) {

  methods = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none","SFG","hmp","Hartung")
  cmethods = paste0("c",methods)
  method = match.arg(method, c(methods, cmethods))
  do_cond = method %in% cmethods
  
  if(do_cond) {
    
    if(missing(lam)) 
      error("Missing argument 'lam'.")
      
    wk = p < lam
    pp = p[wk] / ifelse(scale_up, lam, 1)
    meth = sub("^c","",method)
    
  } else {
  
    pp = p
    wk = rep(TRUE, length(pp))
    meth = method
    
  }
  
  rej = rep(FALSE, length(p))
  if(meth=="hommel") {
    
    if(any(wk)) rej[wk] = !statmod::hommel.test(pp, alpha=alpha)
    
  } else if(meth=="SFG") {
  
    if(any(wk)) rej[wk] = !sfg.test(pp, alpha=alpha, lamSFG=lamSFG)
  
  } else if(meth=="hmp") {
  
    if(any(wk)) rej[which(wk)[1]] = !hmp.test(pp, alpha=alpha)
  
  } else if(meth=="Hartung") {
  
    if(any(wk)) rej[which(wk)[1]] = !hartung.test(pp, alpha=alpha)
  
  } else if(any(meth==methods)) {
  
    if(any(wk)) rej[wk] = p.adjust(pp, method=meth) < alpha
  
  } else error("Unknown method '",method,"'.")
  
  rej

}
  
#' Bonferroni test
#' @export  
bonf.test = function(p, alpha=0.05, lam=2, scale_up=TRUE, what="nr") {
 
  if(is.vector(p)) p = matrix(p, ncol=1)

  do_cond = lam <= 1. && lam > 0
  N = if(do_cond) apply(p < lam, 2, sum) else nrow(p)
  pp = p * N / if(do_cond && scale_up) lam else 1
  non_signif = pp >= alpha
  
  switch(what, "nr"=non_signif, "p"=pp, cbind(ns=non_signif, p=pp))
  
}

#' Holm test
#' @export  
holm.test = function(p, alpha=0.05, what="nr") {

  n = length(p)
  i = 1:n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * (n+1-i)
  non_signif = pp >= alpha
  wns = which(non_signif)
  if(length(wns)>0) non_signif[wns[1]:n] = TRUE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#' Hochberg test
#' @export  
hochberg.test = function(p, alpha=0.05, what="nr") {
  
  n = length(p)
  i = 1:n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * (n+1-i)
  non_signif = pp >= alpha
  ws = which(!non_signif)
  if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#' Benjamini-Hochberg test (FDR)
#' @export  
bh.test = function(p, alpha=0.05, what="nr") {
  
  n = length(p)
  i = 1:n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * n / i
  non_signif = pp >= alpha
  ws = which(!non_signif)
  if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#' Benjamini-Yekutieli test (FDR)
#' @export  
by.test = function(p, alpha=0.05, what="nr") {

  n = length(p)
  i = 1:n
  C = log(n) - digamma(1) + 0.5 / n
  ord = i
  po = if(is.unsorted(p)) { ord = order(p); p[ord] } else p
  pp = po * n * C / i
  non_signif = pp >= alpha
  ws = which(!non_signif)
  if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
  r = rank(p)
  
  switch(what, "nr"=non_signif[r], "p"=pp[r], cbind(ns=non_signif[r], p=pp[r]))
  
}

#' Storey test (aka Storey-Fincher-Goncharuk test)
#' @export  
sfg.test = function(p, alpha=0.05, lamSFG=0.5, lam=2, scale_up=TRUE, what="nr") {
 
  if(is.vector(p)) p = matrix(p, ncol=1)

  do_cond = lam <= 1. && lam > 0
  N = if(do_cond) apply(p < lam, 2, sum) else nrow(p)
  p = p / if(do_cond && scale_up) lam else 1
  m0 = if(lamSFG>=1) nrow(p) else ( apply(p > lamSFG & p <= 1, 2, sum) + 1 ) / ( 1 - lamSFG )
  pp = p * m0 / if(do_cond && scale_up) lam else 1
  non_signif = pp >= alpha
  
  switch(what, "nr"=non_signif, "p"=pp, cbind(ns=non_signif, p=pp))
  
}
  
#' Harmonic mean p-value test
#'
#' This implements the global test based on the harmonic mean 
#' p-value, which is a combination p-value that is insensitive
#' to dependence among the combined p-values. 
#'
#' Requires package \code{harmonicmeanp}.
#'
#' @export  
hmp_test = function(p, alpha=0.05, what="ns") { 
  non_signif = harmonicmeanp::p.hmp(p) >= alpha
  switch(what, "ns"=non_signif, 
         stop(this_fun_name(), " can only return 'ns' (i.e. combined non-significance status)."))
  
}

#' Hartung test for dependent p-values
#'
#' Requires library \code{punitroots} which can be found at r-forge (see examples below for installation).
#' 
#' @examples
#' install.packages("urca")
#' install.packages("CADFtest")
#' install.packages("punitroots", repos="http://r-forge.r-project.org")
#' @export  
hartung.test = function(p, alpha=0.05, kappa=0.2, what="ns") {
 
  non_signif = punitroots::Hartung(p, kappa=kappa)$p.value >= alpha
  
  switch(what, "ns"=non_signif, error("Unknown value in 'what'."))
  
}

#' @export  
mtc.test = function(p, alpha=0.05, method="holm") {
  
  ## Hommel not yet implemented here, so just use statmod's version
  if(method=="hommel") return(statmod::hommel.test(p, alpha=alpha))
  
  # Sort the p-values (if needed)
  po = if(method!="bonferroni" && is.unsorted(p)) sort(p) else p
  
  # Set the corrections
  n = length(p)
  i = 1:n
  k = switch(method, "bonferroni"=n, 
                     "holm"=n+1-i, 
                     "hochberg"=n+1-i, 
                     "BH"=n/i, 
                     "BY"=n/i*(log(n)-digamma(1)+0.5/n), 
                     stop("Unknown method."))
  
  # Determine rejections
  non_signif = po * k >= alpha  
  
  # Modify rejections for holm, hochberg, bh, by
  if(method=="holm") {
    wns = which(non_signif)
    if(length(wns)>0) non_signif[wns[1]:n] = TRUE
    non_signif = non_signif[rank(p)]
  } else if(any(method==c("hochberg","BH","BY"))) {
    ws = which(!non_signif)
    if(length(ws)>0) non_signif[1:tail(ws,1)] = FALSE
    non_signif = non_signif[rank(p)]
  }
  
  return(non_signif)
}

#' @export
mtc.test.mul = function(p, alpha=0.05, method="holm") {

  if(is.vector(p)) p = matrix(p, ncol=1)

  if(method=="hommel") {
    apply(p, 2, statmod::hommel.test, alpha=alpha) 
  } else apply(p, 2, mtc.test, alpha=alpha, method=method)
  
}

#' Fast multiple testing corrections
#' @export
mtctestFast = function(p, alpha=0.05, method=c("bonferroni","hommel","holm","hochberg","BH","BY","fdr","none","SFG"),
  n1=-1, lam=2, lamSFG=0.5, what=c("fwer","fdr","pow"), trace=0, dll_lib=NULL, unload=TRUE) { 
  
  if(is.null(dll_lib))
    dll_lib = "d:/Dropbox/Projects/Jules/Scripts/fortran/mtc.dll"
    
  cat("\nUsing DLL library '",dll_lib,"' ...\n")
  
  file_exists_must(dll_lib)
  
  method = match.arg(method)
  method = switch(method, "none"=0,
                          "bonferroni"=1, 
                          "hommel"=2, 
                          "holm"=3, 
                          "hochberg"=4, 
                          "BH"=, "fdr"=5, 
                          "BY"=6,
                          "SFG"=7,
                          error("Unknown method '",method,"'."))
                  
  if(is.vector(p)) p = matrix(p, ncol=1)
                        
  n = nrow(p)
  m = ncol(p)
  stopifnot(n>0, m>0)
  
  if(!any(names(getLoadedDLLs())=="mtc")) {
  
    if(trace>0) 
      catn("Loading library file '",dll_lib,"' ...")
    
    if(!file.exists(dll_lib)) 
      error("Library file '",dll_lib,"' does not exist!")
      
    loaded = dyn.load(dll_lib)
    
  }
  
  # Make sure the variables have the right size and type
  ns = matrix(FALSE, nrow=n, ncol=m)
  get_fwer = as.logical("fwer" %in% what)
  get_fdr = as.logical("fdr" %in% what)
  get_pow = as.logical("pow" %in% what)
  fwer = fdr = double(1)
  pow = double(max(1,n1))
  n = as.integer(n)
  m = as.integer(m)
  method = as.integer(method)
  ierr = integer(1)
  alpha = as.double(alpha)
  lam = as.double(lam)
  lamSFG = as.double(lamSFG)
  n1 = as.integer(n1)
  
  # Call the Fortran function
  ffun = '__mtc_MOD_mtctest_mul'
  if(trace>0) cat("Calling external function '",ffun,"' for method '",method,"' ... ")
  #OUT = .C(ffun, ns=ns, p=p, n=n, m=m, alpha=alpha, method=method, ierr=ierr, 
  #         lam=lam, lamSFG=lamSFG, n1=n1, fwer=fwer, fdr=fdr, pow=pow)
  args = list(ffun, ns=ns, p=p, n=n, m=m, alpha=alpha, method=method, ierr=ierr, 
              lam=lam, lamSFG=lamSFG, n1=n1, get_fwer=get_fwer, get_fdr=get_fdr, 
              get_pow=get_pow, fwer=fwer, fdr=fdr, pow=pow)
  OUT = do.call(".C", args)
  
  if(trace>0) cat("done.\n")
  
  if(OUT$ierr!=0) 
    error("Non-zero return code '",OUT$ierr,"' returned by '",ffun,"'.")
  
  if(unload && any(names(getLoadedDLLs())=="mtc")) dyn.unload(dll_lib)
  
  return(if(n1>=0) list(ns=OUT$ns, fwer=OUT$fwer, fdr=OUT$fdr, pow=OUT$pow) else OUT$ns) 

}

#' Type-I and type-II errors
#'
#' Determine the type-I (FWER, FDR) and type-II (power) errors of 
#' selected multiple testing procedures
#' @export
mtp_errors = function(p, method="cbonferroni", n1=0, alpha=0.05, lam, lamSFG=0.5, scale_up=TRUE) {

  adj_methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  methods = c(adj_methods, "SFG", "hmp")
  cmethods = paste0("c",methods)
  method = match.arg(method, c(methods, cmethods))
  do_cond = method %in% cmethods
  
  if(do_cond && missing(lam)) 
    error("Missing argument 'lam'.")
  
  if(is.vector(p)) p = matrix(p, ncol=1)
  n = nrow(p)
  n1 = max(n1, 0)

  meth = if(do_cond) sub("^c","",method) else method

  if(meth=="bonferroni") {
  
    if(TRUE) {
    N = if(do_cond) apply(p < lam, 2, sum) else n
    a = alpha / N * if(do_cond && scale_up) lam else 1
    p0 = p[(n1+1):n,,drop=FALSE]
    fwer = mean(apply(p0, 2, min) < a)
    p1 = if(n1==0) NULL else p[1:n1,,drop=FALSE]
    nsig = if(n1==0) NA else apply(p1 < a, 2, sum)
    pow = sapply(1:n1, function(k) mean(nsig>=k))
    } else
    rej = bonf.test(p, alpha=alpha, lam=lam, scale_up=scale_up)
  
  } else {
  
    M = ncol(p)
    rej = matrix(FALSE, nrow=n, ncol=M)
    for(i in 1:M) {
    
      if(do_cond) {
        wk = p[,i] < lam
        pp = p[wk,i] / ifelse(scale_up, lam, 1)
      } else {
        wk = rep(TRUE, n)
        pp = p[,i]
      }
      
      if(!any(wk)) next
              
      rej[wk,i] = 
      if(meth=="bonferroni") {
        !mtc.test(pp, alpha=alpha, method="bonferroni")
      } else if(meth=="hommel") {
        !statmod::hommel.test(pp, alpha=alpha)
      } else if(meth=="holm") {
        !holm.test(pp, alpha=alpha)
      } else if(meth=="hochberg") {
        !hochberg.test(pp, alpha=alpha)
      } else if(meth=="BH") {
        !bh.test(pp, alpha=alpha)
      } else if(meth=="BY") {
        !by.test(pp, alpha=alpha)
      } else if(meth=="SFG") {
        !sfg.test(pp, alpha=alpha, lamSFG=lamSFG)
      } else if(any(meth==adj_methods)) {
        p.adjust(pp, method=meth) < alpha
      } else error("Unknown method '",method,"' in test_pval.")
      
    } # for(i in 1:M)
    
    fwer = mean(apply(rej[(n1+1):n,,drop=FALSE], 2, max)>0)
    pow = sapply(1:n1, function(k) mean(apply(rej[1:n1,,drop=FALSE], 2, sum)>=k))
      
  }
  
  return(list(fwer=fwer, fdr=NA, pow=pow))

} # mtp_errors
  
#############################################################

#' @name mtp_errors
#' @export
mtp_errors_fast = function(p, method="cbonferroni", n1=0, alpha=0.05, lamSFG=0.5, 
  lam, scale_up=TRUE, what=c("fwer","fdr","pow")) {

  methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none", "SFG")
  cmethods = paste0("c",methods)
  method = match.arg(method, cmethods)
  do_cond = method %in% cmethods
  
  if(do_cond && missing(lam)) 
    error("Missing argument 'lam'.")
  
  X = mtctestFast(p, alpha=alpha, method=if(do_cond) sub("^c","",method) else method, 
                  n1=max(n1, 0), lam=ifelse(do_cond, lam, 2), what=what)
      
  list(fwer=X$fwer, fdr=X$fdr, pow=X$pow)

}
  
#############################################################

#' @name mtp_errors
#' @export
mtp_errors_slow = function(p, methods, n1=0, k, alpha=0.05, lamSFG=0.5, lam, scale_up=TRUE, trace=1, what=c("fwer","fdr","power")) {

  if(missing(methods))
    methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "SFG", "hmp", "Hartung")
    
  if(is.vector(p)) p = matrix(p, ncol=1)

  L = list()
  for(method in methods) {
  
    if(trace>0) 
      cat0("evaluating rejections for method '",method,"' (column-wise on p) ... ")
    
    rej = apply(p, 2, test_pval, alpha=alpha, method=method, lam=lam, scale_up=scale_up, lamSFG=lamSFG)  
    
    if(trace>0) 
      cat0("evaluating fwer, fdr, power ... ")
    
    fwer = fdr = power = NULL
    
    if(n1<nrow(rej) && any(c("fwer","fdr") %in% what)) { 
      nrej0 = apply(rej[(n1+1):nrow(rej),,drop=FALSE], 2, sum)
      if("fwer" %in% what) fwer = mean(nrej0>=1)
      if("fdr" %in% what) fdr = mean(nrej0 / pmax(1,apply(rej, 2, sum)))
    }
    
    if(n1>0 && "power" %in% what) {
      if(missing(k)) k = 1:n1
      nrej1 = apply(rej[k,,drop=FALSE], 2, sum)
      power = sapply(k, function(k1) mean(nrej1>=k1))
      names(power) = paste0(">=",k)
    }

    L1 = Filter(length, list(fwer=fwer, fdr=fdr, power=power))
    L = append(L, structure(list(L1), names=method))
    
    if(trace>0) catn()
    
  }
  
  L

}
