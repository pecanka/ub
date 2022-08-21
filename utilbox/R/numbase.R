#' @title
#' Convert base
#'
#' @description
#'
#' `to_base_k()` converts the values in `x` (assumed to be `numeric`, 
#' and thus in base 10) from the decimal base to base `k`.
#'
#' `int2k()` is a version of `to_base_k()`, which allows vector input 
#' in `k`, and returns the conversion of all values in `x` into all 
#' bases specified in `k`.
#'
#' @name to_base_k
#' @family numeric functions provided by utilbox
#' @export
to_base_k = function(x, k, recode=TRUE, min_ndig=1, codes=c(0:9,letters,LETTERS), 
  sep=NULL, collapse=NULL, simplify=TRUE) {
  
  stopifnot(length(k)==1, k>1, k<=length(codes))
  
  pos = lapply(x, to_base_k_1, k, min_ndig)
  
  rec = lapply(pos, function(p) paste(codes[p+1], collapse=sep))
  
  if(simplify && !is.null(sep)) rec = unlist(rec, recursive=FALSE)
  
  if(!is.null(collapse)) paste(unlist(rec), collapse=collapse) else rec
  
}

to_base_k_1 = function(x, k=3, min_ndig=1) {

  rec = c(if(x>=k) Recall(x %/% k, k) else NULL, x %% k)
  
  c(rep(0, max(0,min_ndig-length(rec))), rec)
  
}
  
#' @rdname to_base_k
#' @export
int2k = function(x, k, ..., simplify=TRUE) {

  rec = lapply(k, function(k1) to_base_k(x, k1, ..., simplify=simplify))
  
  if(simplify && length(k)==1) unlist(rec, recursive=FALSE) else rec
  
}
