#' Convert base
#'
#' `to_base_k` converts its length-1 argument (assumed to be 
#' `numeric`) from the decimal base to base `k`. 
#'
#' `int2k` is a vectorized version of `to_base_k`.
#'
#' @name to_base_k
#' @family numeric functions provided by utilbox
#' @export
to_base_k = function(x, k=3, recode=TRUE, codes=c(0:9,letters,LETTERS), sep=NULL, collapse=NULL) {
  stopifnot(length(k)==1)
  pos = lapply(x, to_base_k_1, k)
  rec = lapply(pos, function(p) collapse0(codes[p+1], sep=sep))
  rec = lapply(rec, str_pad, max(sapply(pos, length)), padding='0')
  if(!is.null(collapse)) collapse0(unlist(rec), sep=collapse) else rec
}

to_base_k_1 = function(x, k=3) {
  c(if(x>=k) Recall(x %/% k, k) else NULL, x %% k)
}
  
#' @rdname to_base_k
#' @export
int2k = Vectorize(to_base_k)
