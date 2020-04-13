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
to_base_k = function(x, k=3) {
  c(if(x>=k) Recall(x %/% k, k) else NULL, x %% k)
}
  
#' @rdname to_base_k
#' @export
int2k = Vectorize(to_base_k)

