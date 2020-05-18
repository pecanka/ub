#' @title
#' Simple bootstrap
#'
#' @description
#'
#' Returns a vector of B bootstrap values of real-valued statistic 
#' `T`, where `T` should be an R-function which returns a scalar. 
#' Arguments of `T` can be supplied via the ellipsis (`...`). If length 
#' of `x` and `B` are small enough, do it all at once Otherwise, do it 
#' for each iteration separately.
#'
#' @examples
#' sd(bootstrap(rnorm(100), mean, B=1000))    # compare with 1/sqrt(100)
#'
#' @export
Bootstrap = function(x, statistic, B = 100., ..., portion=1e8) {
  
  if(length(x)*B < portion) {
    X = matrix(sample(x, length(x)*B, replace = TRUE), nrow=length(x))
    apply(X, 2, statistic, ...)
  } else {
    sapply(1:B, function(i) statistic(sample(x, replace = TRUE), ...))
  }
  
}
