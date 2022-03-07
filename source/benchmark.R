#' @title
#' Benchmarking
#'
#' @description
#'
#' Simply a short-named alias for `microbenchmark::microbenchmark`.
#'
#' @export
mb = function(...) {

  check_namespace('microbenchmark')

  microbenchmark::microbenchmark(...)
  
  #bench = try(microbenchmark::microbenchmark(...))
  #if(is_error(bench))
  #  stop("A call to 'microbenchmark::microbenchmark' failed.",
  #       " Looks like the 'microbenchmark' package is not installed.")


}
