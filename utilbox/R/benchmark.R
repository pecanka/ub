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

  do.call(microbenchmark::microbenchmark, list(...))
  
  #bench = try(do.call(microbenchmark::microbenchmark, list(...)))
  #if(is_error(bench))
  #  stop("A call to 'microbenchmark::microbenchmark' failed.",
  #       " Looks like the 'microbenchmark' package is not installed.")


}
