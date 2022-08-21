#' @title
#' Benchmarking
#'
#' @description
#'
#' Simply a short-named alias for `microbenchmark::microbenchmark`.
#'
#' @export
mb = function(..., list = NULL, times = 100L, unit = NULL, check = NULL, control = list()) {
  check_namespace("microbenchmark")
  exprs = c(as.list(match.call(expand.dots = FALSE)$...), list)
  args = list(list=exprs, times=times, unit=unit, check=check, control=control)
  do.call('microbenchmark', args = args, envir=asNamespace('microbenchmark'))
}

## Another version which works by adding arguments `envir` and `parent.frame`
## into `microbenchmark::microbenchmark`.
#mb = function(..., list = NULL, times = 100L, unit = NULL, check = NULL, control = list(), 
#  envir=base::parent.frame()) {
#
#  check_namespace("microbenchmark")
#  
#  exprs = c(as.list(match.call(expand.dots = FALSE)$...), list)
# 
#  fun = microbenchmark::microbenchmark
#  formals(fun)$envir = function() base::parent.frame()
#  formals(fun)$parent.frame = function() get('envir', envir=base::parent.frame())
#
#  #unlockBinding('microbenchmark', asNamespace('microbenchmark'))
#  #assign('microbenchmark', fun, envir=asNamespace('microbenchmark'))
#  #lockBinding('microbenchmark', asNamespace('microbenchmark'))
#  assign_locked('microbenchmark', fun, 'microbenchmark')
#  
#  microbenchmark::microbenchmark(list=exprs, times=times, unit=unit, check=check, control=control, envir=envir)
#  
#}
