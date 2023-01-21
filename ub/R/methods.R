#' Method dispatch sequence
#'
#' `method_dispatch_sequence` is just an alias for `sloop::s3_dispatch` with
#' an easier to remember name.
#'
#' @examples
#' method_dispatch_sequence(print(1))
#'
#' @export
method_dispatch_sequence = function(..., envir=parent.frame()) {
  check_namespace('sloop')
  sloop::s3_dispatch(..., env=envir)
}
