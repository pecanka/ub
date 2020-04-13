#' Last non-zero element
#'
#' Find the position of the last non-zero element
#'
#' @family check-performing functions provided by utilbox
#' @export
last_nonzero = function(x) {
  length(x) - which.max(rev(x)!=0) + 1
}

