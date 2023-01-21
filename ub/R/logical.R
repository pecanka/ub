#' A negation function
#'
#' `not_if()` negates its first argument whenever `condition` is `TRUE`.
#' The values in `condition` get recycled (via `base::rep(..., length.out=length(x))`),
#' which means that for scalar `condition` which is `TRUE` all values in `x` are 
#' negated. For non-scalar `condition` only the corresponding elements in `x`
#' are negated. The main point of this function is for conditional implementation of 
#' of exclusion/inversion arguments (such as `inverse` in `base::grep`).
#'
#' @examples
#' not_if(TRUE, TRUE) 
#' not_if(TRUE, FALSE) 
#'
#' @export
not_if = function(x, condition) {
  ifelse(rep(condition, length.out=length(x)), !x, x)
}