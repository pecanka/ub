#' Convert to factor
#'
#' Convert a vector to factor with levels determined by order of
#' appearance, without sorting of levels (which \code{as.factor} 
#' does).
#'
#' @examples
#' as_factor(c('Germany','Italy','Czechia'))
#'
#' @family sequence-related functions provided by utilbox
#' @export
as_factor = function(x, ordered=FALSE) {
  factor(groups_of_unique(x), labels=unique(x), ordered=ordered)
}

#' Convert a factor to numerical
#'
#' Removes the type factor produced by \code{as_factor}.
#'
#' @examples
#' unfactor(as_factor(c('Germany','Italy','Czechia')))
#'
#' @family sequence-related functions provided by utilbox
#' @export
unname_factor = function(x) {
  levels(x) = 1:nlevels(x)
  as.numeric(x)
}

