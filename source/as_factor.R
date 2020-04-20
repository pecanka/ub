#' @title
#' Convert to factor and back
#'
#' @description
#' `as_factor()` converts a vector to class `factor` with levels 
#' determined by order of appearance and \strong{without} sorting of 
#' levels (unlike \code{base::as.factor}).
#'
#' `unname_factor()` removes the type factor produced by 
#' `as_factor()`.
#'
#' @examples
#' as_factor(c('Germany','Italy','Czechia'))
#'
#' unfactor(as_factor(c('Germany','Italy','Czechia')))
#'
#' @family sequence-related functions provided by utilbox
#' @export
as_factor = function(x, ordered=FALSE) {
  factor(groups_of_unique(x), labels=unique(x), ordered=ordered)
}

#' @rdname as_factor
#' @export
unname_factor = function(x) {
  levels(x) = 1:nlevels(x)
  as.numeric(x)
}

