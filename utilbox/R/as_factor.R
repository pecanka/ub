#' @title
#' Convert to factor and back
#'
#' @description
#'
#' `to_factor()` converts a vector to class `factor` with levels 
#' determined by order of appearance and \strong{without} sorting of 
#' levels (unlike \code{base::as.factor}).
#'
#' `un_factor()` removes the type factor produced by 
#' `to_factor()`.
#'
#' @examples
#' to_factor(c('Germany','Italy','Czechia'))
#'
#' un_factor(to_factor(c('Germany','Italy','Czechia')))
#' un_factor(to_factor(c('Germany','Italy','Czechia')), 'numeric')
#'
#' @family sequence-related functions provided by utilbox
#' @export
to_factor = function(x, ordered=FALSE) {
  factor(groups_of_unique(x), labels=unique(x), ordered=ordered)
}

#' @rdname to_factor
#' @export
un_factor = function(x, to=c('character','numeric')) {
  to = match.arg(to)
  if(to=='character') {
    as.character(x)
  } else {
    #levels(x) = 1:nlevels(x)
    as.numeric(x)
  }
}
