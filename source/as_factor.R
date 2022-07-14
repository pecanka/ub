#' @title
#' Convert to factor and back
#'
#' @description
#'
#' `to_factor()` converts a vector to class `factor` with levels 
#' determined by order of appearance and \strong{without} sorting of 
#' levels (unlike \code{base::as.factor}). It also completes the
#' levels to contain all unique values in `x`, which is useful when
#' one wants to only supply the first few values and let the rest be
#' taken from `x`. The rest of the values are sorted by default, but
#' this can be changed via `fun` (e.g., `fun=identity` to keep the
#' order of appearance). The completion of levels can be useful for
#' instance when using `to_factor` instead of factor when sorting
#' a data set (see the examples with `dplyr::arrange` below).
#'
#' `un_factor()` removes the type factor produced by `to_factor()`.
#'
#' @examples
#' to_factor(c('Germany','Italy','Czechia'))
#'
#' un_factor(to_factor(c('Germany','Italy','Czechia')))
#' un_factor(to_factor(c('Germany','Italy','Czechia')), 'numeric')
#'
#' # sorting (both achieve the same, to_factor is more succinct)
#' dplyr::arrange(mtcars, to_factor(carb, 3:2))
#' dplyr::arrange(mtcars, factor(carb, union(3:2, sort(carb))))
#'
#' @family sequence-related functions provided by utilbox
#' @export
to_factor = function(x = character(), levels=unique(x), labels=levels, exclude=NA, ordered=FALSE, nmax=NA, fun=sort) {
  levels = union(levels, fun(unique(x)))
  if(missing(labels)) labels = levels
  factor(x, levels=levels, labels=labels, exclude=exclude, ordered=ordered, nmax=nmax)
}

#' @rdname to_factor
#' @export
un_factor = function(x, to=c('character','numeric')) {
  to = match.arg(to)
  if(to == 'character') {
    as.character(x)
  } else {
    #levels(x) = 1:nlevels(x)
    as.numeric(x)
  }
}
