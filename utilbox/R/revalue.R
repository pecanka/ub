#' @title
#' Replace values
#'
#' @description
#' `unzero()` replace all zeros in `x` with the content of `value`.
#'
#' `unspace()` replaces  all space characters (or characters matching 
#' the pattern supplied in `space`) with dashes (or the value supplied 
#' in `s`).
#'
#' `de_na()` replaces all `NA` elements in its first argument (`x`) 
#' with the corresponding values in its second argument (`y`). Same 
#' effect as \code{dplyr::coalesce}, except when `use_all_y` is `TRUE`, 
#' when the `NA` elements in `x`, say there are \\emph{n} of them, are 
#' replaced with the first \emph{n} elements of `y` (with possible 
#' recycling of `y`).
#'
#' `bound_between()` between bounds the values in `x` by `lower` from 
#' below and `upper` from above. In other words, all elements in `x` 
#' that exceed `lower` or `upper` are replaced with the bounds.
#'
#' @examples
#' unzero(c(0,0,10,10,0))
#' unzero(c(0,0,10,10,0), NA)
#'
#' unspace('this is a beautiful world.')
#' unspace('this is a beautiful world.', '_')
#' de_na(c(1,2,NA,4,5,NA), 0)
#' de_na(c(1,2,NA,4,5,NA), 1:6)
#' de_na(c(1,2,NA,4,5,NA), Inf)
#'
#' bound_between(-10:10, 0, 5)
#' bound_between(-4:5, rep(c(-1,-2),5), rep(
#'
#' @name replace_value
#' @family numeric functions provided by utilbox
#' @export
unzero = function(x, value=1, zero=0.) {
  x[x==zero] = value
  x
}

#' @rdname replace_value
#' @export
unspace = function(x, s='-', space='\\s+', workhorse=gsub) {
  workhorse(space, s, x)
}

#' @rdname replace_value
#' @export
de_na = function(x, y, use_all_y=FALSE) {
  if(use_all_y) {
    x[is.na(x)] = rep(y, length.out=sum(is.na(x)))
    x
  } else {
    ifelse(!is.na(x), x, y)
  }
}

#' @rdname replace_value
#' @export
bound_between = function(x, lower, upper, na.rm=FALSE) {
  y = if(missing(upper)) x else pmin(x, upper, na.rm=na.rm)
  if(missing(lower)) y else pmax(y, lower, na.rm=na.rm)
}

