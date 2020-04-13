#' Replace all zero-valued elements
#'
#' Replace zeros in \code{x} with the content of \code{value}.
#'
#' @family numeric functions provided by utilbox
#' @export
unzero = function(x, value=1, zero=0.) {
  x[x==zero] = value
  x
}

#' Replace all zero-valued elements
#'
#' Replace zeros in \code{x} with the content of \code{value}.
#'
#' @family numeric functions provided by utilbox
#' @export
unspace = function(x, s='-', space='\\s+', workhorse=gsub) {
  workhorse(space, s, x)
}

#' Remove NA values
#'
#' Replaces all \code{NA} elements in its first argument (\code{x})
#' with the corresponding values in its second argument (\code{y}).
#' Same effect as \code{dplyr::coalesce}, except when \code{use_all_y}
#' is \code{TRUE}, when the \code{NA} elements in \code{x}, say there
#' are \\emph{n} of them, are replaced with the first \emph{n} elements
#' of \code{y} (with possible recycling of \code{y}).
#'
#' @examples
#' de_na(c(1,2,NA,4,5,NA), 0)
#' de_na(c(1,2,NA,4,5,NA), 1:6)
#' de_na(c(1,2,NA,4,5,NA), Inf)
#'
#' @family sequence-modifying functions provided by utilbox
#' @export
de_na = function(x, y, use_all_y=FALSE) {
  if(use_all_y) {
    x[is.na(x)] = rep(y, length.out=sum(is.na(x)))
    x
  } else {
    ifelse(!is.na(x), x, y)
  }
}

#' Bound values
#'
#' Lower- and upper-bounds the elements in `x` with `lower` and `upper`
#'
#' @examples
#' bound_between(-10:10, 0, 5)
#' bound_between(-4:5, rep(c(-1,-2),5), rep(
#'
#' @export
bound_between = function(x, lower, upper, na.rm=FALSE) {
  y = if(missing(upper)) x else pmin(x, upper, na.rm=na.rm)
  if(missing(lower)) y else pmax(y, lower, na.rm=na.rm)
}

