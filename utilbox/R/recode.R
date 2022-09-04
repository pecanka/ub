#' Coalesce function
#'
#' A custom version of the coalesce function (to avoid dependence 
#' on other packages).
#'
#' Taken from https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r
#'
#' @examples
#' coalesce2(c(NA,'a','b',NA), letters[1:4])
#'
#' @export
coalesce2 <- function(...) {
  ans <- ..1
  for (elt in list(...)[-1]) {
      i <- is.na(ans)
      ans[i] <- elt[i]
  }
  ans
}

#coalesce2 <- function(...) {
#  Reduce(function(x, y) {
#            i <- which(is.na(x))
#            x[i] <- y[i]
#            x
#          }, list(...))
#}

#' Recode values function
#'
#' A slightly modified version of `dplyr::recode`, which takes
#' the recoding specification as a named vector.
#'
#' @examples
#' recode2(letters[1:10], c('a'='Aaron','b'='Brenda'))
#' recode2(letters, a, 'b'='BBB', .default='<none>')
#'
#' @export
recode2 = function(.x, conv, .default = NULL, .missing = NULL, .fun = dplyr::recode) {
  args = append(list(.x = .x, .default = .default, .missing = .missing), as.list(conv))
  do.call(.fun, args)
}

#recode3 = function(x, ..., .default=NULL, .missing=NULL) {
#  specs = as.list(unlist(list(...)))
#  specs = list_update(specs, nlist(.default, .missing))
#  
#  X = data.frame(x=x)
#  Z = data.frame(x=names(specs), y=specs)
#  
#  merge(X, Z, by='x', all.x=TRUE)
#  
#}
