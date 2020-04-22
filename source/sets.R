#' @title
#' Sets
#'
#' @description
#'
#' `mintersect()` finds the intersection of multiple sets.
#'
#' `setdiffsym()` determines the \"symmetric difference\" of the 
#' supplied sets, which is all of the elements that are outside the 
#' intersection of the supplied sets.
#'
#' `is_subset()` checks whether `x` is a subset of `y`. 
#' `is_subset2()` does the same thing.
#'
#' @examples
#' setdiffsym(1:5,3:7)
#' setdiffsym(1:5,3:7, 'vector')
#'
#' @name sets
#' @family numeric functions provided by utilbox
#' @export
mintersect = function(...) {
  sets = list(...)
  set = sets[[1]]
  for(i in seq_along(sets)[-1]) set = intersect(set, sets[[i]])
  return(set)
}

#' @rdname sets
#' @export
setdiffsym = function(x, y, labels=c('x','y')) {
  `names<-`(list(base::setdiff(x,y), base::setdiff(y,x)), labels)
}

#' @rdname sets
#' @export
is_subset = function(x, y) {
  all(x %in% y)
}

#' @rdname sets
#' @export
is_subset2 = function(x, y) {
  is_empty(setdiff(x, y))
}
