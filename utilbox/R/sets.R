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
#' `is_subset()` checks whether `x` is a subset of `y` and 
#' `is_superset()` checks whether `x` is a superset of `y`, or equivalently
#' the `y` is a subset of `x`.
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
  set = sets[1][[1]]
  for(i in seq_along(sets)[-1]) {
    set = intersect(set, sets[[i]])
  }
  set
}

#' @rdname sets
#' @export
setdiffsym = function(x, y, labels=c('x','y'), report_counts=TRUE) {
  
  dxy = base::setdiff(x,y)
  dyx = base::setdiff(y,x)
  
  tdi = ' (#difference|#intersect|#total)'
  Counts = c(
    paste0('x: ', paste(length(dxy), length(x)-length(dxy), length(x), sep='|'), tdi),
    paste0('y: ', paste(length(dyx), length(y)-length(dyx), length(y), sep='|'), tdi)
  )
     
  structure(list(dxy, dyx), names=labels, Counts=Counts)
  
}

#' @rdname sets
#' @export
is_subset = function(x, y) {
  all(x %in% y)
}

#' @rdname sets
#' @export
is_superset = function(x, y) {
  all(y %in% x)
}

#' Generate all subsets
#'
#' `all_subsets()` returns a list of subsets of the given set (supplied via `set`) 
#' (of maximum size `max_size`) together with presence indicator for each 
#' value in `set`.
#'
#' `all_subsets_fast()` is a faster version that uses the package 'combinat',
#' which does not produce the presence indicators.
#'
#' @export
all_subsets = function(set, max_size=Inf, use_varnames_in_status=TRUE, stringsAsFactors=FALSE) {

  set = unique(set)

  n = length(set)

  status = expand.grid(rep(list(c(FALSE, TRUE)), n), KEEP.OUT.ATTRS=FALSE, stringsAsFactors=stringsAsFactors)

  if(use_varnames_in_status) {
    names(status) = set
  }

  if(max_size < n) {
    status = status[apply(status, 1, sum) <= max_size,]
  }

  subsets = apply(status, 1, function(x) names(x)[x])

  list(set=set, subsets=subsets, status=status)
  
}

#' @export
all_subsets_fast = function(set) {
  check_namespace('conbinat')
  unlist(lapply(seq_along(set), combinat::combn, x=set, simplify=FALSE), recursive=FALSE)
}

