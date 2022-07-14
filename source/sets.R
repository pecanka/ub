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
setdiffsym = function(x, y, labels, report_counts=TRUE) {
  
  if(missing(labels)) {
    labels = c('x','y')
    #labels = unlist(argvars_to_nlist())
  }
  
  dxy = base::setdiff(x,y)
  dyx = base::setdiff(y,x)
  
  tdi = ' (#difference|#intersect|#total)'
  Counts =
    c('x: '%p%collapse0(c(length(dxy),length(x)-length(dxy),length(x)),sep='|'),
      'y: '%p%collapse0(c(length(dyx),length(y)-length(dyx),length(y)),sep='|'))%p%tdi
                
  structure(`names<-`(list(dxy, dyx), labels), Counts=Counts)
  
}

#' @rdname sets
#' @export
is_subset = function(x, y) {
  all(x %in% y)
}

#' Generate all subsets
#'
#'
#'
#' @export
all_subsets = function(set, use_varnames_in_status=TRUE, stringsAsFactors=FALSE) {
  n = length(set)
  status = expand.grid(rep(list(c(FALSE, TRUE)), n), KEEP.OUT.ATTRS=FALSE, stringsAsFactors=stringsAsFactors)
  if(use_varnames_in_status) names(status) = set
  subsets = apply(status, 1, function(x) names(x)[x])
  list(set=set, subsets=subsets, status=status)
}