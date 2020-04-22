#' Copy attributes from one object to another
#'
#' Copy specified attributes from one object to another. The specification
#' can be done either by name or by pattern.
#'
#' @examples
#' x = structure(999, 'name_first'='john', name_second='doe')
#' y = 100
#' copy_attributes(y, x, 'name_first')
#'
#' @export
copy_attributes = function(to, from, ..., pattern=FALSE, fixed=FALSE) {
  
  attribs = unlist(dots_to_nlist())
  
  if(pattern) {
    attribs = filter_by_call(names(attributes(from)), ~`%m_any%`(attribs, .x, fixed=fixed))
  }
  
  for(a in attribs) {
    to = `attr<-`(to, a, attr(from,a))
  }
  
  to
  
}