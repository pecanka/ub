#' Sort a matrix of p-values (or any matrix, really)
#'
#' Sorts the columns of a given matrix. If \code{n1} is supplied and positive,
#' the rows \code{1:n1} and \code{(n1+1):nrow(p)} are sorted separately.
#'
#' @param p Matrix of p-values to be sorted on a per-column basis.
#' @param n1 The number of top rows that are to be sorted separately
#'        from the rest of the rows.
#' @param decreasing Enable decreasing order sorting. Defaults to \code{FALSE}.
#'
#' @return An object of the same type as \code{p} with sorted columns.
#'
#' @family sorting utilities provided by utilbox
#' @export
sort_pval_matrix = function(p, n1=0, decreasing=FALSE) {

  if(n1<=0) return(apply(p, 2, sort, decreasing=decreasing))

  f = if(length(n1)==1) c(rep(1, n1), rep(2, nrow(p)-n1)) else n1
  
  unsplit(lapply(split_rows(p, f), function(r) Recall(r, decreasing=decreasing)))

}

#' Pattern sort
#'
#' Sorts input according to the order given in pattern(s) (given in 
#' \code{pattern}) by matching the names of the input (i.e. \code{x}) 
#' against the pattern(s).
#'
#' @family sorting utilities provided by utilbox
#' @export
pattern_sort = function(x, pattern, decreasing=FALSE, sort_on_names=TRUE, exclude=FALSE, get_order=FALSE) {
  
  if(is_empty(x) || is_empty(names(x))) 
    return(if(get_order) seq_along(x) else x)
  
  nam = if(sort_on_names) names(x) else x
  
  ## Find which names that match each pattern
  ordp = lapply(pattern, function(pat) 
                           which(do.call(if(exclude) `<` else `>`, 
                                 list(regexpr(pat, nam), 0))))
  
  ## Make them unique while preserving the division according to which 
  ## pattern the elements match first
  ordp = lapply(seq_along(ordp), function(i) setdiff(ordp[[i]], unlist(head(ordp, i-1))))
  
  ## Find all those that did not match any pattern
  ordn = list(setdiff(seq_along(x), unlist(ordp)))
  
  ## Determine the ordering according to names for each pattern separately
  ords = append(ordp, ordn)
  rord = lapply(ords, function(w) order(nam[w], decreasing=decreasing))
  ords = lapply(seq_along(ords), function(i) ords[[i]][rord[[i]]])

  ord = unlist(ords)

  if(get_order) ord else x[ord]
  
}

#' Name sort
#'
#' Sorts the input according to its names attribute.
#'
#' @param x named object to be sorted. If names are missing, no sorting is done.
#' @param get_order Determines whether the ordering of names or the ordered object
#'        is returned.
#'
#' @export
name_sort = function(x, get_order=FALSE) {
  if(is_empty(names(x))) return(x)
  if(get_order) order(names(x)) else x[order(names(x))]
}

#' Reorder an object
#'
#' Reorders the object in \code{x} according to the order given in \code{order}.
#'
#' @param x An object to be sorted.
#' @param order The new order of the elements of \code{x}.
#'
#' @return A reordering of \code{x} according to \code{order}.
#' 
#' @export
reord = function(x, order) { 
  y = reorder(x, order)
  attributes(y) = NULL
  y
}
