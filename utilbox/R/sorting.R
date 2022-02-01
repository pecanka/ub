#' @title
#' Sort a matrix (of p-values)
#'
#' @description
#'
#' `sort_pval_matrix_cols()` sorts the columns of a given matrix. This is 
#' primarily intended for sorting matrices containing p-values with each 
#' column corresponding to a single sample. If `n1` is 
#' supplied and positive, the rows \code{1:n1} and \code{(n1+1):nrow(p)} 
#' are sorted separately.
#'
#' @param p Matrix of p-values to be sorted on a per-column basis.
#' @param n1 The number of top rows that are to be sorted separately 
#' from the rest of the rows.
#' @param decreasing Enable decreasing order sorting. Defaults to 
#' `FALSE`.
#'
#' @return An object of the same type as `p` with sorted columns.
#'
#' @family sorting utilities provided by utilbox
#' @export
sort_pval_matrix_cols = function(x, n1=0, decreasing=FALSE) {

  if(n1<=0) return(apply(x, 2, sort, decreasing=decreasing))

  f = if(length(n1)==1) c(rep(1, n1), rep(2, nrow(x)-n1)) else n1
  
  unsplit(lapply(split_rows(p, f), function(r) Recall(r, decreasing=decreasing)))

}

#' @title
#' Sort a data frame
#'
#' @description
#'
#' `sort_df()` sorts the columns of a given data frame, matrix, or any 
#' two-dimensional array according to the supplied columns.
#'
#' @param x data frame/matrix/array to be sorted on a per-column basis.
#' @param ... names of colums to sort by (symbolic).
#' @param decreasing Enable decreasing order sorting. Defaults to 
#' `FALSE`.
#'
#' @returns An object of the same type as `p` with sorted columns.
#'
#' @examples
#' x = data.frame(A = c(3,2,3,4,4,4,2,2), B = c('d','e','a','r','t','m','f','a2'), C=8:1)
#' # sort on only one column
#' sort_df(x, A)
#' sort_df(x, C)      # effectively, reverse rows
#' # sort on multiple columns
#' sort_df(x, A, B)
#'
#' @family sorting utilities provided by utilbox
#' @export
sort_df = function(x, ..., decreasing=FALSE) {

  vars = as.character(dots_to_nlist(keep_symbolic=TRUE))
  args = unname(as.list(x[vars])) %||||% list(1:nrow(x))
  ord = do.call('order', args %append% list(decreasing=decreasing))
  x[ord,]
  
}

#' @title
#' Pattern sort
#'
#' @description
#'
#' `sort_by_pattern()` sorts the input according to the order given in 
#' pattern(s) (given in `pattern`) by matching the names of the input 
#' (i.e. `x`) against the pattern(s).
#'
#' `sort_by_names()` sorts the input (`x`) according to its `names` 
#' attribute by exact matching. For sorting by names via pattern matching
#' use `sort_by_pattern(..., by_names=TRUE)`.
#'
#' @param x named object to be sorted. If names are missing, no 
#' sorting is done.
#'
#' @param get_order Determines whether the ordering of names or the 
#' ordered object is returned.
#'
#' @param x A vector to be sorted.
#'
#' @param pattern A regular expression pattern (or vector of patterns)
#' to sort `x` by.
#'
#' @param by_names Logical. When `TRUE` (default for `sort_by_names()`), 
#' `x` is sorted based on its its `names` attribute, otherwise it is sorted 
#' based on its values (default for `sort_by_pattern`).
#'
#' @param invert Logical. Sets whether the pattern matching is inverted
#' (in `base::grepl`).
#'
#' @param get_order Logical. Sets whether the reorder `x` (`FALSE`) or the
#' reordering indices (`TRUE`) are returned.
#'
#' @param keep_order Logical. Determines whether the groups of elements
#' determined by matching patterns get themselves sorted or whether the
#' original order of elements is kept within these groups.
#'
#' @param allow_empty_names Logical. Sets whether (any) missing names 
#' cause an error (effective only with `by_names=TRUE`).
#'
#' `reord` simply reorders an object according to reordering indices supplied
#' to it via `order`.
#'
#' @param x An object to be sorted.
#'
#' @param order The new order of the elements of `x`.
#'
#' @returns A reordering of `x` according to `order`.
#'
#' @examples
#' x = c('jan','jany','jani','jana','dariel','dan','danny')
#'
#' # put the "d" names up front, sort them and the rest separately
#' sort_by_pattern(x, 'd')
#'
#' # put the "d" names up front, leave the order alone
#' sort_by_pattern(x, 'd')
#' sort_by_pattern(x, c('d','dar'))
#' sort_by_pattern(x, c('d','dar','y'))
#'
#' # similarly to base::order(), we can get the reordering indices
#' y = sort_by_pattern(x, c('d','nn','dar','y'), invert=TRUE)
#' ord = sort_by_pattern(x, c('d','nn','dar','y'), invert=TRUE, get_order=TRUE)
#' identical(y, x[ord])
#'
#' # without the inversion
#' y = sort_by_pattern(x, c('d','nn','dar','y'))
#' ord = sort_by_pattern(x, c('d','nn','dar','y'), get_order=TRUE)
#' identical(y, x[ord])
#'
#' # exact sorting by names
#' z = c('name_last'='Newman', 'name_given'='Richard', 'date_of_death'='2003-02-03', 'date_of_birth'='1933-12-30')
#' sort_by_names(z)
#'
#' # pattern-sorting by names
#' sort_by_pattern(z, c('name_', 'date_'), by_names=TRUE)
#'
#' @family sorting utilities provided by utilbox
#' @export
sort_by_pattern = function(x, pattern, by_names=FALSE, invert=FALSE, get_order=FALSE,
  keep_order_in_groups=FALSE, allow_empty_names=FALSE, ...) {
  
  if(missing(pattern))
    error('Supply a pattern via `pattern`.')
  
  if(is_empty(x)) {
    return(if(get_order) seq_along(x) else x)
  }
  
  if(!allow_empty_names && by_names) {
    if(is_empty(names(x)))
      error('`x` cannot be sorted according to its `names` when `x` has no names.')
    if(str_is_empty_any(names(x)))
      error('Some elements of `x` have no names. If you wish to proceed, you can disable',
            ' this check via `allow_empty_names=TRUE`')
  }
    
  if(by_names && is_empty(names(x))) {
    return(if(get_order) seq_along(x) else x)
  }
    
  # choose what the sort will be based on (values or names)
  y = if(by_names) names(x) else x
  
  res = order_by_pattern2(y, seq_along(x), pattern, invert, keep_order_in_groups)
  
  if(get_order) res$ord else x[res$ord]
  
}

#' @rdname sort_by_pattern
#' @export
sort_by_names = function(x, get_order=FALSE) {
  
  if(is_empty(names(x))) {
    return(x)
  }
  
  if(get_order) order(names(x)) else x[order(names(x))]
  
}

#'
#' Ordering by multiple variables and regular pattern matching
#'
#' `order_by_pattern` returns the order when sorting by multiple variables supplied
#' via `...`. The sorting by the variables in `...` (all but the last) is done in a
#' standard way (i.e. equivalently to a call to `base::order`), while the sorting by
#' the last variable in `...` is done based on regualar pattern matching of `x` against
#' the pattern(s) supplied in `pattern`. Multiple patterns can be supplied to `pattern` and the
#' matching is done in a cascading way, meaning that the elements that match the first
#' element in `pattern` are returned first, then those matching the second element in `pattern`,
#' and so on. Optionally, arguments to the internal call to `grepl` and `order` can be
#' supplied (as lists) via `args_grepl` and `args_order`, respectively.
#'
#' @examples
#' # define data
#' people = c('Jake','John','Mark','Rogen','Mary','David','Betty')
#' sex = c('m','m','m','m','f','m','f')
#' pattern = c('a','b','r','o')
#'
#' # order by an all-matching pattern (i.e. no reordering)
#' people[order_by_pattern(people)]
#'
#' # order by the patterns
#' people[order_by_pattern(people, pattern=pattern)]
#'
#' # order first by sex and then by the patterns
#' people[order_by_pattern(sex, people, pattern=pattern)]
#'
#' # order first by sex and then by the patterns (case-insensitive)
#' people[order_by_pattern(sex, people, pattern=pattern, args_grepl=list(ignore.case=TRUE))]
#'
#' @export
order_by_pattern = function(..., pattern='.', args_grepl=list(), args_order=list()) {
  cols = list(...)
  colp = tail(cols, 1)[[1]]
  cols = head(cols, -1)
  is_match = lapply(c(pattern, '.'), function(patrn) -do.call(grepl, append(list(patrn, colp), args_grepl)))
  by = append(append(cols, is_match), args_order)
  do.call(order, by)
}

#' @rdname order_by_pattern
#' @export
order_by_pattern2 = function(x, ord, pattern, invert=FALSE, keep_order=FALSE, ...) {
  
  if(is_empty(x)) {
    return(nlist(y=x, ord=seq_along(x)))
  }
    
  # simply sort when no pattern supplied (such as is the case for 
  # the last recursive call after exhausting all pattern)
  if(is_empty(pattern)) {
    if(keep_order) {
      return(nlist(y=x, ord))
    } else {
      y_ord = order(x)
      return(nlist(y=x[y_ord], ord=ord[y_ord]))
    }
  }
  
  is_match = not_if(grepl(pattern[1], x, ...), invert)
  
  x1 = x[is_match]
  x2 = x[!is_match]
  ord1 = ord[is_match]
  ord2 = ord[!is_match]
  
  args = nlist(pattern=pattern[-1], invert, keep_order, ...)
  
  y1 = do.call(Recall, list(x1, ord1) %append% args)
  y2 = do.call(Recall, list(x2, ord2) %append% args)

  list(y=c(y1$y, y2$y), order=c(y1$ord, y2$ord))
  
}

sort_by_pattern_v2 = function(x, pattern, sort_by_names=FALSE, invert=FALSE, get_order=FALSE,
  keep_order_within_pattern_groups=FALSE, allow_empty_names=FALSE, ...) {
  
  if(missing(pattern))
    error('Supply a pattern via `pattern`.')
  
  if(!allow_empty_names && sort_by_names && is_empty(names(x)))
    error('`x` cannot be sorted according to its `names` when `x` has no names.')
    
  if(is_empty(x) || (sort_by_names && is_empty(names(x)))) {
    return(if(get_order) seq_along(x) else x)
  }
    
  # choose what the sort will be based on (values or names)
  if(sort_by_names) {
    y = names(x)
    get_ord = TRUE
  } else {
    y = x
    get_ord = get_order
  }
  
  # simply sort when no pattern supplied (such as is the case for 
  # the last recursive call after exhausting all pattern)
  if(is_empty(pattern)) {
    return(if(keep_order_within_pattern_groups) x else x[order(y)])
  }
  
  is_match = not_if(grepl(pattern[1], y, ...), invert)
  
  y1 = y[is_match]
  y2 = y[!is_match]
  
  args = nlist(pattern=pattern[-1], sort_by_names, invert, get_ord, keep_order_within_pattern_groups, ...)
  
  y1 = do.call(Recall, list(y1) %append% args)
  y2 = do.call(Recall, list(y2) %append% args)

  c(y1, y2)
  
}

sort_by_pattern_old = function(x, pattern, decreasing=FALSE, by_names=TRUE, exclude=FALSE, 
  get_order=FALSE) {
  
  if(is_empty(x) || (by_names && is_empty(names(x))))
    return(if(get_order) seq_along(x) else x)
  
  nam = if(by_names) names(x) else x
  
  ## Find which names that match each pattern
  ordp = lapply(pattern, grep, nam, invert=exclude)
  
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

#' @rdname sort_by_pattern
#' @export
reord = function(x, order) { 
  y = reorder(x, order)
  attributes(y) = NULL
  y
}
