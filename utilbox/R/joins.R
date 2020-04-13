#' Recursive joins
#'
#' `join_recurse` Performs a join of multiple tibbles supplied in a list. 
#' In order to function properly it requires a join function to be supplied.
#' and exist. This can be for instance those found in the package 
#' \code{dplyr} (\code{left_join}, \code{full_join}, etc.).
#'
#' @param list A list of tibbles to be recursively joined.
#' @param ... Arguments passed on to the join function.
#' @param join The name of the join function to call (e.g. \code{full_join}
#'        from package \code{dplyr})
#'
#' @returns The result of joining the elements of \code{list} recursively.
#'
#' @section Recursive joins:
#'
#' `full_join_recurse` is alias for `join_recurse` with `join` set
#' to `full_join`. 
#'
#' `left_join_recurse` is alias for `join_recurse` with `join` set
#' to `left_join`. 
#'
#' `right_join_recurse` is alias for `join_recurse` with `join` set
#' to `right_join`. 
#'
#' `inner_join_recurse` is alias for `join_recurse` with `join` set
#' to `inner_join`. 
#'
#' `semi_join_recurse` is alias for `join_recurse` with `join` set
#' to `semi_join`. 
#'
#' Naturally, all of these will fail if the function supplied in `join` 
#' does exists in the search path.
#'
#' @examples
#' a = tibble(day=c('Mon','Tue','Wed'), a=1:3)
#' b = tibble(day=c('Tue','Wed'), b=3:4)
#' c = tibble(day=c('Mon','Wed'), c=5:6)
#' join_recurse(list(a, b, c), by='day', join=full_join)
#'
#' @name join_recurse
#' @family matrix/data-frame functions provided by utilbox
#' @export
join_recurse = function(list, ..., join) {

  stopifnot(is.list(list))
  
  if(length(list)<=1) unlist(list)
  
  y = do.call(join, append(unname(head(list, 2)), list(...)))
  
  if(length(list)==2) {
    y 
  } else {
    Recall(append(list(y), tail(list, -2)), ..., join=join)
  }
  
}

#' @rdname join_recurse
#' @export
full_join_recurse = hijack(join_recurse, join='full_join')

#' @rdname join_recurse
#' @export
left_join_recurse = hijack(join_recurse, join='left_join')

#' @rdname join_recurse
#' @export
right_join_recurse = hijack(join_recurse, join='right_join')

#' @rdname join_recurse
#' @export
inner_join_recurse = hijack(join_recurse, join='inner_join')

#' @rdname join_recurse
#' @export
semi_join_recurse = hijack(join_recurse, join='semi_join')
