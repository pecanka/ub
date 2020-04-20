#' @title
#' Recursive joins
#'
#' @description
#' `join_recurse()` performs a join of multiple data frames (or 
#' tibbles) supplied in a list. In order to function properly it 
#' requires a join function to be supplied. and exist. This can be for 
#' instance those found in the package `dplyr` (`left_join`, 
#' `full_join`, etc.).
#'
#' @param list A list of tibbles to be recursively joined.
#' @param ... Arguments passed on to the join function.
#' @param join The name of the join function to call (e.g. 
#' `full_join` from package `dplyr`)
#'
#' @returns The result of joining the elements of `list` recursively.
#'
#' @section Recursive joins:
#'
#' `full_join_recurse` is alias for `join_recurse` with 
#' `join=full_join`.
#'
#' `left_join_recurse` is alias for `join_recurse` with 
#' `join=left_join`.
#'
#' `right_join_recurse` is alias for `join_recurse` with 
#' `join=right_join`.
#'
#' `anti_join_recurse` is alias for `join_recurse` with 
#' `join=anti_join`.
#'
#' `semi_join_recurse` is alias for `join_recurse` with 
#' `join=semi_join`.
#'
#' `inner_join_recurse` is alias for `join_recurse` with 
#' `join=inner_join`.
#'
#' Naturally, all of these will fail if the function supplied in 
#' `join` does exists in the search path.
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
  
  if(!exists('join', mode='function'))
    error("The function '",join,"' supplied in 'join' does not exist.",
          " If you are using the defaults, remember that the package",
          " 'dplyr' needs to be installed.")
  
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
full_join_recurse = function(list, ...join='full_join') {
  join_recurse(list, ..., join=join)
}

#' @rdname join_recurse
#' @export
left_join_recurse = function(list, ...join='left_join') {
  join_recurse(list, ..., join=join)
}

#' @rdname join_recurse
#' @export
right_join_recurse = function(list, ...join='right_join') {
  join_recurse(list, ..., join=join)
}

#' @rdname join_recurse
#' @export
anti_join_recurse = function(list, ...join='anti_join') {
  join_recurse(list, ..., join=join)
}

#' @rdname join_recurse
#' @export
semi_join_recurse = function(list, ...join='semi_join') {
  join_recurse(list, ..., join=join)
}

#' @rdname join_recurse
#' @export
inner_join_recurse = function(list, ...join='inner_join') {
  join_recurse(list, ..., join=join)
}

