#' @title
#' Recursive joins
#'
#' @description
#'
#' `join_recurse()` performs a join of multiple data frames (or 
#' tibbles) supplied in a list. In order to function properly it 
#' requires a join function to be supplied and to exist. This can be for 
#' instance those found in the package `dplyr` (`left_join`, 
#' `full_join`, etc.).
#'
#' @param list A list of tibbles to be recursively joined.
#' @param ... Arguments passed on to the join function.
#' @param join_fun The name of the join function to call (e.g. 
#' `full_join` from package `dplyr`)
#'
#' @returns The result of joining the elements of `list` recursively.
#'
#' @section Recursive joins:
#'
#' `full_join_recurse` is alias for `join_recurse` with 
#' `join_fun=full_join`.
#'
#' `left_join_recurse` is alias for `join_recurse` with 
#' `join_fun=left_join`.
#'
#' `right_join_recurse` is alias for `join_recurse` with 
#' `join_fun=right_join`.
#'
#' `anti_join_recurse` is alias for `join_recurse` with 
#' `join_fun=anti_join`.
#'
#' `semi_join_recurse` is alias for `join_recurse` with 
#' `join_fun=semi_join`.
#'
#' `inner_join_recurse` is alias for `join_recurse` with 
#' `join_fun=inner_join`.
#'
#' Naturally, all of these will fail if the function supplied in 
#' `join_fun` does exists in the search path.
#'
#' @examples
#' a = tibble(day=c('Mon','Tue','Wed'), a=1:3)
#' b = tibble(day=c('Tue','Wed'), b=3:4)
#' c = tibble(day=c('Mon','Wed'), c=5:6)
#' join_recurse(list(a, b, c), by='day', join_fun=full_join)
#'
#' @name join_recurse
#' @family matrix/data-frame functions provided by utilbox
#' @export
join_recurse = function(list, ..., join_fun) {

  stopifnot(is.list(list))
  
  if(!is.function(join_fun) && !exists(join_fun, mode='function'))
    error("The function '",join_fun,"' supplied in 'join_fun' does not exist.",
          " If you are using the defaults, remember that the package",
          " 'dplyr' needs to be installed.")
  
  if(length(list)<=1) unlist(list)
  
  y = do.call(join_fun, append(unname(head(list, 2)), list(...)))
  
  if(length(list)==2) {
    y 
  } else {
    Recall(append(list(y), tail(list, -2)), ..., join_fun=join_fun)
  }
  
}

#' @rdname join_recurse
#' @export
full_join_recurse = function(list, ..., join_fun='full_join') {
  join_recurse(list, ..., join_fun=join_fun)
}

#' @rdname join_recurse
#' @export
left_join_recurse = function(list, ..., join_fun='left_join') {
  join_recurse(list, ..., join_fun=join_fun)
}

#' @rdname join_recurse
#' @export
right_join_recurse = function(list, ..., join_fun='right_join') {
  join_recurse(list, ..., join_fun=join_fun)
}

#' @rdname join_recurse
#' @export
anti_join_recurse = function(list, ..., join_fun='anti_join') {
  join_recurse(list, ..., join_fun=join_fun)
}

#' @rdname join_recurse
#' @export
semi_join_recurse = function(list, ..., join_fun='semi_join') {
  join_recurse(list, ..., join_fun=join_fun)
}

#' @rdname join_recurse
#' @export
inner_join_recurse = function(list, ..., join_fun='inner_join') {
  join_recurse(list, ..., join_fun=join_fun)
}

#' @title
#' Symmetric anti-join
#'
#' @description
#'
#' `anti_join_sym()` performs two anti-joins on the two data frames (or 
#' tibbles) supplied to it as `x` and `y`. It returns a list of length
#' 2 with `anti_join(x, y, ...)` and `anti_join(y, x, ...)` as it elements.
#' By default, it depends on the existence of a function `anti_join()`, but
#' an alternative function name can be supplied via `join_fun`.
#'
#' @examples
#' x = data.frame(a=1:3, b=letters[1:3], stringsAsFactors=FALSE)
#' y = data.frame(a=0:2, b=c('z',letters[1:2]), stringsAsFactors=FALSE)
#' anti_join_sym(x, y)
#'
#' @name anti_join_sym
#' @family matrix/data-frame functions provided by utilbox
#' @export
anti_join_sym = function(x, y, ..., join_fun='anti_join') {

  if(!is.function(join_fun) && !exists(join_fun, mode='function'))
    error("The function '",join_fun,"' supplied in 'join_fun' to 'anti_join_sym' does not exist.",
          " If you are using the defaults, remember that the package",
          " 'dplyr' needs to be installed.")
          
  A1 = do.call(join_fun, list(x, y, ...))
  A2 = do.call(join_fun, list(y, x, ...))
  
  list(x_antijoin_y=A1, y_antijoin_x=A2)

}
