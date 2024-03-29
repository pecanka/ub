#' @title
#' Group belonging indices
#'
#' @description
#'
#' For each element in `x`, `groups_of_unique()` determines which 
#' group (first group, second group, etc.) the element belongs to and 
#' returns the group indices. The groups are determined based on the 
#' order of appearance of unique elements in `x`.
#'
#' This is basically an alias for `replace3` with `new_values` equal 
#' to \code{1:nunique(x)}.
#'
#' @examples
#' groups_of_unique(c('b','b','aa', 'a','b','c','a'))
#'
#' @family sequence-related functions provided by ub
#' @export
groups_of_unique = function(x, format=as.numeric) {
  replace3(x, format=format)
}

#' @title
#' Split an object into groups
#'
#' @description
#'
#' Split the elements of an object into groups (referred to as 
#' \"bags\") of given size.
#'
#' @returns List with the i-th element containing the elements that 
#' were assigned into the i-th group (\"bag\")
#'
#' @examples
#' split_into_bags(1:22, 7)
#'
#' @family sequence-related functions provided by ub
#' @export
split_into_groups = function(x, n) {

  if(missing(n))
    stop("Supply the number of groups to divide `x` into.")
  
  tapply(x, (seq_along(x) - 1) %/% max(1,ceiling(n)) + 1, `[`, simplify=FALSE)
  
}

#' @title
#' Virtually bag the elements of `x`
#'
#' @description
#'
#' "Puts" the elements of `x` into virtual bags each of size 
#' `bagsize`, starting from bag labelled \"1\", then bag labeled \"2\", 
#' and so on, and returns which bag each element ended up in.
#'
#' @examples
#' what_bag(1:40)
#' what_bag(1:90, 25)
#'
#' @family sequence-related functions provided by ub
#' @export
what_bag = function(x, bagsize=20) {
  if(is_empty(x)) return(x)
  (1:length(x)-1) %/% bagsize + 1
}

