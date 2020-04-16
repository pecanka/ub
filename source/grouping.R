#' Group belonging indices
#'
#' For each element in \code{x}, it determines which group (first 
#' group, second group, etc.) the element belongs to and returns 
#' the group indices. The groups are determined based on the order 
#' of appearance of unique elements in \code{x}.
#'
#' This is basically an alias for \code{replace3} with \code{new_values}
#' equal to \code{1:nunique(x)}.
#'
#' @examples
#' groups_of_unique(c('b','b','aa' 'a','b','c','a'))
#'
#' @family sequence-related functions provided by utilbox
#' @export
groups_of_unique = function(x, format=as.numeric) {
  replace3(x, format=format)
}

#' Split an object into groups
#'
#' Split the elements of an object into groups (referred to as 
#' \"bags\") of given size.
#'
#' @returns List with the i-th element containing the elements that
#'          were assigned into the i-th group (\"bag\")
#'
#' @examples
#' split_into_bags(1:22, 7)
#'
#' @family sequence-related functions provided by utilbox
#' @export
split_into_groups = function(x, n) {
  tapply(x, (seq_along(x) - 1) %/% max(1,ceiling(n)) + 1, `[`, simplify=FALSE)
}

#' Virtually bag the elements of \code{x}
#'
#' "Puts" the elements of \code{x} into virtual bags each of size 
#' \code{bagsize}, starting from bag labelled \"1\", then bag 
#' labeled \"2\", and so on, and returns which bag each element 
#' ended up in.
#'
#' @examples
#' what_bag(1:40)
#' what_bag(1:90, 25)
#'
#' @family sequence-related functions provided by utilbox
#' @export
what_bag = function(x, bagsize=20) {
  if(is_empty(x)) return(x)
  (1:length(x)-1) %/% bagsize + 1
}

