#' @title
#' Generates all permutations
#'
#' @description
#'
#' `all_permutations()` generates all possible permutations of the 
#' elements of its argument.
#'
#' `is_permutation()` checks if the elements in the first argument 
#' (`x`) correspond to a permutation of the second argument (`y`).
#'
#' @name permutations
#' @family numeric functions provided by ub
#' @export
all_permutations = function(x) {
  llibrary(gtools)
  if(length(x)>20) 
    warn("Length of 'x' is large (",length(x),"), which gives",
         " a total of ",factorial(length(x))," permutations,",
         " and generating them will likely take very long.")
         
  ip = gtools::permutations(n=length(x), r=length(x))
  matrix(x[ip], ncol=length(x))
}

#' @rdname permutations
#' @export
is_permutation = function(x, y) {
  length(x)==length(y) &&
    isTRUE(all.equal(sort(as.vector(x)), sort(as.vector(y))))
}

