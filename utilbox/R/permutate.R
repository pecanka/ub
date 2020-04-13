#' Generates all permutations 
#'
#' Generates all possible permutations of the elements of its argument.
#'
#' @family numeric functions provided by utilbox
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

#' Check for permutation
#'
#' Checks if the elements in the first argument (\code{x}) correspond to 
#' a permutation of the second argument (\code{y}).
#'
#' @family check-performing functions provided by utilbox
#' @export
is_permutation = function(x, y) {
  length(x)==length(y) &&
    isTRUE(all.equal(sort(as.vector(x)), sort(as.vector(y))))
}

