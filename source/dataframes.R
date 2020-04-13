#' Split a matrix or data frame
#'
#' Takes a matrix, data frame or any two-dimensional object (array) and splits
#' its rows according to the grouping supplied in \code{f}. Uses \code{base::split} 
#' as the workhorse, see \code{?base::split} for more details.
#
#' @param x A two-dimensional object to be split.
#' @param f A vector giving the grouping of rows
#' @param drop A logical indicating if levels that do not occur should be dropped 
#'        (if \code{f} is a factor or a list). See \code{base::split} for details.
#' @param ... Further potential arguments passed on to \code{split::base}.
#'
#' @returns A list of separated rows.
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
split_rows = function(x, f=seq_len(nrow(x)), drop=FALSE, ...) {
  lapply(base::split(x = seq_len(nrow(x)), f = f, drop = drop, ...), 
         function(indices) x[indices, , drop = FALSE])
}

#' Reverse the order of rows/colums
#'
#' Reverses the order of rows (\code{rev_rows}) or columns (\code{rev_cols})
#' of a matrix, a data frame or a 2-dimensional array.
#
#' @returns Object of the same class as the input.
#'
#' @name rev_rows
#' @family matrix/data-frame functions provided by utilbox
#' @export
rev_rows = function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x) || is.array(x) || length(dim(x))==2)
  x[nrow(x):1,,drop=FALSE]
}

#' @rdname rev_rows
#' @family matrix/data-frame functions provided by utilbox
#' @export
rev_cols = function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x) || is.array(x) || length(dim(x))==2)
  x[,ncol(x):1,drop=FALSE]
}

#' Check for identical columns
#'
#' Takes a matrix, data frame or any two-dimensional object (array) and checks
#' whether all (or a subset of) its columns are identical
#
#' @param x A two-dimensional object to be checked.
#' @param w (Optional) A vector giving a subset of columns to be considered.
#'
#' @returns Logical indicating whether all of the selected columns are identical.
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
identical_cols = function(x, w) {
  stopifnot(length(dim(x))==2)
  if(missing(w)) w = 1:NCOL(x)
  if(length(w)<=1) return(TRUE)
  all(sapply(w[-1], function(v) identical(x[,w[1],drop=TRUE], x[,v,drop=TRUE])))
}

#' Safe column addition
#'
#' Adds a column(s) into the data frame \code{x} if the column(s) 
#' is/are not present.
#'
#' @examples
#' add_col(tibble(x=1:2), y=2)
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
add_col = function(.data, ...) {
  if(!is.data.frame(.data)) error("object '.data' must be a data frame")
  ll = list(...)
  do.call('mutate', append(nlist(.data), ll[which(names(ll) %notin% names(.data))]))
}

#' Repeat data frame
#'
#' Repeat the input (\code{x}) a total of \code{n} times and bind
#' the result together using \code{bind}. Primarily intended for
#' data frames (hence the name) and matrices, but can be used for 
#' objects of another type (e.g. lists) as long as the (default 
#' or user-supplied) binding function \code{bind} is applicable 
#' to that type.
#'
#' @examples
#' rep_df(tibble(x=1:2), 3)
#' rep_df(1:3, 3)             # row-binds as a row vector
#' rep_df(t(t(1:3)), 3)       # row-binds as a column vector
#' rep_df(t(t(1:3)), 3)       # row-binds as a column vector
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
rep_df = function(x, n, fun_bind=rbind) {
  do.call(fun_bind, lapply(seq2(1,n,1), function(i) x))
}
