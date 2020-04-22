#' @title
#' Split a matrix or data frame
#'
#' @description
#'
#' Takes a matrix, data frame or any two-dimensional object (array) 
#' and splits its rows according to the grouping supplied in `f`. Uses 
#' \code{base::split} as the workhorse, see \code{?base::split} for more 
#' details.
#'
#' @param x A two-dimensional object to be split.
#' @param f A vector giving the grouping of rows
#' @param drop A logical indicating if levels that do not occur 
#' should be dropped (if `f` is a factor or a list). See 
#' \code{base::split} for details.
#' @param ... Further potential arguments passed on to 
#' \code{split::base}.
#'
#' @returns A list of separated rows.
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
split_rows = function(x, f=seq_len(nrow(x)), drop=FALSE, ...) {

  lapply(base::split(x = seq_len(nrow(x)), f = f, drop = drop, ...), 
         function(indices) x[indices, , drop = FALSE])
         
}

#' @title Reverse the order of rows/colums
#'
#' @description
#'`rev_rows()` reverses the order of rows of a matrix, 
#' a data frame or a 2-dimensional array.
#'
#' `rev_cols()` reverses the order of columns.
#'
#' @returns Object of the same class as the input.
#'
#' @examples
#' rev_rows(data.frame(x=1:10, y=letters[1:10]))
#' rev_cols(data.frame(x=1:10, y=letters[1:10]))
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
rev_rows = function(x) {

  stopifnot(is.matrix(x) || is.data.frame(x) || is.array(x) || length(dim(x))==2)
  
  x[nrow(x):1,,drop=FALSE]
  
}

#' @rdname rev_rows
#' @export
rev_cols = function(x) {

  stopifnot(is.matrix(x) || is.data.frame(x) || is.array(x) || length(dim(x))==2)
  
  x[,ncol(x):1,drop=FALSE]
  
}

#' @title Check for identical columns
#'
#' @description
#'`identical_cols()` takes a matrix, data frame or any 
#' two-dimensional object (array) and checks whether all (or a subset 
#' of, when `w` given) its columns are identical in terms of content, 
#' not necessarily in terms of equality of column names.
#'
#' @title
#' @param x A two-dimensional object to be checked. @description
#'
#' @param w (Optional) A vector giving a subset of columns to be 
#' considered.
#'
#' @returns Logical indicating whether all of the selected columns 
#' are identical.
#'
#' @examples
#' identical_cols(data.frame(x=1:10, y=letters[1:10]))
#' identical_cols(data.frame(x=1:10, y=1:10))
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
identical_cols = function(x, w) {

  stopifnot(length(dim(x))==2)

  if(missing(w)) w = 1:NCOL(x)

  if(length(w)<=1) return(TRUE)

  all(sapply(w[-1], function(v) identical(x[,w[1],drop=TRUE], x[,v,drop=TRUE])))
  
}

#' @title Safe column addition
#'
#' @description
#'`add_col()` adds a column(s) into the data frame `x` 
#' if the column(s) is/are not present. The way it is written is 
#' intended to work both for standard `data.frame` and other 
#' `data.frame` derived classes (such as `dplyr::tibble()`) provided 
#' the derived class has the method `[<-` defined.
#'
#' @examples
#' # works for data.frames and tibbles
#' x = data.frame(id=1:2)
#' add_col(x, name=c('John','Jeff'), age=c(20,40))
#' #add_col(as_tibble(x), name=c('John','Jeff'), age=c(20,40))
#'
#' @family matrix/data-frame functions provided by utilbox
#' @export
add_col = function(.data, ...) {
  
  if(!is.data.frame(.data)) 
    error("object '.data' must be a data frame")
  
  args = dots_to_nlist()
  
  new_cols = args[which(names(args) %notin% names(.data))]
  
  `[<-`(.data, names(new_cols), value=new_cols)
  
}

#' @title Repeat data frame
#'
#' @description
#'`rep_df()` repeats the input (`x`) a total of `n` 
#' times and bind the result together using the function specified in 
#' `bind`. Primarily intended for data frames (hence the name) and 
#' matrices, but can be used for objects of another type (e.g. lists) as 
#' long as the (default or user-supplied) binding function `bind` is 
#' applicable to that type.
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
