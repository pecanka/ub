#' @title
#' First and last elements
#'
#' @description
#'
#' Get the first or last `n` elements of an object.

#' @title
#' The two function are similar to, and relying on,
#' @description
#'
#' \code{utils::head()} and \code{utils::tail()} with two main 
#' differences. First, `h1()` and `t1()` have different default number 
#' of elements (\code{n=1}). Second, if the requested number of elements 
#' (`n`) is larger than the length of the input (`x`), the elements of 
#' `x` are recycled, though only for `n` positive. For non-positive `n`, 
#' and for classes different from those for which the methods are 
#' defined, the functions behave exactly like their counterparts from 
#' `utils`.
#'
#' @examples
#' # head
#' h1(LETTER[1:7])
#' h1(LETTER[1:7], 10)
#' h1(data.frame(id=1:3, name=c('John','Jane','Paul')))
#' h1(data.frame(id=1:3, name=c('John','Jane','Paul')), 5)
#'
#' # tail
#' t1(LETTER[1:7])
#' t1(LETTER[1:7], 10)
#' t1(data.frame(id=1:3, name=c('John','Jane','Paul')))
#'
#' @name h1
#' @family sequence-related functions provided by ub
#' @export
h1 = function(x, ...) {
  if(is_empty(x)) return(x)
  UseMethod('h1')
}

#' @rdname h1
#' @export
h1.vector = function(x, n=1, ..., stop_on_greedy=FALSE) {
  if(stop_on_greedy && (n < -length(x) || n > length(x))) {
    stop("Asking for non-existing elements.")
  } else if(n<=0) {
    utils::head(x, n=n, ...) 
  } else {
    utils::head(rep_len(x, length.out=n), n=n, ...) 
  }
}

#' @rdname h1
#' @export
h1.data.frame = function(x, n=1, ..., stop_on_greedy=FALSE) {
  if(stop_on_greedy && (n < -nrow(x) || n > nrow(x))) {
    stop("Asking for non-existing elements.")
  } else if(n<=0) {
    utils::head(x, n=n, ...) 
  } else {
    utils::head(rep_df(x, ceiling(n/nrow(x))), n=n, ...)
  }
}

#' @rdname h1
#' @export
h1.default = function(x, n=1, ..., stop_on_greedy=FALSE) {
  if(stop_on_greedy && (n < -length(x) || n > length(x))) {
    stop("Asking for non-existing elements.")
  } else {
    utils::head(x, n=n, ...)
  }
}

#' @rdname h1
#' @export
h1.numeric = h1.vector

#' @rdname h1
#' @export
h1.character = h1.vector

#' @rdname h1
#' @export
h1.logical = h1.vector

#' @rdname h1
#' @export
h1.factor = h1.vector

#' @rdname h1
#' @export
h1.complex = h1.vector

#' @rdname h1
#' @export
h1.list = h1.vector

#' @rdname h1
#' @export
h1.matrix = h1.data.frame

#' @rdname h1
#' @export
h1.table = h1.data.frame




#' @rdname h1
#' @export
t1 = function(x, ...) {
  if(is_empty(x)) return(x)
  UseMethod('t1')
}

#' @rdname h1
#' @export
t1.vector = function(x, n=1, ..., stop_on_greedy=FALSE) {
  if(stop_on_greedy && (n < -length(x) || n > length(x))) {
    stop("Asking for non-existing elements.")
  } else if(n<=0) {
    utils::tail(x, n=n, ...) 
  } else {
    utils::tail(rev(rep_len(rev(x), length.out=n)), n=n, ...) 
  }
}

#' @rdname h1
#' @export
t1.data.frame = function(x, n=1, ..., stop_on_greedy=FALSE) {
  if(stop_on_greedy && (n < -nrow(x) || n > nrow(x))) {
    stop("Asking for non-existing elements.")
  } else if(n<=0) {
    utils::tail(x, n=n, ...) 
  } else {
    utils::tail(rev_rows(rep_df(rev_rows(x), ceiling(n/nrow(x)))), n=n, ...)    
  }
}

#' @rdname h1
#' @export
t1.default = function(x, n=1, ..., stop_on_greedy=FALSE) {
  if(stop_on_greedy && (n < -length(x) || n > length(x))) {
    stop("Asking for non-existing elements.")
  } else {
    utils::tail(x, n=n, ...)
  }
}

#' @rdname h1
#' @export
t1.numeric = t1.vector

#' @rdname h1
#' @export
t1.character = t1.vector

#' @rdname h1
#' @export
t1.logical = t1.vector

#' @rdname h1
#' @export
t1.factor = t1.vector

#' @rdname h1
#' @export
t1.complex = t1.vector

#' @rdname h1
#' @export
t1.list = t1.vector

#' @rdname h1
#' @export
t1.matrix = t1.data.frame

#' @rdname h1
#' @export
t1.table = t1.data.frame

