#' @title
#' Get every n-th element of an object
#'
#' @description
#'
#' `nth` extracts the n-th element (single value) of an object `x` 
#' (vector, list, etc.). It relies on `\`[[\`` for the extraction.
#'
#' `nthm` allows for multivariate input in `n`. It relies on `\`[\`` 
#' for the extraction.
#'
#' `nthr` and `nthmr are the analogue of `nth` and `nthm` which 
#' determine index the position in reversed order.
#'
#' @examples
#' # extaction of value
#' nth(1:10, 2)             # second
#' nthm(1:10, 3:4)          # third and fourth
#' nthr(1:10, 2)            # second from the end
#' nthmr(1:10, 3:4)         # third and fourth from the end
#'
#' # extaction and assignment of value
#' y = as.list(1:10)
#' nth(y, 2) = Inf       # second
#' nthm(y, 3:5) = Inf    # third thru fifth
#' nthr(y, 2) = Inf      # second from the end
#' nthmr(y, 3:5) = Inf   # third thru fifth from the end
#'
#' # extract via logicals
#' nthm(y, sapply(y, is.finite))
#'
#' @name filter_by_position
#' @family sequence-related functions provided by utilbox
#' @export
nth = function(x, n) {
  x[[n]]
}

#' @rdname filter_by_position
#' @export
nthm = function(x, n) {
  x[n]
}

#' @rdname filter_by_position
#' @export
nthr = function(x, n) {
  x[[n_nth_rev(n, x)]]
}

#' @rdname filter_by_position
#' @export
nthmr = function(x, n) {
  x[n_nth_rev(n, x)]
}

#' @rdname filter_by_position
#' @export
`nth<-` = function(x, n, value) {
  `[[<-`(x, n, value)
}

#' @rdname filter_by_position
#' @export
`nthm<-` = function(x, n, value) {
  `[<-`(x, n, value)
}

#' @rdname filter_by_position
#' @export
`nthr<-` = function(x, n, value) {
  `[[<-`(x, n_nth_rev(n, x), value)
}

#' @rdname filter_by_position
#' @export
`nthmr<-` = function(x, n, value) {
  `[<-`(x, n_nth_rev(n, x), value)
}

# process indexes for reversed extraction/assignment
n_nth_rev = function(n, x) {
  if(is.logical(n)) {
    rev(n)
  } else {
    ifelse(n>0, length(x)+1-n, -(length(x) + 1 - abs(n)))
  }
}

#' @title
#' Get every n-th element of an object
#'
#' @description
#'
#' Extracts every n-th element of an object (vector, list, etc.) 
#' starting from 'start'.
#'
#' @examples
#' every_nth(1:10, 2)
#' every_nth(1:10, 2, 2)
#'
#' @family sequence-related functions provided by utilbox
#' @export
every_nth = function(x, n, start=1L) {
  x[seq.int(as.integer(start), length(x), as.integer(n))]
}

#' @title
#' Get the last element
#'
#' @description
#'
#' Returns the last element in an vector or list.
#'
#' @family sequence-related functions provided by utilbox
#' @export
last_element = function(x) {
  x[length(x)]
}

#' @title
#' Locate value
#'
#' @description
#'
#' `first_nonzero` finds the value of the last non-zero element in 
#' `x`.
#'
#' `nth_nonzero` finds the value of the n-th non-zero element in `x`.
#'
#' `last_nonzero` finds the value of the last non-zero element in `x`.
#'
#' `first_positive` finds the value of the first positive element in 
#' `x`.
#'
#' `last_positive` finds the value of the last positive element in 
#' `x`.
#'
#' `first_above` (`first_above_soft`) finds the value of the first 
#' element in `x` with value larger than (larger than or equal to) `y`.
#'
#' `last_above` (`last_above_soft`) finds the value of the last 
#' element in `x` with value larger than (larger than or equal to) `y`.
#'
#' `first_below` (`first_below_soft`) finds the value of the first 
#' element in `x` with value smaller than (smaller than or equal to) `y`.
#'
#' `last_below` (`last_below_soft`) finds the value of the last 
#' element in `x` with value smaller than (smaller than or equal to) `y`.
#'
#' @family numeric functions provided by utilbox
#' @name extract_by_value
#' @export
#' @export
first_nonzero = function(x) {
  x[w_first_nonzero(x)]
}

#' @rdname extract_by_value
#' @export
nth_nonzero = function(x, n) {
  x[w_nth_nonzero(x)]
}

#' @rdname extract_by_value
#' @export
last_nonzero = function(x) {
  x[w_last_nonzero(x)]
}

#' @rdname extract_by_value
#' @export
first_positive = function(x) {
  x[w_first_nonzero(x)]
}

#' @rdname extract_by_value
#' @export
last_positive = function(x) {
  x[w_last_positive(x)]
}

#' @rdname extract_by_value
#' @export
first_above = function(x, y) {
  x[w_first_above(x, y)]
}

#' @rdname extract_by_value
#' @export
first_above_soft = function(x, y) {
  x[w_first_above_soft(x, y)]
}

#' @rdname extract_by_value
#' @export
last_above = function(x, y, ...) {
  x[w_last_above(x, y, ...)]
}

#' @rdname extract_by_value
#' @export
last_above_soft = function(x, y) {
  x[w_last_above_soft(x, y)]
}

#' @rdname extract_by_value
#' @export
first_below = function(x, y) {
  x[w_first_below(x, y)]
}

#' @rdname extract_by_value
#' @export
first_below_soft = function(x, y) {
  x[w_first_below_soft(x, y)]
}

#' @rdname extract_by_value
#' @export
last_below = function(x, y) {
  x[w_last_below(x, y)]
}

#' @rdname extract_by_value
#' @export
last_below_soft = function(x, y) {
  x[w_last_below_soft(x, y)]
}

#' @rdname extract_by_value
#' @export
first_nonempty = function(x) {
  x[w_first_nonempty(x)]
}

  
#' @title
#' Locate position of a value
#'
#' @description
#'
#' `w_first_nonzero` finds the position of the last non-zero element 
#' in `x`.
#'
#' `w_nth_nonzero` finds the position of the n-th non-zero element in 
#' `x`.
#'
#' `w_last_nonzero` finds the position of the last non-zero element 
#' in `x`.
#'
#' `w_first_positive` finds the position of the first positive 
#' element in `x`.
#'
#' `w_last_positive` finds the position of the last positive element 
#' in `x`.
#'
#' `w_last_above` finds the position of the last element in `x` with 
#' value larger than `y`.
#'
#' `w_first_above` finds the position of the first element in `x` 
#' with value larger than `y`.
#'
#' `w_last_below` finds the position of the last element in `x` with 
#' value below `y`.
#'
#' `w_first_below` finds the position of the first element in `x` 
#' with value below `y`.
#'
#' `w_first_nonempty` finds the position of the first non-empty 
#' element in `x`. It is designed as a method, which currenlty only 
#' operates on lists.
#'
#' @name locate_position_of_value
#'
#' @family numeric functions provided by utilbox
#' @export
w_first_nonzero = function(x) {
  h1(which(x!=0))
}

#' @rdname locate_position_of_value
#' @export
w_nth_nonzero = function(x, n) {
  stopifnot(n>=1)
  if(n==1) {
    h1(which(x!=0))
  } else {
    h1(tail(which(x!=0),-n+1))
  }
}

#' @rdname locate_position_of_value
#' @export
w_last_nonzero = function(x) {
  length(x) - w_first_nonzero(rev(x)) + 1
}

#' @rdname locate_position_of_value
#' @export
w_first_positive = function(x) {
  w_first_nonzero(x>0)
}

#' @rdname locate_position_of_value
#' @export
w_last_positive = function(x) {
  length(x) - w_first_nonzero(rev(x)>0) + 1
}

#' @rdname locate_position_of_value
#' @export
w_first_above = function(x, y) {
  w_first_positive(x>y)
}

#' @rdname locate_position_of_value
#' @export
w_first_above_soft = function(x, y) {
  w_first_positive(x>=y)
}

#' @rdname locate_position_of_value
#' @export
w_last_above = function(x, y) {
  w_last_positive(x>y)
}

#' @rdname locate_position_of_value
#' @export
w_last_above_soft = function(x, y) {
  w_last_positive(x>=y)
}

#' @rdname locate_position_of_value
#' @export
w_first_below = function(x, y) {
  w_first_positive(x<y)
}

#' @rdname locate_position_of_value
#' @export
w_first_below_soft = function(x, y) {
  w_first_positive(x<=y)
}

#' @rdname locate_position_of_value
#' @export
w_last_below = function(x, y) {
  w_last_positive(x<y)
}

#' @rdname locate_position_of_value
#' @export
w_last_below_soft = function(x, y) {
  w_last_positive(x<=y)
}

#' @export
w_first_nonempty = function(...) {
  UseMethod("w_first_nonempty")
}

#' @export
w_first_nonempty.list = function(x) {
  h1(which(sapply(x, is_nonempty)))
}

#' @title
#' Extract elements based on their positiveness/negativeness
#'
#' @description
#'
#' `positive` extract all positive elements in `x`.
#'
#' `negative` extacts all negative elements in `x`.
#'
#' `nonpositive` extacts all non-positive elements in `x`.
#'
#' `nonnegative` extacts all non-negative elements in `x`.
#'
#' @name extract_by_positiveness_negativeness
#' @family check-performing functions provided by utilbox
#' @export
positive = function(x) {
  x[!is.na(x) & x>0]
}

#' @rdname extract_by_positiveness_negativeness
#' @export
negative = function(x) {
  x[!is.na(x) & x<0]
}

#' @rdname extract_by_positiveness_negativeness
#' @export
nonnegative = function(x) {
  x[!is.na(x) & x>=0]
}

#' @rdname extract_by_positiveness_negativeness
#' @export
nonpositive = function(x) {
  x[!is.na(x) & x<=0]
}

#' @title
#' Filter an object based
#'
#' @description
#'
#' `filter_by_value` filters `x` based on whether its values match 
#' contain `pattern` as substring (fixed pattern matching).
#'
#' `filter_by_pattern` filters `x` based on whether its values match 
#' the pattern in `pattern` (regular pattern matching).
#'
#' `filter_by_name` filters `x` based on whether its names match the 
#' pattern in `pattern`.
#'
#' `filter_by_bool` filters `x` according to logical values in 
#' `keep`. It is just an alias for `x[`keep`]`.
#'
#' `filter_by_call` filters an object (`x`) based on whether the 
#' results of a function (call given in `call`) match the pattern in 
#' `pattern`. The `call` argument can be a standard R function (class 
#' `function`) or a symbolic `tidyverse`-style representation of a 
#' function, which uses the formula sign \code{~} to represent a 
#' function with a single argument `.x` and which is evaluated with `x` 
#' supplied to it as the argument `.x`.
#'
#' In all of these function the pattern can be a vector at a match
#' against any of its components is sufficient.
#'
#' @examples
#' v = c(first='hello',second='amsterdam')
#'
#' filter_by_value(v, 'ams')
#' 
#' filter_by_name(v, 'ams')
#'
#' filter_by_bool(v, c(TRUE,FALSE))
#' 
#' filter_by_call(v, ~nchar(.x)>6)
#'
#' filter_by_call(1:10, ~.x<5)
#'
#' x = 1; f = function() { x = 2; filter_by_call(1:10, ~.x>x) }; print(f())
#'
#' f = function() { x = 2; filter_by_call(data.frame(z=1:10), ~.x$z>x) }; print(f())
#' f = function() { x = 2; filter_by_call(data.frame(z=1:10), ~z>x) }; print(f())
#'
#' @name filter_by
#' @family sequence-related functions provided by utilbox
#' @export
filter_by_value = function(x, pattern, fixed=TRUE, exclude=FALSE) {
  keep = not_if(`%m_any%`(pattern, x, fixed=fixed), exclude)
  if(is_dimtwo(x)) x[keep,] else x[keep]
}
  
#' @rdname filter_by
#' @export
filter_by_pattern = function(x, pattern, fixed=FALSE, exclude=FALSE, ignore.case=FALSE) {
  keep = not_if(`%m_any%`(pattern, x, fixed=fixed, ignore.case=ignore.case), exclude)
  if(is_dimtwo(x)) x[keep,] else x[keep]
}
  
#' @rdname filter_by
#' @export
filter_by_name = function(x, pattern, fixed=FALSE, exclude=FALSE, ignore.case=FALSE) {
  keep = not_if(`%m_any%`(parent, names(x), fixed=fixed, ignore.case=ignore.case), exclude)
  if(is_dimtwo(x)) x[keep,] else x[keep]
}
  
#' @rdname filter_by
#' @export
#' @rdname filter_by
#' @export
filter_by_bool = function(x, keep, exclude=FALSE) {
  if(exclude) keep = !keep
  if(is_dimtwo(x)) x[keep,] else x[keep]
}
  
#' @rdname filter_by
#' @export
filter_by_call = function(...) {
  
  # translate the dots into this creates 'args' and 'call' in this environment
  args = dots_to_nlist()
  
  if(length(args)<2) 
    error('Supply both the vector to filter and the call to filter it with.')
  
  call = nthr(args, 1)
  args = nthmr(args, -1)
  
  ##lazy_dots_to_args_and_call(...)     
  ##nargs = length(args)
  
  #if(is_empty(args)) return(NULL)
  
  #names(args) = symbolic_call_names(length(args))
  fun = process_symbolic_call(call, length(args))
  
  # call the function constructed by `process_symbolic_call` while 
  # making sure the function can see what's in the environment
  # from which `filter_by_call` was called
  environment(fun) = parent.frame()
  fltr = do.call(fun, unname(args))
  
  x = args[[1]]
  if(is_dimtwo(x)) x[fltr,] else x[fltr]
  
}

#' @rdname filter_by
#' @export
filter_out = function(x, pattern, fixed=FALSE, ignore.case=FALSE) {
  if(is_empty(x)) return(x)
  keep = sapply(x, `%nm_any%`, pattern=pattern, fixed=fixed, ignore.case=ignore.case)
  if(is_dimtwo(x)) x[keep,] else x[keep]
}
  

#lazy_dots_to_args_and_call = function(..., envir=parent.frame()) {
#  args = list(...)
#  if(is_empty(args)) error('Supply a call.')
#  assign('call', nthr(args,1), envir=envir)
#  assign('args', nthmr(args, -1), envir=envir)
#}
