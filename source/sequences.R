#' Get a sequence
#'
#' Produces a sequence in a manner very similar to [base::seq]
#' except for when the inputs would default to a decreasing
#' sequence (e.g. `seq(0)`, `seq(1,0)`) for which it returns
#' a zero-length numeric vector.
#'
#' @examples
#' seq2(10)           # same as seq(10)
#' seq2(1,0,by=1)     # no error (unlike seq(1,0,by=1))
#'
#' # comparisons of `seq2` with `seq`
#' n = 0; seq2(n); seq(n)                       # different beharior
#' n = 1; seq2(n); seq(n)                       # same beharior
#' n = 2; seq2(n); seq(n)                       # same beharior
#'
#' to = 0; seq2(1,to); seq(1,to)                # different beharior
#' to = 0; seq2(1,to,-1); seq(1,to,-1)          # same beharior
#' to = 1; seq2(1,to); seq(1,to)                # same beharior
#' to = 2; seq2(1,to); seq(1,to)                # same beharior
#'
#' to = 0; seq2(1,to,1); seq(1,to,1)            # different beharior
#' to = 4; seq2(1,to,1); seq(1,to,1)            # same beharior
#'
#' to = 0; seq2(1,to,l=5); seq(1,to,l=5)        # different beharior
#' to = 4; seq2(1,to,2); seq(1,to,2)            # same beharior
#
#' seq2(1,3,by=0.1,l=10); seq(1,3,by=0.1,l=10)  # same beharior (error)
#
#' seq(1,3,l=5); seq2(1,3,l=5)                  # same beharior
#' seq(1,3,l=NULL); seq2(1,3,l=NULL)            # same beharior (error)
#'
#' @family sequence-related functions provided by utilbox
#' @export
seq2 = function(...) {

  args = dots_to_nlist(names=c('from','to'))
  
  base::seq(...)
  
  browser()
  
} 

seq3 = function(from=1, to=1, by, length.out, along.with, ...) {

  if(!missing(by) || !missing(length.out) || !missing(along.with)) {
    return(base::seq(from, to, by, length.out, along.with, ...))
  }
  
  if(missing(to)) { 
    if(from==0) {
      return(numeric(0))
    } else {
      to = from
      from = 1 
    }
  } else if(from!=to && sign(to - from) != sign(by)) {
    return(numeric(0))
  }
  
  if(missing(by) && !missing(length.out) && !is_empty(length.out)) {
    return(base::seq(from, to, ((to - from)/(length.out - 1)), ...))
  }
  
  base::seq(from, to, by, ...)
  
}

seq2.old = function(...) {
    
  if(class(s <- try(seq(...), silent = TRUE)) == "try-error") {
    NULL
  } else {
    s
  }
  browser()
  
}

#' Insert element into sequence
#'
#' Insert an element in \code{what} inside \code{x} at the position 
#' \code{after+1}. The replacement is processed backwards, which means
#' that new additions can be rewritten when `make_room=FALSE` and
#' the parameters
#'
#' @examples
#' x = as.list(1:10); w = list(99, 999); v = c(2, 7)
#' insert(x, w, v, count=2)                                     # making room, no dropping of rows
#' insert(x, w, v, count=2, make_room=FALSE, drop_limit=0)      # no dropping can be forced also via 'drop_limit'
#' insert(x, w, v, count=2, make_room=FALSE, drop_limit=1)      # only one row is dropped
#'
#' x = as.list(1:10); what = list(99, 999, 9999); at = c(2, 3, 7)
#' x = as.list(do.call(paste0, rep_list(list(LETTERS[1:15]),10)))
#' what = list('1','2','3'); at = c(2, 3, 7)
#' insert(x, what, at, count=1)
#' insert(x, what, at, count=3)
#' insert(x, what, at, count=3, make_room=FALSE)                    # the placements do not get shifted, thus overwriting

#' what[2] = list(list(a=1:10, b=sin))
#'
#' @name insert
#' @family sequence-related functions provided by utilbox
#' @export
insert = function(...) {
  UseMethod('insert')
}

#' @rdname insert
#' @export
insert.list = function(x, what, at, after, count=1, replace_old=!FALSE, 
  allow_overwriting=FALSE, indicate_new_old=TRUE) {

  if(is_empty(x)) return(x)
  
  if(missing(at) && missing(after))
    error("Supply either 'at' or 'after'.")
    
  if(!missing(at) && !missing(after))
    error("Supply only either 'at' or 'after'.")
    
  if(missing(at))
    at = after + 1

  if(!allow_overwriting && !is_unique(at))
    error("Overwriting of new elements is currently disabled.",
          "Set 'allow_overwriting=TRUE' to enable it.")
  
  if(any(at<1) || any(at>length(x)))
    error("Value in 'at' are out of range.")
    
  if(length(what)!=length(at))
    error("The lengths of 'what' and 'at' must match.")
    
  if(!is.list(what))
    error("'what' must be a list.")
    
  if(any(count<0))
    error("Negative counts are not allowed")
  
  # alter placements and counts 
  count = rep_len(count, length.out=length(what))

  at = at + cumsum(c(0, sapply(h1(what,-1), length) * h1(count,-1)))
  
  if(replace_old) {
    at = at - 1:length(what) + 1
  }
  
  # insert the values
  mod = insert_recurse.list(x, what, at, count, replace_old)

  # mirror the insertion to identify which elements are the original ones
  if(indicate_new_old) {
    is_old = as.list(rep(TRUE, length(x)))
    is_old_what = list_mirror(what, value=FALSE)
    old_and_new = insert_recurse.list(is_old, is_old_what, at, count, replace_old)
    old = which(unlist(old_and_new))
    new = which(!unlist(old_and_new))
    mod = structure(mod, new=new, old=old)
  }
  
  mod
  
}

#' @rdname insert
#' @export
insert.default = function(x, ...) {
  y = insert.list(as.list(x), ...)
  set_class(y, class(x)[1])
}
  
#' @rdname insert
#' @export
insert_recurse.list = function(x, what, at, count=1, replace=FALSE) {
  
  if(length(what)==1) {
    insert1(x, what, at, count, replace)
  } else {
    x1 = Recall(x, h1(what,-1), h1(at,-1), h1(count,-1), replace)
    insert1(x1, t1(what), t1(at), t1(count), replace)
  }

}

#' @rdname insert
#' @export
insert1 = function(...) {
  UseMethod("insert1")
}

#' @rdname insert
#' @export
insert1.list = function(x, what, at, count=1, replace=FALSE) {
  
  if(missing(at)) at = length(x)
  
  stopifnot(length(what)==1, length(at)==1, at>=0)
  
  if(is.list(what)) what = as.list(what[[1]])
  
  after = at - 1
  start = head(x, after)
  middle = rep_list(what, count)
  end = if(after==0) x else tail(x, -(after+replace))

  start %append% middle %append% end
  
}

#' Shift elements in an object
#'
#' Shift (rotate) the elements in a vector in \code{x} by \code{lag} spaces.
#' With `rotate=FALSE` the rotated elements are replaced with `value_rotated`
#' (`NA` by default).
#'
#' @family sequence-related functions provided by utilbox
#' @export
shift = function(x, lag=1, rotate=TRUE, value_rotated=NA) {
  
  n = if(is_dimtwoplus(x)) nrow(x) else length(x)
  
  if(n==0 || n==lag) return(x)
  
  lag = lag %% length(x)
  
  if(lag==0) return(x)
  
  if(rotate) {
    append(tail(x, lag), head(x, -lag))
  } else if(lag>0) {
    append(rep(value_rotated, lag), head(x, -lag))
  } else {
    append(tail(x, lag), rep(value_rotated, -lag))
  }
  
}

#' Count the number of unique values
#'
#' Count the number of unique values in the supplied argument. 
#' Simply an alias for \code{length(unique(x))}.
#'
#' @family sequence-related functions provided by utilbox
#' @export
nunique = function(x) {
  length(unique(x))
}

#' Lagged ratios
#'
#' Similar to \code{base::diff} except returns lagged ratios instead of lagged differences.
#'
#' @family numeric functions provided by utilbox
#' @export
frac = function(x) {
  if(length(x)<=1) NA else tail(x, -1) / head(x, -1)
}

#' Find midpoints of a sequence
#'
#' Find the midpoints between individual elements of a sequence given in \code{x}.
#'
#' @examples
#' midpoints(1:10)
#' midpoints(c(1:5,3*1:5))
#'
#' @family sequence-related functions provided by utilbox
#' @export
midpoints = function(x) {
  0.5*(head(x, -1) + tail(x, -1))
}

#' Produce a sequence of numbers
#'
#' Produces an equidistant sequence between a and b which contains both
#' m1 and m2 of appropriate length near len. The sequence might not contain
#' the border points points a and b. Forcing them into the sequence
#' can be done via add_a and/or add_b, but the equi-distance of all points 
#' might no longer be true.
#'
#' @family sequence-related functions provided by utilbox
#' @export
seq_around = function(a, b, m1, m2, len, add_a=FALSE, add_b=FALSE) {
  
  stopifnot(!missing(a))
  
  if(length(a)>1) {
  
    b = max(a)
    a = min(a)
    if(missing(len) && is_integer(a) && is_integer(b)) len = b-a+1
    
  } else stopifnot(!missing(b))
  
  if(a > b) {
    
    if(len>1) 
      error("With 'len' above 1 the value in 'b' cannot be smaller than value in 'a'!")
      
    return(a)
    
  }

  # Use ordinary seq if both middle points
  if(missing(m1) && missing(m2)) {
    
    x = seq(a,b,l=len)
  
  # Add only one middle point
  } else if(missing(m2) || abs(m1-m2)/abs(b-a)<1e-6) {

    if(a>m1) 
      error("Make m1 >= a.")
    
    if(m1>b) 
      error("Make m1 <= b.")
      
    if(m1==a || m1==b) {
    
      x = seq(a,b,l=len)
    
    } else {
    
      rat = (m1 - a) / (b - a)
      l1 = max(2,ceiling(len * rat))
      start_seq = seq(a,m1,l=l1)
      end_seq = seq(m1,b,by=start_seq[2]-start_seq[1])
      x = c(start_seq, end_seq[-1])
      
    }
  
  # Add both middle points
  } else {
  
    if(m1>m2) { tmp = m2; m2 = m1; m1 = tmp; rm(tmp) }
    if(m2>=b) error("Make both midpoints smaller than b.")
    if(a>=m1) error("Make both midpoints larger than a.")
    rat = (m2 - m1) / (b - a)
    l1 = max(2,ceiling(len * rat))
    middle_seq = seq(m1,m2,l=l1)
    start_seq = rev(seq(m1,a,by=middle_seq[1]-middle_seq[2]))
    end_seq = seq(m2,b,by=middle_seq[2]-middle_seq[1])
    x = c(start_seq, middle_seq[-1], end_seq[-1])
  
  }
  
  # Append the ends
  if(add_a && head(x,1)>a) x = c(a,x)
  if(add_b && tail(x,1)<b) x = c(x,b)
  
  return(x)
}

