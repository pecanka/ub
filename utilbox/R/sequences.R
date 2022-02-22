#' @title Points in a sequence
#'
#' @description
#'
#' `rotate()` shifts the elements in a vector in `x` by `lag` spaces.
#' With `value=NULL` (default), the elements are rotated, meaning 
#' those at the end move to the beginning (for `lag>0`) or those at 
#' the beginning move to the end (for `lag<0`). With a non-NULL value 
#' in `value` the rotated elements are replaced with `value`.
#'
#' `frac()` gives sequential fractions. It is similar to `base::diff()` 
#' except that it returns lagged ratios instead of lagged differences.
#'
#' `midpoints()` finds the midpoints between individual elements of a 
#' sequence given in `x`.
#'
#' `range_sym()` returns a symmetric range around the given point determined
#' via `around`. Useful for instance for determining axis range for producing
#' plots that are symmetric around a given value.
#'
#' `nunique()` counts the number of unique values in the supplied 
#' argument. Simply an alias for `length(unique(x))`.
#'
#' @examples
#' rotate(1:10)
#' rotate(1:10, 3)
#' rotate(1:10, 3, value=NA)
#'
#' frac(1:10)
#'
#' midpoints(1:10)
#' midpoints(c(1:5,3*1:5))
#'
#' range_sym((-3):5)
#' range_sym((-3):5, around=1)
#' range_sym((-3):5, around=4)
#'
#' @name sequences
#' @family numeric functions provided by utilbox
#' @export
rotate = function(x, lag=1, value=NULL) {
  
  n = if(is_dimtwoplus(x)) nrow(x) else length(x)
  
  lag = sign(lag) * (abs(lag) %% n)
  
  if(n %in% c(0,lag)) {
    x
  } else if(is.null(value)) {
    append(tail(x, lag), head(x, -lag))
  } else if(lag > 0) {
    append(rep(value, lag), head(x, -lag))
  } else {
    append(tail(x, lag), rep(value, -lag))
  }
  
}

#' @rdname sequences
#' @export
nunique = function(x) {
  length(unique(x))
}

#' @rdname sequences
#' @export
frac = function(x) {
  if(length(x)<=1) NA else tail(x, -1) / head(x, -1)
}

#' @rdname sequences
#' @export
midpoints = function(x) {
  0.5*(head(x, -1) + tail(x, -1))
}

#' @rdname sequences
#' @export
range_sym = function(..., around=0) {
  rng = range(...)
  lft = abs(around - rng[1])
  rgh = abs(rng[2] - around)
  around + c(-1,1) * max(lft, rgh)
}

### OLD CODE BELOW ###

#' @title Get a sequence
#'
#' @description
#'
#' `seq2()` produces a sequence in a manner very similar to 
#' [`base::seq`] except for when the inputs would default to a 
#' decreasing sequence (e.g. `seq(0)`, `seq(1,0)`) for which it returns 
#' a zero-length numeric vector.
#'
#' @examples
#' seq2(10)           # same as seq(10)
#' seq2(1,0,by=1)     # no error (unlike seq(1,0,by=1))
#'
#' # comparisons of `seq2` with `seq`
#' n = 0; seq2(n); seq(n)                       # different beharior
#' n = 1; seq2(n); seq(n)                       # same beharior
#' n = 5; seq2(n); seq(n)                       # same beharior
#'
#' to = 0; seq2(1,to); seq(1,to)                # different beharior
#' to = 0; seq2(1,to,-1); seq(1,to,-1)          # same beharior
#' to = 1; seq2(1,to); seq(1,to)                # same beharior
#' to = 2; seq2(1,to); seq(1,to)                # same beharior
#'
#' to = 0; seq2(to=to); seq(to=to)              # different beharior
#' to = 1; seq2(to=to); seq(to=to)              # same beharior
#' to = 7; seq2(to=to); seq(to=to)              # same beharior
#'
#' to = 0; try(seq2(1,to,1)); try(seq(1,to,1))  # same beharior (i.e. an error)
#' to = 4; seq2(1,to,1); seq(1,to,1)            # same beharior
#'
#' to = 0; seq2(1,to,l=5); seq(1,to,l=5)        # same beharior
#' to = 4; seq2(1,to,2); seq(1,to,2)            # same beharior
#'
#' to = 0; seq2(to=to,l=5); seq(to=to,l=5)      # different beharior
#'
#' @family sequence-related functions provided by utilbox
#' @export
seq2 = function(from=1, to=1, ...) {

  args = dots_to_nlist(names=c('by', 'length.out', 'along.with'))
  
  if(length(args)==0) {
    
    if(!missing(from) && missing(to)) {
      to = from
      from = 1
    } else if(missing(from) && !missing(to)) {
      by = 1
    }
    
    if(to<from) {
      numeric(0)
    } else {
      base::seq(from, to, by=1)
    }
    
  } else {  
    do.call(base::seq, nlist(from, to) %append% args)
  }
  
} 

#' @title Produce a sequence of numbers
#'
#' @description
#'
#' Produces an equidistant sequence between `a` and `b` of length approximately 
#' `len` which contains both `m1` and `m2`. The sequence might not contain the 
#' border points `a` and `b`. Enforcing their presence in the sequence can be
#' done via  `add_a=TRUE` and/or `add_b=TRUE`, althought then the equi-distance
#' of all points might no longer be true.
#'
#' @examples
#' seq_around(0, 10, 7, len=20)
#'
#' @family sequence-related functions provided by utilbox
#' @export
seq_around = function(a, b, m1, m2, len, add_a=FALSE, add_b=FALSE) {
  
  if(missing(a))
    error("Supply 'a' in seq_around.")
  
  if(length(a)>1) {
  
    b = max(a)
    a = min(a)
    if(missing(len) && equals_integer(a) && equals_integer(b)) 
      len = b - a + 1
    
  } else stopifnot(!missing(b))
  
  if(a > b && len>1)
    error("With 'len' above 1 the value in 'b' cannot be smaller than value in 'a'!")
      
  if(a > b) return(a)

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

