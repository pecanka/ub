#' @title
#' Type conversion
#'
#' @description
#'
#' `as_numeric()` converts to type `numeric` without any warnings about non-numeric 
#' elements. Non-numeric elements are turned into `NA`. Basically, a 
#' less verbose version of \code{base::as.numeric}.
#'
#' `make_numeric()` changes the type of `x` to `numeric` whenever all elements can be 
#' converted (using \code{base::as.numeric}. Otherwise, calls the 
#' function supplied in `on_error`. This allows a direct control of what 
#' happens on the conversion via supplying the error function. 
#' Basically, a more flexible (but by default a more stringent) version 
#' of \code{base::as.numeric()}.
#'
#' `force_as_integer()` and `force_as_real()` convert to type `integer` and 
#' `double`, respectively, by stripping all non-number substrings in each 
#' element of `x`.This is a very radically forced conversion, which can be useful 
#' when strange artefacts polluted the data, but caution is advised.
#'
#' `force_unnumber()` removes all number characters from `x`. (It does not remove
#' the character parts of a number in the scientific notation.)
#'
#' @examples
#' # convert to numeric type
#' as_numeric(c("1","2"))
#' as_numeric(c("1","a"))
#'
#' make_numeric(c("1","2"))       # conversion works
#' make_numeric(c("1","x"))       # conversion fails
#'
#' # force as integers
#' force_as_integer(c('1','2'))
#' force_as_integer(c('1','x'))
#' force_as_integer(c('1','2.0','a3','-17.x','not-17.2','not-17.2-'))
#'
#' # force as real numbers
#' force_as_real(c('1','2.0','a3','-17.x','not-17.2','not-17.2-'))
#' force_as_real(c('1','2.0','a3','-17.x','not-17.2','not-17.2-'), ignore_signFALSE)
#' force_as_real(c('1','x'))
#'
#' # an example of a custom error function
#' err = function(x) {catn("Problem converting the values:\n"); print(x[!is_number(x,I)]); stop()}
#' make_numeric(c('1','a'), on_error=err)
#'
#' # remove all numbers
#' unnumber('x: '%.%c('1','32','1.2','.32342','212.20','+42.2','-13','-0.2'))
#' # remove all numbers and number strings (including the decimal point)
#' unnumber('x: '%.%c('1','32','1.2','.32342','212.20','+42.2','-13','-0.2'), TRUE)
#' # remove all numbers and number strings (including the decimal point)
#' unnumber('x: '%.%c('1','32','1.2','.32342','212.20','+42.2','-13','-0.2'), TRUE, TRUE)
#'
#' @family numeric conversion functions provided by utilbox
#' @export
as_numeric = function(x) {
  y = as.numeric(rep(NA, length(x)))
  is_num = is_number(x, freduce=I)
  y[is_num] = as.numeric(x[is_num])
  y
}

#' @rdname as_numeric
#' @export
make_numeric = function(x, convert=as.numeric, on_error=stop) {
  if(is.numeric(x)) {
    x 
  } else if(is_number(x)) {
    as.numeric(x)
  } else on_error("Cannot convert x to class 'numeric'.")
}

#' @rdname as_numeric
#' @export
force_as_integer = function(x, ignore_sign=TRUE, na_val) {

  w = if(ignore_sign) {
    rep(FALSE, length(x)) 
  } else {
    grepl(paste0('[-][0-9]'), x)
  }
  
  y = ifelse(!w, 
             gsub('[^0-9]*','',x),
             '-' %.% gsub('[^0-9]*','',substr(x,w+1,nchar(x))))
                  
  y = as.integer(y)
  
  if(!missing(na_val)) {
    y[is.na(y)] = na_val
  }
  
  y
  
}

#' @rdname as_numeric
#' @export
force_as_real = function(x, ignore_sign=TRUE, dec='.', dec_fixed=TRUE, na_val) {

  w = grep(dec, x, fixed=dec_fixed)
  
  lp = ifelse(!w, 
              force_as_integer(x, ignore_sign=ignore_sign, na_val='0'), 
              force_as_integer(substr(x,1,w-1), ignore_sign=ignore_sign, na_val='0'))
                   
  rp = ifelse(!w, 
              0, 
              force_as_integer(substr(x,w+1,nchar(x)), ignore_sign=TRUE, na_val='0'))
              
  y = as.numeric(lp %.% '.' %.% rp)
  
  if(!missing(na_val)) {
    y[is.na(y)] = na_val
  }
  
  y
}

#' @rdname as_numeric
#' @export
unnumber = function(x, drop_dec=FALSE, drop_sign=FALSE) {
  
  dsp = if(drop_sign) '[+-]?' else ''
  
  pattern = if(drop_dec) {
    collapse0(dsp%.%c('[0-9][.]?[0-9]','[.]?[0-9]','[0-9][.]?','[0-9]'), sep="|")
  } else {
    dsp%.%'[0-9]'
  }
  
  gsub(pattern,'', x)
  
}
