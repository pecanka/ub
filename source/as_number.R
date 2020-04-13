#' Type conversion
#'
#' Convert to type \code{numeric} without any warnings about non-numeric 
#' elements. Non-numeric elements are turned into \code{NA}. Basically,
#' a less verbose version of \code{base::as.numeric}.
#'
#' @examples
#' as_numeric(c("1","2"))
#' as_numeric(c("1","a"))
#'
#' @family numeric functions provided by utilbox
#' @export
as_numeric = function(x) {
  y = as.numeric(rep(NA, length(x)))
  is_num = is_number(x, freduce=I)
  y[is_num] = as.numeric(x[is_num])
  y
}

#' Type conversion
#'
#' Changes the type of \code{x} to \code{numeric} whenever all
#' elements can be converted (using \code{as.numeric}. Otherwise,
#' calls the function supplied in \code{on_error}. This allows
#' a direct control of what happens on the conversion via supplying
#' the error function. Basically, a more flexible (but by default 
#' a more stringent) version of \code{base::as.numeric}.
#'
#' @examples
#' make_numeric(c("1","2"))       # conversion works
#' make_numeric(c("1","x"))       # conversion fails
#'
#' # an example of a custom error function
#' err = function(x) {catn("Problem converting the values:\n"); print(x[!is_number(x,I)]); stop()}
#' make_numeric(c('1','a'), on_error=err)
#'
#' @family numeric functions provided by utilbox
#' @export
make_numeric = function(x, convert=as.numeric, on_error) {
  if(missing(on_error)) {
    on_error = function(...) stop("Cannot convert x to class 'numeric'.")
  }
  if(is.numeric(x)) {
    x 
  } else if(is_number(x)) {
    as.numeric(x)
  } else on_error(x)
}

#' Force type conversion
#'
#' Convert to type integer (\code{force_as_integer}) or double 
#' (\code{force_as_real)} by stripping all non-number substrings 
#' in each element of \code{x}. 
#'
#' This is a very radically forced conversion, which can be useful 
#' when strange artefacts polluted the data, but caution is advised.
#'
#' @examples
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
#' @name force_as_
#'
#' @family numeric functions provided by utilbox
#' @export
force_as_integer = function(x, ignore_sign=TRUE, na_val) {
  w = if(ignore_sign) rep(-1, length(x)) else regexpr(paste0('[-][0-9]'), x)
  y = ifelse(w<0, gsub('[^0-9]*','',x),
                  paste0('-',gsub('[^0-9]*','',substr(x,w+1,nchar(x)))))
  y = as.integer(y)
  if(!missing(na_val)) y[is.na(y)] = na_val
  y
}

#' @rdname force_as_
#'
#' @family numeric functions provided by utilbox
#' @export
force_as_real = function(x, ignore_sign=TRUE, dec='.', dec_fixed=TRUE, na_val) {
  w = regexpr(dec, x, fixed=dec_fixed)
  lp = ifelse(w<0, force_as_integer(x, ignore_sign=ignore_sign, na_val='0'), 
                   force_as_integer(substr(x,1,w-1), ignore_sign=ignore_sign, na_val='0'))
  rp = ifelse(w<0, 0, 
                   force_as_integer(substr(x,w+1,nchar(x)), ignore_sign=TRUE, na_val='0'))
  y = as.numeric(paste(lp,rp,sep='.'))
  if(!missing(na_val)) y[is.na(y)] = na_val
  y
}

