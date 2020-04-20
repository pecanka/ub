#' @title
#' Minimal distance within a vector
#'
#' @description
#' Get the minimal distance between elements of `x`.
#'
#' @family numeric functions provided by utilbox
#' @export
min_dif = function(x) {
  if(length(x)==1) NA else min(diff(sort(x)))
  #min(sapply(x, function(x1) min(abs(x1-x))))
}

#' @title
#' Maximum/minimum
#'
#' @description
#' More versatile versions of `base:which.max` and 
#' `base::which.min`.
#'
#' Essentially, the functions `which_max` and `which_min` behave just 
#' like their base equivalents except they can return the position of 
#' both first or last extreme in a vector (the last by default). They 
#' also differ in the treatment of `NA`s. The removal of NA values can 
#' be done in two ways, they are either removed and then the position of 
#' the extreme in a vector stripped of `NA`s is returned, or they are 
#' ignored (default) and the position of a extreme within the entire 
#' original vector is returned.
#'
#' `max0()` is a wrapper for `base::max` which does not produce warnings.
#'
#' @examples
#' which_max(1:5)
#' which_max(1:5, last=FALSE)
#' which_max(c(NA,1:5))
#' which_max(c(NA,1:5), na.ignore=FALSE, na.rm=TRUE)
#'
#' @family numeric functions provided by utilbox
#' @export
which_max = function(x, last=TRUE, first=FALSE, all=FALSE, na.rm=FALSE, 
  na.ignore=TRUE, arr.ind=FALSE) {
  
  which_min(-x, last=last, first=first, all=all, na.rm=na.rm, 
            na.ignore=na.ignore, arr.ind=arr.ind)
            
}
  
#' @rdname which_max
#' @export
which_min = function(x, last=TRUE, first=FALSE, all=FALSE, na.rm=FALSE, 
  na.ignore=TRUE, arr.ind=FALSE, eps=0) {
  
  if(!missing(first) && missing(last)) last = !first
  if(!missing(last) && missing(first)) first = !last
  
  if(na.rm) x = x[!is.na(x)]
  if(na.ignore) x[is.na(x)] = Inf
  
  is_min = which(abs(x-min(x))<=eps)
  w = if(all) is_min else c(if(last) t1(is_min) else NULL, if(first) h1(is_min) else NULL)
  
  #w = if(last) length(x) + 1 - which.min(rev(x)) else which.min(x)
  
  if(length(dim(x))>1 && arr.ind) {
    w = which(array(seq_along(x), dim=dim(x))==w, arr.ind=arr.ind)
  }
  
  w
  
}

#' @rdname which_max
#' @export
max0 = function(..., na.rm=FALSE, val0=-Inf) {
  x = unlist(c(...))
  if(na.rm) x = x[!is.na(x)]
  if(length(x)==0) val0 else base::max(..., na.rm=na.rm)
}

