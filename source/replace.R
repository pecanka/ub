#' Value replacement
#'
#' Replaces the values in \code{x} with \code{new_values} either 
#' in the order of occurrence if \code{old_values} is missing, or 
#' using \code{old_values} for reference.
#'
#' @family sequence-related functions provided by utilbox
#' @export
replace2 = function(x, new_values, old_values, is_pattern=TRUE, number_new_values=FALSE) {
  
  if(missing(old_values)) {
    y = as.factor(x)
    stopifnot(nlevels(y)==length(new_values))
    levels(y) = new_values
    if(any(class(new_values)==c("integer","numeric"))) {
      y = as.numeric(levels(y))[y]
    } else if(class(new_values) == "character") {
      y = as.character(y)
    } else {
      y = as(as.character(y), class(new_values))
    }
  } else {
    y = x
    stopifnot(length(new_values)==length(old_values)) 
    f = if(is_pattern) regexpr else `==`
    idx = lapply(old_values, function(v) which(f(v, x)>0))
    len = sapply(idx, length)
    val = rep(new_values, len)
    if(number_new_values) val = paste0(val, unlist(sapply(len, seq)))
    y[unlist(idx)] = val
  }
  
  return(y)
}
#replace2 = plyr::revalue

#' Value replacement
#'
#' Replaces the values in \code{x} with \code{new_values} in the
#' order of occurrence. If \code{new_values} is missing, it is
#' set to the integers between 1 and the number of unique elements
#' in \code{x}. Optionally, via the argument \code{format} a 
#' function can be supplied which attempts to set the class of the
#' output (e.g. \code{format=as.numeric}).
#'
#' @examples
#' x = c('b','b','aa','a','b','c','a')
#' replace3(x)
#' replace3(x, c('bar','Aaron','and','Cuba'))
#' replace3(x, format=as.numeric)
#' replace3(x, c('bar','Aaron','and','Cuba'), format=as.numeric)  # produces a warning about NAs during conversion
#'
#' @family sequence-related functions provided by utilbox
#' @export
replace3 = function(x, new_values, format) {
  if(is_empty(x)) return(x)
  ux = unique(x)
  if(missing(new_values)) new_values = 1:length(ux)
  idx = lapply(ux, function(v) which(x==v))
  y0 = if(missing(format)) x[1] else format(new_values[1])
  y = rep(y0, length(x))
  for(i in seq_along(idx)) y[idx[[i]]] = new_values[i]
  y
}

