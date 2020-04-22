#' @title
#' Repeat object
#'
#' @description
#'
#' An upgraded version of `rep`. Allows for different number of 
#' occurrences for each value supplied in the first argument.
#'
#' @examples
#' rep2(c('a','b'), e=3:4)
#'
#' @family sequence-related functions provided by utilbox
#' @export
rep2 = function(x, each, ...) {
  
  if(missing(each)) {
    rep(...)
  } else if(length(each)==0) {
    numeric(0)
  } else if(length(each)>1 && length(each)==length(x)) {
    unlist(sapply(seq_along(each), function(i) rep(x[i], t=each[i])))
  } else {
    rep(..., each=each)
  }  

}

#rep2b = function(...) {
#  
#  d = list(...)
#  
#  if(length(d)==2) {
#    
#    eaches = c("each","eac","ea","e")
#    w_each = head(which(eaches %in% names(d)),1)
#    is_each = length(w_each)==1
#    
#    if(!is_each) {
#      rep(...)
#    } else {
#    
#      each = d[[eaches[w_each]]]
#      x = d[-which(names(d) == eaches[w_each])][[1]]
#    
#      if(length(each)==0) {
#        numeric(0)
#      } else if(length(each)>1 && length(each)==length(x)) {
#        unlist(sapply(seq_along(each), function(i) rep(x[i], t=each[i])))
#      } else
#        rep(...)
#    
#    }
#    
#  } else rep(...)
#
#}

