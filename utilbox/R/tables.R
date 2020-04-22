#' @title
#' Contingency table
#'
#' @description
#'
#' Does the same thing as \code{base::table} but makes sure that the 
#' counts of values in `obligate` are included in the final table, 
#' although only if a tabulation of a single vector is performed.
#'
#' @examples
#' table2(
#' @export
table2 = function(..., obligate=NULL) {
  tab = table(...)
  nams = names(tab)
  if(!is.null(obligate) && !is.null(nams)) {
    miss = setdiff(obligate, nams)
    if(length(miss)>0) {
      ztab = as.table(rep(0, length(miss)))
      names(ztab) = miss
      tab = append(tab, ztab)
      nams = names(tab)
      ord = order(if(is_number(nams)) as_numeric(nams) else nams)
      tab = tab[ord]
    }
  }
  return(tab)
}

#' @title
#' Quick tabulation
#'
#' @description
#'
#' A quicker version of \code{base::table}. Assumes natural number 
#' values and every value outside 1,2,... is counted as 0.
#' @export
tablenat = function(x) {
  C = tabulate(x)
  lev = which(C>0)
  C = C[lev]
  names(C) = lev
  if(sum(C)<length(x)) C = c("0"=length(x)-sum(C), C)
  return(C)
}
