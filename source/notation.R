#' Convert a number to an abbreviated notation
#' @export
short_notation = function(x, exact_cutoff=999, ndig=1, word_units=FALSE) {
  
  # Figure out which to convert
  x = as.numeric(x)
  xx = x
  xx[is.na(x)] = as.character(NA)
  stay = x<=exact_cutoff
  xx[stay] = as.character(x[stay])
  wx = !is.na(x) & !stay

  # Figure out the units
  w = trunc(log10(x[wx])/3)
  w1 = pmin(w+1,5)
  unit = sapply(w1, function(y) if(is.na(y) || is.nan(y)) NA else 
                                if(word_units) switch(y, "", "thousand", "million", "billion", "trillion") else 
                                switch(y, "", "K", "M", "B", "T"))
  
  # Do the conversion and append the units
  xx[wx] = paste(as.character(round(x[wx]/10^(3*w),ndig)), unit, sep=ifelse(word_units," ",""))

  return(xx)
}

#' Convert a number to an abbreviated notation using 
#' @export
short_notation_exp = function(x, base=10, ndig=2, as_expr=FALSE) {
  x = sapply(x, function(x1) { 
                  z = signif(log(x1,base),ndig); 
                  if(z==0) bquote(""~10^{""~0}) else bquote(.(base)^{.(z)})})
  if(as_expr) x = sapply(x, as.expression)
  if(length(x)==1) x = x[[1]]
  return(x)
}

#' @export
N2T = short_notation

#' @export
N2Texp = short_notation_exp

#' @export
shorten_sequence = function(x, sep="-", collapse="-", f=length) 
  paste(c(paste(range(x),collapse=collapse),head(f(x),1)),collapse=sep)

