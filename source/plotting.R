#' @title
#' List/close graphical devices
#'
#' @description
#'
#' List or close all graphical devices.
#'
#' `.all_devs` returns a list of all open graphical devices with an 
#' announcement of the number of open devices. The announcement can be 
#' disabled.
#'
#' `.all_devs_off` and `.ado` close all open graphical devices either 
#' with announcement (`.ado`) or silently (`.all_devs_off`).
#'
#' @examples
#' .all_devs(silent=FALSE)
#'
#' @name all_devs
#' @family plotting-related functions provided by utilbox
#' @export
.all_devs = function(silent=TRUE) {
  if(!silent) message("Found ",length(dev.list())," open devices.") 
  dev.list()
}

#' @rdname all_devs
#' @family plotting-related functions provided by utilbox
#' @export
.all_devs_off = function(silent=TRUE) {

  devs = .all_devs(silent)
  
  if(is.null(devs)) return(invisible(0))

  if(!silent) message("Closing all open devices ...")
  
  for(dev in devs) dev.off(dev)

  if(!is.null(dev.list()))
    warn("Some devices could not be closed.")
    
  else if(!silent) 
    cat0("All devices closed.\n")
  
}

#' @rdname all_devs
#' @family plotting-related functions provided by utilbox
#' @export
.ado = function() {
  .all_devs_off(silent=FALSE)
}

#' @title
#' Color palette
#'
#' @description
#'
#' Returns a specified number of colors a from a color palette. If 
#' the requested number of colors is larger than the size of the palette 
#' the colors are recycled.
#'
#' @examples
#' palette_ub()
#' palette_ub(3)
#' 
#' @family plotting-related functions provided by utilbox
#' @export
palette_ub = function(n) {

  palette = c(
    '#df64df','#12c909','#dbcb35','#6c44a1','#4bbb5d',
    '#0357a7','#862d20','#ffa703','#2fe996','#754343',
    '#fa499b','#34b454','#d4af37','#7b1a28','#9a3aca',
    '#3f8126','#0f6567','#a32a1d','#6a3631','#d655ba',
    '#f90f0b','#f7ab4d','#fdfc0d','#0564b2','#30a316',
    '#c24d5b','#46f600','#d2f449','#6ab5fb','#082470',
    '#97c134','#e4da44','#3db2de','#9f74a4','#ff8c3c',
    '#274263','#5ba394','#3d85c6','#6bc82e','#6d9eeb',
    '#1fe2f3','#1b3f90','#65200c','#a61c00','#cc4125',
    '#dd7e6b','#86b8af','#58cbad','#f4cccc','#974e13',
    '#38761d','#e06666','#cc0000','#20124d','#492eee',
    '#290050','#c41dc6','#4d50dd','#f13636','#a4c2f4',
    '#98d9f9','#911e1e','#2f71a3','#dca1f9','#0e562d',
    '#3ef406','#7fd700','#0512f4','#4f9cf4','#04a918',
    '#dc143c','#6a5acd','#ff3f33','#af4500','#ffc1cc',
    '#d3a57f','#b4641f','#e27d17','#0e9915','#98ac59',
    '#1bff00','#00feff','#a02052','#888888','#70266b',
    '#e0d040','#40e0d0','#d040e0','#a0e040','#e08040',
    '#7e9953','#d9a739','#bd0808','#0300ff','#e8ff00',
    '#e94900','#9f5f5f','#f4951b','#6d3300','#7b33b4')
  
  if(missing(n)) n = length(palette)
  
  h1(palette, n)
  
}

#' @title
#' Generate a vector of colors
#'
#' @description
#'
#' Generate a vector of colors based on the values in the supplied 
#' vector `x`.
#'
#' @examples
#' x = c('a','b','a','c'); as_color(x)
#' 
#' @family plotting-related functions provided by utilbox
#' @export
as_color = function(x, palette=palette_ub) {
  palette(nunique(x))[groups_of_unique(x)]
}

#' @title
#' A color palette
#'
#' @description
#'
#' Returns a list of `n` interesting and contrasting colors (at least 
#' as far as neighbours go). Up to 100 unique colors. For `n` above 100 
#' the colors are recycled in a loop.
#'
#' @examples
#' Colors(1)                         # gives 1 (the first) color
#' Colors(5)                         # gives 5 colors from the beginning
#' Colors(21:25)                     # gives 5 colors from a different place
#' Colors(c('a','b','b'))            # gives 3 colors, the last two are the same
#' Colors(seq(0,1,l=10))             # gives colors from the entire palette sampled equidistantly
#'
#' @family plotting-related functions provided by utilbox
#' @export
Colors = function(n=20, skip=0, randomize=FALSE, reverse=FALSE) {
  
  x = if(is.numeric(n)) {
    if(length(n)==1) 1:n else n
  } else {
    groups_of_unique(n)
  }
  
  w_na = is.na(x)
  y = x[!w_na]

  stopifnot(y>=0)
  stopifnot(min(y)>=1 || max(y)<=1)
  if(any(y<1)) y = y*(length(palette_ub())-1) + 1

  x[!w_na] = y
  
  x = x + max(0, skip)
  
  p = palette_ub(max(x))[x]
  if(randomize) p = sample(p)
  if(reverse) p = rev(p)
  
  p
}

#' @title
#' Illustrate the utilbox color palette
#'
#' @description
#'
#' Produces a plot which shows the colors returned by `palette_ub`, 
#' via a call to `Colors`.
#'
#' @examples
#' show_Colors(30)
#'
#' @family plotting-related functions provided by utilbox
#' @export
show_Colors = function(n, col, cex=3, pch=16, pos1=2, pos2=4, adj1=-.25, adj2=0.2, sort=FALSE) {
  if(missing(n)) n = length(palette_ub())
  if(missing(col)) col = Colors(n)
  if(sort) col = sort(col)
  y = 1:length(col)
  x = (what_bag(y) - 1)*3
  y = - ((y-1)%%20+1)
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(y=y, x=x, cex=cex, pch=pch, col=col, xlim=range(x)+c(-1,3), ylim=range(y)+c(-1,1), axes=FALSE)
  text(1:length(col)%p%'.', x=x+adj1, y=y, cex=0.7, pos=pos1)
  text(col, x=x+adj2, y=y, cex=0.8, pos=pos2)
}

#' @title
#' Histogram
#'
#' @description
#'
#' A wrapper for \code{base::hist}, which allows to specify the 
#' number of bins by through `nbreaks`.
#'
#' @family plotting-related functions provided by utilbox
#' @export
hist2 = function(..., nbreaks) {
  if(missing(nbreaks)) {
    hist(...)
  } else {
    arg = list(...)
    x = if(any(names(arg)=="x")) arg[["x"]] else arg[[1]]
    x = x[is.finite(x)]
    if(length(x)==0) error("No values to plot.")
    breaks = seq(min(min(x)*0.99,min(x)-1e-9),max(max(x)*1.01,max(x)+1e-9), l=nbreaks)
    hist(..., breaks=breaks)
  }
}

#' @title
#' Add tight axis labels to a plot
#'
#' @family plotting-related functions provided by utilbox
#' @export
add_tight_axes = function(xlab=NULL, ylab=NULL, linex=1.4, liney=1.47, padjx=-0.85, 
  padjy=0.85, tclx=-0.3, tcly=-0.3, cex=NULL, cexx=1, cexy=1, cexfaclabx=1, cexfaclaby=1, 
  xlim=c(-Inf,Inf), ylim=c(-Inf,Inf), scipenx, scipeny, scipen0, ylabfun, xlabfun,
  do_x=TRUE, do_y=TRUE) {
  
  if(missing(cexx) && !missing(cex)) cexx = cex
  if(missing(cexy) && !missing(cex)) cexy = cex

  if(!missing(scipenx)) options(scipen=scipenx)
  xt = axTicks(side=1)
  xt = xt[xlim[1]<=xt & xt<=xlim[2]]
  xtl = if(!missing(xlabfun)) xlabfun(xt) else xt
  if(do_x) axis(side=1, at=xt, lab=xtl, line=0, padj=padjx, tcl=tclx, cex.axis=cexx)
  
  if(!missing(scipeny)) options(scipen=scipeny)
  yt = axTicks(side=2)
  yt = yt[xlim[1]<=yt & yt<=ylim[2]]
  ytl = if(!missing(ylabfun)) ylabfun(yt) else yt
  if(do_y) axis(side=2, at=yt, lab=ytl, line=0, padj=padjy, tcl=tcly, cex.axis=cexy)
  
  if(!missing(scipen0)) options(scipen=scipen0)
  mtext(xlab, side=1, line=linex, cex=cexfaclabx*cexx)
  mtext(ylab, side=2, line=liney, cex=cexfaclaby*cexy)
	
  box()
  
  return(invisible(NULL))
}

#' @title
#' Add a box above a plot
#'
#' @family plotting-related functions provided by utilbox
#' @export
add_box_above = function(nboxes=0, labels=NULL, hfr=0.07, hsp=0.01, hfac=1, base=0, 
                         col="black", bg=grey(0.35), cex=1, offset=0, logscale="") {
  
  # Check for an unopen device
  if(dev.cur()==1) {
    warn("Cannot add box because no device seems to be open.")
    return(invisible(NULL))
  }
  
  if(length(hfr)==1)    hfr = rep(hfr, nboxes)
  if(length(hsp)==1)    hsp = rep(hsp, nboxes)
  if(length(bg)==1)     bg = rep(bg, nboxes)
  if(length(col)==1)    col = rep(col, nboxes)
  if(length(cex)==1)    cex = rep(cex, nboxes)
  if(length(offset)==1) offset = rep(offset, nboxes)
  
  # Calculate position of the box
  xy = par()$usr
  hght = diff(xy[3:4]) / hfac^1.5
  
  if(class(labels)=="call") labels = list(labels)
  
  par(xpd=NA)
  yt = base + xy[4]
  for(i in 1:nboxes) {
  
    yb = yt + hsp[i]*hght
    yt = yb + hfr[i]*hght
    
    if(grepl("x", logscale)) {
      xy[1:2] = 10^xy[1:2]
    }
    
    if(grepl("y", logscale)) {
      yb = 10^yb
      yt = 10^yt
    }

    # Plot the box
    rect(xleft=xy[1], xright=xy[2], ybottom=yb, ytop=yt, col=bg[i])
    
    l = 0.85
    if(length(labels)>=i) 
      text(x=mean(xy[1:2]), y=l*yb+(1-l)*yt, lab=as.expression(labels[[i]]), pos=3, col=col[i], 
           cex=cex[i], offset=offset[i])
    
  }
  par(xpd=FALSE)
  
  return(invisible(NULL))
}
