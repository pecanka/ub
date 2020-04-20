#' @title
#' Adjust R GUI
#'
#' @description
#' `R_gui_adjust()` changes the way the GUI looks (e.g. font size, 
#' font style). Without changing the default arguments, a call to 
#' `Rgui_adjust` makes the font smaller so that more info fits the 
#' screen. For changes in other ways set the value of `cmd` 
#' appropriately.
#'
#' `R_gui_adjust2()` is a wrapper for `R_gui_adjust()`, which by 
#' default only resizes the active window.
#'
#' `Rgui_reset()` resets the settings to their defaults.
#'
#' @family system-related functions provided by utilbox
#' @export
R_gui_adjust = function(cmd=c("points=7", "rows=51", "columns=248")) {
  
  if(!is_rgui()) return(invisible(FALSE))
    
  temp = tempfile()
  catn(paste(cmd, collapse="\n"), file=temp)
  loadRconsole(file = temp)
  invisible(NULL)
    
}

#' @rdname R_gui_adjust
#' @export
R_gui_adjust2 = function(cmd=c("points=10", "rows=37", "columns=154")) {
  R_gui_adjust(cmd)
}

#' @rdname R_gui_adjust
#' @export
R_gui_reset = function() {
  loadRconsole(file = file.path(R.home(), "etc/Rconsole"))
  invisible(TRUE)
}

