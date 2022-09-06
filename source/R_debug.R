#' @title
#' Stopping and quitting
#'
#' @description
#'
#' `.sow()` sets `options()$warn` to either 0 or 2, which makes R 
#' either stop when a warning is issued (>=2) or not (0). If the argument 
#' `turn_on` is missing, the function acts as a toggle turning stopping 
#' on warnings on and off on alternative calls.
#'
#' `.roe()` analogously toggle the recovering on error option (same 
#' logic as `.sow()`).
#'
#' `.q()` is a shortcut for quitting R session without being asked to 
#' save the current workspace.
#'
#' @name stopping_control
#' @family system-related functions provided by utilbox
#' @export
.sow = function(turn_on, announce=TRUE) {
  
  if(missing(turn_on)) turn_on = options()$warn<2
  options(warn=ifelse(turn_on, 2, 0))
  
  status = ifelse(turn_on,"ENABLED","DISABLED")
  
  if(announce) note("Stopping on warnings has been ",status,".")
  
  invisible(status)
  
}

#' @rdname stopping_control
#' @export
.roe = function(turn_on, announce = TRUE, tb_max_lines = 1000) {
  
  if(missing(turn_on)) 
    turn_on = is.null(options()$error)
    
  options(traceback.max.lines = tb_max_lines)
  
  options(error = if(turn_on) recover else NULL)
  
  status = ifelse(turn_on, "ENABLED", "DISABLED")
  
  if(announce) note("Recovery on error has been ",status,".")
  
  invisible(status)
}

#' @rdname stopping_control
#' @export
.q = function(save="no") {
  base::q(save=save)
}

