#' @title
#' Time stamp
#'
#' @description
#'
#' `timest()` generates a "unique" stamp based on the current system 
#' time and the R process system ID.
#'
#' @examples
#' timest()
#' timest(add_pid=FALSE)
#' timest(format="%Y%m%d%H%M%S")
#'
#' @family system-related functions provided by ub
#' @export
timest = function(add_time=TRUE, add_pid=TRUE, format="%Y%b%d%H%M%S") {
  paste0(if(add_time) format(Sys.time(), format) else NULL,
         if(add_pid) paste0("pid",Sys.getpid()) else NULL)
}

