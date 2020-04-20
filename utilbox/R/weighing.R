#' @title
#' Down-weighing
#'
#' @description
#' Down-weighs the values in `x` if they are below the pivot (`from`) 
#' at the rate of determined by `speed`. More specifically, the value of 
#' `speed` is used as the standard deviation of the normal density which 
#' is used to bring the values in `x` to zero as they deviate from the 
#' pivot.
#'
#' The argument `side` determines whether the down-weighing is 
#' applied only to the values below the pivot (\code{side='left'}), 
#' above the pivot (\code{side='right'}), or to both sides of the pivot 
#' (\code{side='both'}).
#'
#' The argument `speed` expects values between '0' (no down-weighing) 
#' and '1' (maximum down-weighing). Values outside this range are set to 
#' one of the two boundaries.
#'
#' @examples
#' downweight(1:10, 5)
#'
#' @family numeric functions provided by utilbox
#' @export
downweight = function(x, pivot, speed=0.15, side=c('left','right','both'), nonnegative=FALSE) {
  side = match.arg(side)
  
  speed = pmax(0, pmin(1, speed), speed)
  w = switch(side, 'left'=I(x<=pivot), 'right'=I(x>=pivot), 'both'=1:length(x))
  
  if(speed==0) return(x)
    
  x[w] = if(speed==1) {
    x[w] * as.numeric(x[w]==pivot)
  } else {
    x[w] * dnorm(x[w], pivot, abs(log(speed))) / dnorm(0, 0, abs(log(speed)))
  }
  
  if(nonnegative) x[w] = pmax(0, x[w])
  
  x
  
}
