#' @title
#' Pipe friendly conditional operation
#'
#' @description
#'
#' Apply a transformation on the data only if a condition is met, by 
#' default if condition is not met the input is returned unchanged.
#'
#' Credit goes to 
#' [`Moody_Mudskipper](https://github.com/moodymudskipper), Code taken 
#' from 
#' [stackoverflow.com`](https://stackoverflow.com/questions/30604107/r-co
#' nditional-evaluation-when-using-the-pipe-operator`)
#'
#' The use of formula or functions is recommended over the use of 
#' expressions for the following reasons :
#'
#' \itemize{
#'   \item If `true` and/or `false` are provided as expressions they 
#' will be evaluated wether the condition is `TRUE` or `FALSE`. 
#' Functions or formulas on the other hand will be applied on the data 
#' only if the relevant condition is met
#'   \item Formulas support calling directly a column of the data by 
#' its name without \code{x$foo} notation.
#'   \item Dot notation will work in expressions only if `pif` is 
#' used in a pipe chain
#' }
#'
#' @param x An object
#' @param p A predicate function, a formula describing such a 
#' predicate function, or an expression.
#' @param true,false Functions to apply to the data, formulas 
#' describing such functions, or expressions.
#'
#' @return The output of `true` or `false`, either as expressions or 
#' applied on data as functions
#'
#' @examples
#' # using functions
#' pif(iris, is.data.frame, dim, nrow)
#' # using formulas
#' pif(iris, ~is.numeric(Species), ~"numeric :)",~paste(class(Species)[1],":("))
#' # using expressions
#' pif(iris, nrow(iris) > 2, head(iris,2))
#' # careful with expressions
#' pif(iris, TRUE, dim,  warning("this will be evaluated"))
#' pif(iris, TRUE, dim, ~warning("this won't be evaluated"))
#'
#' @export
pif = function(x, p, true, false = identity){
  
  if(!requireNamespace("purrr")) 
    stop("Package 'purrr' needs to be installed to use function 'pif'")

  if(inherits(p,     "formula"))
    p     <- purrr::as_mapper(
      if(!is.list(x)) p else update(p,~with(...,.)))
  if(inherits(true,  "formula"))
    true  <- purrr::as_mapper(
      if(!is.list(x)) true else update(true,~with(...,.)))
  if(inherits(false, "formula"))
    false <- purrr::as_mapper(
      if(!is.list(x)) false else update(false,~with(...,.)))

  if ( (is.function(p) && p(x)) || (!is.function(p) && p)){
    if(is.function(true)) true(x) else true
  }  else {
    if(is.function(false)) false(x) else false
  }
  
}
