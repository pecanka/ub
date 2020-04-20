#' @title
#' Method for conversion to class `character` for functions (closures)
#'
#' @description
#' This method extends the S3 class `as.character` to allow the 
#' conversion of function source code to class `character`.
#'
#' @examples
#' as.character(base::mean)
#'
#' @export
as.character.closure = function(fun) {
  capture.output(print(fun))
}
