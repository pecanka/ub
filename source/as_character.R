#' @export
as.character.function = function(fun) {
  capture.output(print(fun))
}
