#' @title
#' Reads file
#'
#' @description
#'
#' Reads a text from a file as a single string. By default, reads the 
#' entire file.
#'
#' @family file-reading functions provided by utilbox
#' @export
read_char = function(file, encoding) { 
  nchars = if(!is.null(attr(file,'size'))) attr(file, 'size') else file.info(file)$size
  x = readChar(file, nchars)
  if(!missing(encoding)) Encoding(x) = encoding
  x
}

