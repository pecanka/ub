#' @title
#' Read text from file
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

#' @title
#' Save to file
#'
#' @description
#'
#' A lazy version `base::saveRDS` that writes to the specified file but only 
#' when the file does not yet exist or when the contents of the existing file 
#' would be changed by writing `object` into it.
#'
#' @family file-reading functions provided by utilbox
#' @export
saveRDSlazy = function(object, file="", backup=TRUE, check_for_sameness=TRUE, announce=TRUE, ...) {

  exist = file.exists(file)

  if(exist && check_for_sameness) {
    x = readRDS(file)
    if(identical(x, object)) {
      if(announce) {
        msgf('The file ', file,' already exists and has the same content as the new file. Saving skipped.')
      }
      return
    }
  }
 
  file_backup(file, announce=announce)
  saveRDS(object, file=file, ...)


}

#' Read data table from an RDS file
#'
#' Function for reading data.table objects, which avoids losing its "identity"
#' (i.e., where the external pointer points) when being read from the disk,
#' which can be diagnosed by looking at whether the output of `data.table::truelength()`
#' on the data.table object is 0 or not (with 0 indicating that there is an issue).
#'
#' @export
readDT = function(file) {
  x = readRDS(file)
  setDT(x)
  x
}