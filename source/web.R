# This is just a short selection of simple web utilities. 
# For more a sophisticated list of tools look at other packages such as httr and RCurl.

#' @title
#' Public IP address
#'
#' @description
#'
#' Gets ones public IP address using the ipify.com API. The default 
#' server can be changed.
#'
#' The function probably needs a rewrite for when the call to the 
#' server fails for some reason.
#'
#' @export
get_my_ip = function(server = "https://api.ipify.org", trace=1) {
  if(trace>0) catn("Checking your IP address via ",server," ...")
  ip = scan(con <- url(server), what = 'character', quiet = TRUE)
  if(exists("con", envir=environment())) close(con)
  if(trace>0) catn("Your IP address is ",ip,".")
  invisible(ip)
}

#' @title
#' Download file
#'
#' @description
#'
#' A wrapper for `download.file()` that handles missing files without 
#' stopping on error. It uses the input URL to define the destination 
#' file.
#'
#' @examples
#' download_file('http://www.karlin.mff.cuni.cz/~kpms/index.php')
#' @export
download_file = function(url, destfile=separate_paths(url)$files, ...) {
  try(download.file(url, destfile, ...))
  invisible(destfile)
}

#' @title
#' Check the existence of a file on the web
#'
#'
#'
#' @examples
#' 
#' @export
web_file_exists = function(url, ...) {
  ## code not created yet ##
}

#' @title
#' List file on an FTP server
#'
#' @description
#'
#' Gets a nice looking list of files present on an FTP server.
#'
#' The structure returned is dependent on the FTP site as there are 
#' various formats for directory listings dependent upon the server and 
#' the OS. you will need to play with this. have a look at the FTP with 
#' your browser first and adjust accordingly. some formats only return 4 
#' columns.
#'
#' column 1: literal string first position mean file column 2: number 
#' 1 column 3: owner column 4: group column 5: file size column 6: month 
#' column 7: day column 8: time (year) column 9: file name
#'
#' @export
get_ftp_file_list = function(ftp) {

  txt = getURL(ftp)

  dir = read.table(textConnection(txt), as.is=TRUE)
  out = data.frame(dir=ftp, filename=dir[,ncol(dir)], size=dir[ ,5],
                   month=dir[ ,6],day=dir[ ,7], time=dir[,8], stringsAsFactors=FALSE)
  closeAllConnections()
  return(out)
  
} 

#' @export
url_exists = function(url, ok_status=200) {
  response = httr::HEAD(url)
  structure(response$status_code %in% ok_status, 
            'server_status_code' = response$status_code)
}

#' @export
url_exists1 = function(url) {
  
  if('^https:' %m% url) 
    error("Only plain/unencrypted (http) URLs allowed.")
  
  RCurl::url.exists(url, useragent="curl/7.39.0 Rcurl/1.95.4.5")
  
}
