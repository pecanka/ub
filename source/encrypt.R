#' Encrypt and decrypt files
#'
#' Utilities for encrypting and decrypting files using the package encryptr.
#'
#' `encrypt_files()` encrypts the supplied files (directly via `files` and/or 
#' via `pattern`, which is matched against the files in found in the path in 
#' `path`). `decrypt_files()` achieves the opposite effect, i.e., it decrypts
#' the supplied files. Both function require key files (public key file for 
#' `encrypt_files()` and private key file for `decrypt_files()`), which can
#' be generated using `generate_keys()`.
#'
#' `encrypt_R_files()` and `decrypt_R_files()` are special cases of the above
#' functions that look for R script files (i.e., *.R) in the given `path` and
#' encrypt/decrypt them from and into the specified folders (i.e., 'R_decrypted' 
#' and 'R_decrypted' relative to the current working directory as default).
#'
#' @examples
#' cat('print("hello world")', file='foo.R')
#' generate_keys()
#' encrypt_R_files('foo.R', appendix = '.backup', public_key_file = 'id_rsa.pub')
#' decrypt_R_files('foo.R.backup.encryptr.bin', private_key_file = 'id_rsa')
#' file_compare('foo.R', 'R_decrypted/foo.R.backup')
#' 
#' @export
crypt_files = function(files = NULL, pattern = NULL, path = '.', out_path = '.', appendix = '', 
    key_file = "id_rsa.pub", delete_existing = FALSE, path_create = FALSE, exclude_key_file = TRUE,
    action = c('encrypt','decrypt')) {
   
  check_namespace('encryptr')
  action = match.arg(action)
 
  message(toupper(substring(action, 1, 1)), substring(action, 2),"ing files ...")
 
  existing_cons = showConnections(all = FALSE)
  on.exit(lapply(setdiff(showConnections(all = FALSE), existing_cons), close.connection))
 
  if(!file.exists(key_file)) {
    stop(ifelse(action == 'encrypt', 'Public', 'Private')," key file '",key_file,
         "' not found in the path '",getwd(),"'.\n  Either specify a different file",
         " name or generate the public and private key files using `generate_keys()`.")
  }
 
  if(!dir.exists(out_path)) {
    if(!path_create)
      stop("Output path '",out_path, "' does not exist.")
   
    dir.create(out_path, recursive = TRUE)
  }
 
  if(!is.null(pattern)) {
    files2 = list.files(path, pattern = pattern)
    dirs = list.dirs(path, full.names = FALSE)
    files2 = setdiff(files2, dirs)
    if(exclude_key_file) {
      files2 = setdiff(files2, key_file)
    }
    files = c(files, files2)
  }

  files = file.path(path, files)
 
  if(length(files) == 0) {
    message('No matching file found.')
    return(invisible())
  }
 
  if(action == 'encrypt') {
    outfiles = paste0(base_name(files), appendix, '.encryptr.bin')
  } else {
    outfiles = sub('[.]encryptr[.]bin$', '', base_name(files))
  }
 
  files = normalizePath(files, mustWork = FALSE)
  outfiles = normalizePath(file.path(out_path, outfiles), mustWork=FALSE)
 
  for(i in seq_along(files)) {
   
    f = files[i]
    of = outfiles[i]
   
    if(file.exists(of) && delete_existing) {
      message("Deleting existing output file '",of,"' ...")
      file.remove(of)
    }
   
    message(toupper(substring(action, 1, 1)), substring(action, 2),"ing file '",f,"' ...")

    if(action == 'encrypt') {
      res = encryptr::encrypt_file(f, crypt_file_name = of, public_key_path = key_file)
    } else {
      res = encryptr::decrypt_file(f, file_name = of, private_key_path = key_file)
    }
   
  }
 
  message('Finished.')
 
  return(invisible(outfiles))
 
}

#' @rdname crypt_files
#' @export
decrypt_files = function(files = NULL, pattern = NULL, path = '.', out_path = '.', appendix = '',
    private_key_file = 'id_rsa', delete_existing = FALSE, path_create = FALSE) {

  check_namespace('encryptr')
  crypt_files(files = files, pattern = pattern, path = path, out_path = out_path, 
              key_file = private_key_file, delete_existing = delete_existing, 
              path_create = path_create, appendix = appendix, action = 'decrypt')
 

}

#' @rdname crypt_files
#' @export
encrypt_files = function(files = NULL, pattern = NULL, path = '.', out_path = '.', appendix = '',
    public_key_file = 'id_rsa', delete_existing = FALSE, path_create = FALSE) {

  check_namespace('encryptr')
  crypt_files(files = files, pattern = pattern, path = path, out_path = out_path, 
              key_file = public_key_file, delete_existing = delete_existing, 
              path_create = path_create, appendix = appendix, action = 'encrypt')
 

}

#' @rdname crypt_files
#' @export
encrypt_R_files = function(files = NULL, pattern = NULL, path = '.', appendix = '', 
    out_path = 'R_encrypted', delete_existing = TRUE, path_create = TRUE, 
    public_key_file = file.path(out_path, 'id_rsa.pub')) {
  
  if(missing(files) && missing(pattern))
    pattern = '[.]R$'
    
  encrypt_files(files = files, pattern = pattern, path = path, out_path = out_path, 
                public_key_file = public_key_file, delete_existing = delete_existing, 
                path_create = path_create, appendix = appendix)
 
}

#' @rdname crypt_files
#' @export
decrypt_R_files = function(files = NULL, pattern = NULL, path = 'R_encrypted', 
    appendix = '', out_path = 'R_decrypted', delete_existing = TRUE, path_create = TRUE, 
    private_key_file = file.path(path, 'id_rsa')) {
   
  if(missing(files) && missing(pattern))
    pattern = '[.]R[.]encryptr[.]bin$'

  decrypt_files(files = files, pattern = pattern, path = path, out_path = out_path, 
                private_key_file = private_key_file, delete_existing = delete_existing, 
                path_create = path_create, appendix = appendix)
 
}

#' @rdname crypt_files
#' @export
generate_keys = function(private_key_file = "id_rsa", public_key_file = paste0(private_key_file,'.pub')) {
  check_namespace('encryptr')
  encryptr::genkeys(private_key_name = private_key_file, public_key_name = public_key_file)  
}

