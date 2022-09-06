#' Get the details about where a call came from
#'
#' `caller_info()` returns a list with information about the calling sequence.
#' Optionally, a message with the information is directly printed.
#'
#' @returns A list with the following elements:
#' `fun` ... calling function
#' `dir` ... path to the script file
#' `file` ... name of the script file
#' `line` ... line in the script where called
#' `message` ... message with the sequence
#'
#' @family halting utilities provided by utilbox
#' @export
caller_info = function(print_msg = FALSE, level = 1, warn = TRUE) {
   
  orig_deparse = getOption('deparse.cutoff')
  options(deparse.cutoff = 10000)
  on.exit(options(deparse.cutoff = orig_deparse))

  K = .traceback(x = level + 1)

  if(length(K) == 0) {
    if(warn) {
      warning(this_fun_name(), "(): No suitable reference found with level ", level,
              " (i.e. ", level + 1, " above the call to ", this_fun_name(), ").")
    }
    return(NULL)
  }

  refs = lapply(K, utils::getSrcref)
  w = unlist(lapply(refs, Negate(is.null)))
  K = rev(K[w])
  dirs = lapply(K, utils::getSrcDirectory)
  fils = lapply(K, utils::getSrcFilename)
  lins = lapply(K, utils::getSrcLocation)
  cals = lapply(K, function(x) paste(as.character(utils::getSrcref(x)), collapse='\n'))
  evok = lapply(K, paste, collapse='\n')
  difr = lapply(seq_along(cals), function(i) !identical(str2lang(cals[[i]]), str2lang(evok[[i]])) %ERR% TRUE)
  difr = which(as.logical(unlist(difr)))
  locs = lapply(seq_along(K), function(i) paste0(file.path(dirs[[i]], fils[[i]]), ":", lins[[i]]))
  Cals = lapply(seq_along(K), function(i) paste0(i,'. -> @', locs[[i]],':\n  \\-> ', paste(cals[[i]], collapse='\n')))
  Cals[difr] = lapply(difr, function(i) paste0(Cals[[i]], '\n   \\-> ', paste(evok[[i]], collapse='\n')))
  msg = paste0('Invocation sequence: \n', paste(unlist(Cals), collapse = '\n'))
 
  info = list(calls = cals, wdir = getwd(), srcdir = dirs, srcfile = fils, srcline = lins,
              srcloc = locs, message = msg)
               
  if(print_msg) {
    msgf(msg)
  }
 
  return(info)
   
}
#' Get the details about where a call came from
#'
#' `caller_info_old()` returns a list with information about the calling sequence.
#' Optionally, a message with the information is directly printed.
#'
#' `call_info_old()` prints and returns the information about the calling sequence
#' (obtained using `caller_info()`.
#'
#' @returns A list with the following elements:
#' `fun` ... calling function
#' `dir` ... path to the script file
#' `file` ... name of the script file
#' `line` ... line in the script where called
#' `msg` ... message with the sequence
#'
call_info_old = function(print_msg=TRUE, level = 1) {

  info = caller_info(FALSE, level)
  
  if(print_msg) msgf(info$message2)
 
  return(invisible(info))
 
}

caller_info_old = function(print_msg=FALSE, level = 1) {

  K = .traceback(x = level + 1)
  
  refs = sapply(K, getSrcref)
  w = which(!sapply(refs, is.null))
  
  if(length(K)==0) {
    warning("caller_info(): No suitable reference found.")
    return(NULL)
  }
  
  K = rev(K[w])
  
  cals = sapply(K, function(x) as.character(getSrcref(x)))
  dirs = sapply(K, getSrcDirectory)
  fils = sapply(K, getSrcFilename)
  lins = sapply(K, getSrcLocation)
  locs = paste0(file.path(dirs, fils),':',lins)

  msg1 = paste0('Invocation sequence: ',paste0(paste0(cals,'@',locs), collapse=' -> '))
  msg2 = paste0('Invocation sequence:\n    -> ',paste0(paste0(cals,'@',locs), collapse='\n    -> '))
  
  srcloc = list(calls=cals, wdir=getwd(), srcdir=dirs, srcfile=fils, srcline=lins, srcloc=locs, message=msg1, message2=msg2)
 
  if(print_msg) msgf(msg2)
 
  return(invisible(srcloc))
 
}

#' Print caller information
#'
#' `caller_info2()` is a function that prints the caller information,
#' i.e. the sequence of calling fuctions and the invocation locations.
#'
#' Taken from: https://stackoverflow.com/q/59537482/684229
#'
#' Serves as inspiration for the function `caller_info()`.
#'
caller_info2 = function (fmtstring = NULL, level = 1) {
    
  x = .traceback(x = level + 1)

  i = 1
  repeat { # loop for subexpressions case; find the first one with source reference
      srcref = getSrcref(x[[i]])
      if (is.null(srcref)) {
          if (i < length(x)) {
              i = i + 1
              next;
          } else {
              warning("caller_info(): not found\n")
              return (NULL)
          }
      }
      srcloc = list(fun = getSrcref(x[[i+1]]), file = getSrcFilename(x[[i]]), line = getSrcLocation(x[[i]]))
      break;
  }

  if (is.null(fmtstring))
      return (srcloc)

  fmtstring = sub("%f", paste0(srcloc$fun, collapse = ""), fmtstring)
  fmtstring = sub("%s", srcloc$file, fmtstring)
  fmtstring = sub("%l", srcloc$line, fmtstring)
  fmtstring
  
}

