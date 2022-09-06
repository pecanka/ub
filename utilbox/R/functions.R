#' @title
#' Get function name
#'
#' @description
#' `this_fun_name()` returns the name of the parent function, i.e. the
#' function from which it is invoked. If invoked from `.GlobalEnv`, it 
#' returns an empty string.
#'
#' `parent_fun_name()` returns the parent function of the parent function.
#'
#' @examples
#' example_function = function() print(this_fun_name())
#' example_function()     # should return 'example_function'
#'
#' @family coding-related functions provided by utilbox
#' @name fun_name
#' @export
this_fun_name = function(e=sys.parent()) {
  if(e==0) "" else as.character(head(sys.call(which=e), 1)) 
}

#' @rdname fun_name
#' @export
parent_fun_name = function(n=2L) {
  this_fun_name(sys.parent(n))
}

#' Finding functions by substrings
#'
#' `str_find_in_function()` searches the function body of the specified 
#' function (via `fun`) for the given `string`. If the string is found,
#' it returns the corresponding line(s) (as obtained by printing the 
#' result of a call to `base::body()`), otherwise `NULL` is returned. 
#'
#' `str_find_in_package()` searches for the string in functions within
#' a package. Either all functions when (`what='all'`) or only exported
#' functions (when `what='exported'`) are searched.
#'
#' @examples
#' str_find_in_function('contrasts', 'lm', envir=asNamespace('stats')) # lines 3, 49, 59
#' str_find_in_package('contrasts', 'stats') # returns 28 functions from `stats`
#'
#' @name str_find_in
#' @export 
str_find_in_function = function(string, fun, envir=parent.frame()) {

  if(is.character(fun))
    fun = get(fun, envir=envir)
    
  grep(string, print2var(fun))
  
}

#' @rdname str_find_in
#' @export 
str_find_in_package = function(string, pckg, what='all') {

  funs = list_package_objects(pckg, mode = 'function', what = what, quiet = TRUE)$object_name
  names(funs) = funs
  
  locs = lapply(funs, str_find_in_function, string = string, envir = asNamespace(pckg))
  
  list_clean(locs)
  
}

#' Argument hijack function
#'
#' @description
#'
#' A function that allows for changing of default values for 
#' arguments of other functions. Similar to [`base::formals`], which it
#' leverages, but arguably with nicer syntax. Besides revaluing the defaults
#' of existing arguments, it also allows adding new arguments.
#'
#' @examples
#' cat = hijack(cat, sep='')        # equivalent to cat0
#' mean = hijack(mean, na.rm=TRUE)  # equivalent to mean2
#'
#' @family coding-related functions provided by utilbox
#' @export
hijack = function(fun, ...) {
  
  fun_hj = fun
  args = list(...)
  w_old = names(args) %in% names(formals(fun_hj))
  
  lapply(seq_along(args[w_old]), function(i) formals(fun_hj)[[names(args)[i]]] <<- args[[i]])
  
  if(any(!w_old)) {
    formals(fun_hj) = append(formals(fun_hj), args[!w_old])
  }
  
  fun_hj
  
}

#' @title
#' Number of arguments a function accepts
#'
#' @description
#' Returns the number of arguments that a given function `fun` 
#' accepts. If the arguments of `fun` contain an ellipse (`...`), it 
#' returns `Inf`.
#'
#' @examples
#' nformals(mode)
#' nformals(mean)
#'
#' @family coding-related functions provided by utilbox
#' @export 
nformals = function(fun, envir=parent.frame()) {

  if(is.character(fun)) {
    fun = get(fun)
  }
  
  fs = formals(fun)
  ifelse('...' %in% names(fs), Inf, length(fs))
  
}

#' @title
#' Get the source code of a function
#'
#' @description
#' `function_code_to_text()` returns the body of the supplied function as 
#' a character string.
#'
#' @param fun function for which the body of the code is desired
#'
#' @details This function takes an R function on input and returns 
#' its source code as character vector.
#'
#' @returns A character vector with each command of the input 
#' function as an element.
#'
#' @examples
#' f <- function(x) { 
#'   print(x)
#'   x = 2
#'   invisible(x)
#' }
#' ff <- function_code_to_text(f)
#' print(ff)
#'
#' @family coding-related functions provided by utilbox
#' @export
function_code_to_text = function(fun, file=NULL) {

  stopifnot(is.function(fun))
  
  b = c(deparse(args(fun))[deparse(args(fun))!="NULL"], deparse(body(fun)))
  
  if(!missing(file) && !is.null(file)) {
    cat(paste(b, collapse="\n"), file = file)
    invisible(b)
  } else {  
    b
  }
  
}

#' Dump the source code of a function to a file
#'
#' `function_dump_code()` dumps the source code of a function `fun` 
#' to a file `file` (`stdout()` by default).
#'
#' @family coding-related functions provided by utilbox
#' @export
function_dump_code = function(fun, file=stdout()) {
  cat(paste(print2var(fun), collapse='\n'), '\n', file=file)
}

#' Separate lines of the code of a function 
#'
#' `function_separate_code()` converts the body of a function (`fun`) to a list
#' where each element in the list corresponds to a block of code (often a
#' line). This is used when altering the code for instance via `append_body()`
#' below or via `base::trace()`.
#'
#' @family coding-related functions provided by utilbox
#' @export
function_separate_code = function(fun) {
  b = as.list(body(fun))
  num = seq_along(b) - ifelse(identical(b[[1]], as.name('{')), 1, 0)
  `names<-`(b, paste0("at=", num))
}

#' @title
#' Append the body of a function
#'
#' @description
#' Takes an existing function `fun` and changes its body by appending 
#' to it the calls supplied in `calls`. The calls can either be of class 
#' `calls` or class `character` (converted to `call` via 
#' `as.character`). The way the new cody is appended is governed by 
#' `where`, which allows these options:
#'
#' \"first\": the code in `calls` is placed before the existing code.
#'
#' \"last\": the code in `calls` is placed after the existing code.
#'
#' \"at\": `calls` must be of length 1 and be of class character and 
#' its content is parsed and evaluated. It can reference the selected 
#' old line (which one that is is determined by the value in `at`) as 
#' `.oldline.` (see the example below). A call with non-missing `at` 
#' and missing  `where` implies `where='at'`.
#'
#' Remember to use `<-` in any assignment strings (not `=`) supplied to
#' `append_body()`.
#'
#' @examples
#' f = function() { x = pi }
#' print(f())                           # returns pi (i.e. 3.141593)
#' f = append_body(f, quote(x^2))
#' print(f())                           # returns pi^2 (i.e. 9.869604)
#' f = append_body(f, c(quote(x <- x^2), quote(x-1)))
#' print(f())                           # returns pi^2 - 1 (i.e. 8.869604)
#'
#' # Modify an existing function:
#' call = base::str2lang("cat(\"And the symmetric set difference is...\n\")")
#' f = append_body(setdiffsym, call, where='first')
#' f(1:5, 3:10)
#'
#' # Above the line is placed at the top so that it does not affect
#' # the return value from the function. A more complex editing is
#' # is possible. We can append the last call in the function
#' # to turn it into an assignment and thus assign the calculated
#' # value into a variable. We can then save add the printing line 
#' # without affecting the returned value of the function. The line 
#' # in the original code that is altered is determined thru 'at'.
#' call = "substitute(y -> x, list(y=.oldline.))"
#' f = append_body(setdiffsym, call, at=1)
#' f = append_body(f, c("cat(\"And the symmetric set difference is...\n\")", "x"), where='last')
#' f(1:5, 3:10)
#'
#' @family function-modification functions provided by utilbox
#' @export
append_body = function(fun, calls, where=c('first','last','at'), at=NULL, replace_at=TRUE) {
  
  where = if(missing(where) && !missing(at)) 'at' else match.arg(where)

  if(where=='at' && length(at)!=1)
    stop("Supply a single value via `at` when `where='at'`.")
  
  if(length(body(fun))==0) {
    warning('The supplied function has an empty body. It is probably a generic, which',
            ' `append_body()` does not support. Returning the unmodified function.')
    return(fun)
  }

  if(is.character(calls)) {
    calls = lapply(calls, base::str2lang)
  }
    
  old_body = as.list(body(fun))
  
  old_body = if(identical(old_body[[1]], as.name('{'))) {
    old_body[-1]
  } else {
    list(body(fun)) 
  }
  
  new_body = if(where=='first' || at<1) {
    append(calls, old_body)
  } else if(where=='last' || at > length(old_body)) {
    append(old_body, calls)
  } else {
    append(append(head(old_body, at-1), calls), tail(old_body, -at+1))
  }

  body(fun) = as.call(c(as.name("{"), new_body))
  
  return(fun)

}
  
append_body_old = function(fun, calls, where=c('first','last','at'), at=NULL, replace_at=TRUE) {
  
  where = if(missing(where) && !missing(at)) 'at' else match.arg(where)
  
  old_body = as.list(body(fun))
  
  # drop the brackets if there are any
  if(identical(old_body[[1]], as.name('{'))) {
    old_body = old_body[-1]
  }
  
  # append the call at the top ('first') or at the bottom ('last')
  if(where %in% c('first', 'last')) {
    
    if(is.character(calls)) {
      calls = lapply(calls, base::str2lang)
    }
    
    new_body = if(where=='first') {
      append(calls, old_body)
    } else {
      append(old_body, calls)
    }
  
  # alter the original code line number 'at' with the parsed and evaluated
  # content in calls
  } else {
  
    if(length(calls)!=1) 
      stop("With where='at' exactly one code line must be supplied.")
    if(missing(at))
      stop("With where='at' a value for 'at' must be supplied.")
    
    calls = calls[[1]]

    at = min(at, length(old_body))
    .oldline. = old_body[[at]]
    
    new_code = gsub('.oldline.', '.(.oldline.)', calls, fixed=TRUE)
    new_code = unlist(strsplit(new_code, ';'))
    new_code = lapply(new_code, base::str2lang)
    new_code = lapply(new_code, function(nc) do.call(bquote, list(nc)))
    
    new_body = insert(old_body, list(new_code), at, replace_old=replace_at)

  }
  
  # assign the modified code back into fun and return the function
  body(fun) = as.call(c(as.name("{"), new_body))
  
  fun
  
}

#' Null function (function that does nothing)
#'
#' `null()` is a function that takes arbitrary arguments and does nothing.
#'
#' @family coding-related functions provided by utilbox
#' @export
null = function(...) {
  return(invisible())
}

#' Modify and restore functions
#'
#' Functions to **modify in place** and restore other functions located  
#' anywhere including inside locked packages.
#'
#' `function_modify_in_place()` is the workhorse function that can be used to 
#' modify other functions **in place**. It attaches the original source 
#' code of the function that is being modified as the attribute 
#' `original_function` of the modified function.
#'
#' `function_restore()` restores the original code of a modified
#' function (specifically using the attribute `original_function`).
#'
#' `function_disable()` can be used to disable any function, i.e., make
#' the function (supplied by name) do no action. The modification is done
#' also **in place**.
#
#' `function_kidnap()` kidnaps the supplied function, where kidnapping 
#' means that the function will ask the user for input before proceeding 
#' with its code. The original code of the kidnapped function is backed 
#' up in the attribute `original_function` of the kidnapped function.
#' The modification is done also **in place**.
#'
#' @name function_modify
#' @family coding-related functions provided by utilbox
#' @export
function_modify_in_place = function(fun_name, calls='', envir=parent.frame(), 
    where=c('first','last','at'), at = NULL, restore_call, remodify=FALSE, 
    quietly=FALSE) {

  if(quietly)
    message = function(...) {}

  stopifnot(is.character(fun_name))
  
  if(missing(where) && !missing(at))
    where = 'at'
  
  if(missing(envir) && grepl('::', fun_name)) {
    envir = sub('::.*', '', fun_name)
    fun_name = sub('.*::','',fun_name)
  }

  if(is.character(envir)) {

    if(missing(restore_call))
      restore_call = paste0("function_restore('",fun_name,"', '",envir,"')")
      
    envir_name = paste0("in the namespace '",envir,"'")
    envir = asNamespace(envir)
    
  } else if(identical(envir, parent.frame())) {
    envir_name = "in the parent frame"
  } else {
    envir_name = "in the specified namespace / environment"
  }

  if(is.character(calls)) {
    calls = paste('{', paste(calls, collapse='\n'), '}')
    calls = base::str2lang(calls)
  }

  fun = get(fun_name, envir=envir)

  if(!is.null(attr(fun, 'original_function')) && remodify) {
    message("The function '",fun_name,"' had already been modified. To modify it",
            " further, set the argument 'remodify=FALSE'.")
    return(FALSE)
  }

  fun_modified = append_body(fun, calls, where=where, at=at)
  attributes(fun_modified) = attributes(fun)

  if(is.null(attr(fun, 'original_function'))) {
    attr(fun_modified, 'original_function') = fun
  }

  message('Modifying the function `',fun_name,'` (',envir_name,') ...')
  
  #unlockBinding(fun_name, env=envir)
  #assign(fun_name, fun_modified, envir=envir)
  #lockBinding(fun_name, env=envir)
  assign_locked(fun_name, fun_modified, envir)
 
  if(identical(get(fun_name, envir=envir), fun_modified)) {
  
    message('Function `',fun_name,'` has been modified (',envir_name,').')
    result = TRUE
    
    if(missing(restore_call)) 
      restore_call = 'function_restore()'
      
    if(!is.null(restore_call)) 
      message('You can restore the original version(s) using `',restore_call,'`.')
    
  } else {
    warning('Something went wrong during the modification of `',fun_name,'`!', immediate.=TRUE)
    result = FALSE
  }
  
  invisible(result)
 
}


#' @rdname function_modify
#' @export
function_restore = function(fun_name, envir=parent.frame(), quietly=FALSE) {

  if(quietly)
    message = function(...) {}

  if(missing(envir) && grepl('::', fun_name)) {
    envir = sub('::.*', '', fun_name)
    fun_name = sub('.*::','',fun_name)
  }
  
  if(is.character(envir))
    envir = asNamespace(envir)

  if(is.null(attr(get(fun_name, envir=envir), 'original_function'))) {
    message("The function `",fun_name,"` does not appear to have been modified,",
            "or the attribute `original_function` has been lost at some point.")
    return(invisible())
  }

  message('Restoring the original version of the function `',fun_name,'` ...')
  
  fun_original = attr(get(fun_name, envir=envir), 'original_function')
  
  #unlockBinding(fun_name, env=envir)
  #assign(fun_name, fun_original, envir=envir)
  #lockBinding(fun_name, env=envir)
  assign_locked(fun_name, fun_original, envir)
 
  if(identical(get(fun_name, envir=envir), fun_original)) {
    message('The original version of the function `',fun_name,'` has been restored.')
    result = TRUE
  } else {
    warning('Something went wrong when restoring of `',fun_name,'`!', immediate.=TRUE)
    result = TRUE
  }
  
  invisible(result)

}

#' @rdname function_modify
#' @export
function_disable = function(fun_name, envir=parent.frame(), quietly=FALSE) {

  if(missing(envir) && grepl('::', fun_name)) {
    envir = sub('::.*', '', fun_name)
    fun_name = sub('.*::','',fun_name)
  }

  function_modify_in_place(fun_name, 'return()', envir=envir, quietly=quietly)

}

#' @title`
#' Find dependencies of a function
#'
#' @description
#' `function_find_dependencies()` searches the source code of a
#' function (supplied either directly or by name as string via `fun`)
#' and identifies invocations of functions that are found in the 
#' specified environment or package (supplied via `dep_envir`). When
#' supplied by name, the environment in `dep_envir` is searched for
#' a function of matching name. The function name can be supplied
#' together with a package using the double colon notation (e.g.,
#' 'base::mean`), which is then used to set `envir` (unless
#' it has been supplied. When the argument `dep_envir` was not specified,
#' the environment in `envir` is used.
#'
#' @examples
#' # Function supplied directly or by name, `envir` is implied (i.e.,
#' # set to `environment(base::sample)`), `dep_envir` missing so taken 
#' # the same as `envir`. Thus it looks for dependencies of `sample` 
#' # in the environment "base".
#' function_find_dependencies(base::sample)
#' function_find_dependencies('base::sample') 
#'
#' # function supplied directly, `envir` is implied (i.e., set to
#' # `environment(base::sample)`), `dep_envir` is specified explicitly.
#' function_find_dependencies(base::sample, 'stats')
#' function_find_dependencies('base::sample', 'stats')
#' function_find_dependencies('sample', 'stats', 'base')
#'
#' # An example of a missing function (xxxx)
#' g = function() print('hi')
#' ff = function() { x = xxxx(); a = sin(1); g() }
#' function_find_dependencies(ff)
#'
#' @family coding-related functions provided by utilbox
#' @export
function_find_dependencies = function(fun, dep_envir, envir=parent.frame(), get_status = TRUE, 
    announce = TRUE, warn = TRUE) {

  if(length(fun)==0)
    return(NULL)
    
  if(length(fun) > 1) {
    deps = lapply(fun, function_find_dependencies, dep_envir, envir, get_status)
    deps = do.call(rbind, deps)
    return(deps)
  }
  
  if(announce)
    msgf("Identifying dependencies for '",fun,"' ...")

  if(is.character(fun)) {
    fun_name = fun  
    if(missing(envir) && grepl('::', fun)) {
      envir = sub('::.*', '', fun)
      fun = sub('.*::','',fun)
    }
  } else {
    fun_name = NA_character_
  }
  
  if(is.character(envir)) {
    envir = asNamespace(envir)
  }

  if(is.character(fun)) {
    fun = get(fun, envir=envir)
  }
  
  if(!is.function(fun)) {
    if(warn) {
      warning("Argument `fun` must be a function or a name of an existing function.", 
              if(!is.na(fun_name)) paste0(" Function '",fun_name,"' not found."),
              immediate. = TRUE)
    }
    return(NULL)
  }

  envir = environment(fun)
  
  if(missing(dep_envir))
    dep_envir = envir

  if(is.character(dep_envir)) {
    dep_envir = asNamespace(dep_envir)
  }
  
  # Identify the portions of the function's code that correspond 
  # to function calls
  fun_calls = fun2funcalls(fun)
  funs = unique(fun_calls$calls)
  funs = unlist(lapply(lapply(funs, str2lang), as.character))
  
  if(get_status && length(funs)>0) {
    
    assign_calls = fun2assigncalls(fun)
    lhs = list2DF(assign_calls)[,c('order','lhs')]
    fcs = list2DF(fun_calls)[,c('order','calls')]
    info = merge(fcs, lhs, by.x='calls', by.y='lhs', all.x=TRUE)
    info$calls = gsub('`','',info$calls)
    info$ok = sapply(info$order.x > info$order.y, isTRUE)
    ok = tapply(info$ok, info$calls, all)
    
    Get = function(x) {
      if(grepl('::', x)) {
        list(where = try(environment(eval(str2lang(x))), silent=TRUE) %ERR% NULL)
      } else {
        unlist(getAnywhere(x))
      }
    }
    
    locs = structure(lapply(funs, Get), names=funs)
    locs = lapply(locs, function(l) l[grepl('^where',names(l))])
    names(locs) = funs
    miss = unlist(lapply(locs, function(x) !any(grepl('where[0-9]*', names(x)))))
    miss[names(miss) %in% names(as.list(args(fun)))] = FALSE
    ok = ok[names(miss)]
    miss[ok] = FALSE
    
    nam_miss = names(miss[miss])
    

    locs = lapply(locs, function(x) unname(unlist(x[grepl('where[0-9]*', names(x))])))
    
    packgs = lapply(locs, function(x) x[!grepl('^namespace:',x)])
    packgs = lapply(packgs, sub, pattern='^[^:]+:', repl='')
    packgs = lapply(packgs, function(nam) try(asNamespace(nam), silent=TRUE) %ERR% nam)
    packgs = lapply(packgs, function(nam) try(as.environment(nam), silent=TRUE) %ERR% nam)

    namsps = lapply(locs, function(x) x[grepl('^namespace:',x)])
    namsps = lapply(namsps, sub, pattern='^namespace:', repl='')
    namsps = lapply(namsps, function(nam) try(asNamespace(nam), silent=TRUE) %ERR% nam)
    namsps = lapply(namsps, function(nam) try(as.environment(nam), silent=TRUE) %ERR% nam)

    locs = unlist(lapply(locs, paste, collapse=';'))
    
    in_dep_pckg = unlist(lapply(lapply(packgs, identical, dep_envir), any))
    in_dep_nsps = unlist(lapply(lapply(namsps, identical, dep_envir), any))
    in_dep = in_dep_pckg | in_dep_nsps
    
    funs = data.frame(function_name = fun_name, dependency = funs, currently_missing = miss, 
                      locations = locs, found_in_envir = in_dep)
    rownames(funs) = NULL

  }
  
  funs
  
}
