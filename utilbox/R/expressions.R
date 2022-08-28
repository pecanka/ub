#' Evaluate an expression given as a string
#'
#' Takes an expression in a character vector, pastes it into a single
#' string and evaluates it in the `envir` environment (the parent 
#' frame by default).
#'
#' @examples
#' x = NULL
#' print(x)
#' cc('x','=1')
#' print(x)
#'
#' @export
cc = function(..., envir=parent.frame()) {
  eval(base::str2lang(paste0(...)), envir=envir)
}

#' Convert a function to a call
#'
#' Takes a function and returns an object of class `call` with the
#' code of the function.
#'
#' @examples
#' # compare with the output of print(sample)
#' code = fun2call(sample)
#'
#' @export
fun2call = function(fun) {
  str2lang(deparse1(fun, collapse='\n'))
}

#' Split expressions into terms
#'
#' `split_expression()` takes one or more expressions (optionally as 
#' strings, unless `is_lang=TRUE`) and splits them up into 
#' individual language elements (via `expr2terms_string()` or 
#' `expr2terms()`). With `only_names=TRUE`, only elements that 
#' have syntactic names (i.e., those starting with a letter or dot) 
#' are returned.
#'
#' `expr2terms()` splits up a single expression. `expr2terms_string()`
#' takes string input and converts it to language first before calling
#' `expr2terms()`. 
#'
#' `fun2terms()` splits up an entire function into constituent parts (all).
#'
#' `fun2funcalls()` splits up an entire function into constituent parts
#' and returns those parts that correspond to function calls.
#'
#' `fun2funcalls2()` is an alternative implementation of `fun2funcalls()`
#' with slightly different results.
#'
#' @examples
#' split_expression(c("I(A/B/1000/(D*G))", 'I(Wealth - Health)*Age'))
#' unlist(lapply(as.list(body(optimize)), FUN=split_expression, assume_lang=TRUE))
#' 
#' # get all expressions in base::sample (including arguments)
#' fun2terms(base::sample)    # a list of length 47
#'
#' # extract calls to functions from a function
#' print(fun2funcalls(mean.default))
#' 
#' @name expressions
#' @export
split_expression = function(..., is_lang=FALSE, only_names=FALSE) {

  x = unlist(list(...), recursive=TRUE)
  
  x = if(is_lang) {
    lapply(x, FUN=expr2terms, only_names=only_names)
  } else {
    lapply(x, FUN=expr2terms_string, only_names=only_names)
  }
  
}

#' @rdname expressions
#' @export
expr2terms = function(x, only_names=FALSE) {

  if(length(x)==1) 
    return(x)
    
  x = as.list(x)
  x = unlist(lapply(x, expr2terms))
  x = unlist(lapply(unique(append(x, names(x))), as.character))
  
  if(only_names) {
    is_name = sapply(x, FUN=grepl, pattern='^[a-zA-Z.]')
    x = x[is_name]
  }
  
  x
}

#' @rdname expressions
#' @export
expr2terms_string = function(x, only_names=FALSE) {

  stopifnot(length(x)==1)

  x = str2lang(x)
  
  expr2terms(x, only_names=only_names)
    
}

#' @rdname expressions
#' @export
fun2terms = function(fun) {

  parts = fun
  repeat {
    prev_length = length(parts)
    parts = list_flatten(lapply(parts, as.list))
    if(length(parts) == prev_length) break
  }
  
  parts
}

#' @rdname expressions
#' @export
fun2terms_drop_assignments = function(fun) {

  parts = fun
  repeat {
    prev_length = length(parts)
    parts = list_flatten(lapply(parts, as.list))
    
    is_assign = unlist(lapply(parts, is_assign_call))
    
    lapply(parts[is_assign], function(x) stopifnot(length(as.list(x))==3))
    
    parts[is_assign] = lapply(parts[is_assign], function(x) as.list(x)[[3]])
    
    if(length(parts) == prev_length) break
  }
  
  parts
}

#' @rdname expressions
#' @export
fun2funcalls = function(fun) {

  stopifnot(is.function(fun))
  
  tree = lapply(fun, my_ast_tree, list(n='_'))
  tree = str_trim_space(unlist(tree))
  w_calls = grep(pattern='^[ _]*_', tree)
  calls = sub('^[ _]*_','', tree[w_calls])
  
  if('`::`' %in% calls) {
    wddc = which(calls == '`::`')
    wddt = grep('`::`', tree)
    calls[wddc] = paste0('`',tree[wddt+1],gsub('`','',calls[wddc]),tree[wddt+2],'`')
  }
  
  # Drop the calls that correspond to empty arguments inside function
  # definitions, and which are not valid calls
  calls = calls[!grepl('=[ ][`][`]', calls)]
  
  return(list(order = w_calls, calls = calls))
  
}

#' @rdname expressions
#' @export
fun2assigncalls = function(fun) {

  stopifnot(is.function(fun))
  
  tree = lapply(fun, my_ast_tree, list(n='_'))
  tree = str_trim_space(unlist(tree))
  w_calls = grep(pattern='^[ _]*_`<-`', tree)
  
  calls = sub('^[ _]*_','',tree[w_calls])
  lhs = tree[w_calls+1]
  rhs = sub('^[ _]*_','',tree[w_calls+2])
  
  list(order = w_calls, calls=calls, lhs = lhs, rhs = rhs)
  
}

#' @rdname expressions
#' @export
fun2funcalls2 = function(fun) {

  stopifnot(is.function(fun))
  
  call = fun2call(fun)
  tree = lobstr::ast(!!rlang::enquo(call))

  tree = unlist(as.list(tree))

  w_calls = unlist(lapply(tree, grepl, pattern='o-'))
  
  calls = tree[w_calls]
  
  calls = sub('.*o-','', calls)
  
  return(calls)
  
}

#' @title Empty symbols
#'
#' @description
#' Some operations in R can result in a special object: the empty symbol.
#' The symbol can be obtained directly (e.g., via `quote(expr=)` as in
#' `empty_symbol()`) or indirectly (e.g., when applying `as.list()` to
#' a function, `as.list(function(x) {})$x`). 
#'
#' `empty_symbol()` returns the empty symbol object. `is_empty_symbol()`
#' checks its argument for identity with it.
#'
#' @examples
#' print(empty_symbol())  # prints the empty symbol
#' is_empty_symbol(empty_symbol())              # TRUE
#' is_empty_symbol(as.list(function(x) {})$x)   # TRUE
#'
#' @export
empty_symbol = function(x) {
  quote(expr = )
}

#' @rdname empty_symbol
#' @export
is_empty_symbol = function(x) {
  identical(x, quote(expr = ))
}

#' @title
#' Guess whether an expression is an assignment
#'
#' Checks whether an expression has an assignement form, namely
#' that it is a call to the `<-` or other such functions.
#'
#' @examples
#' is_assign_call(expression(x <- 1))    # TRUE
#' is_assign_call(expression(x = 1))    # TRUE
#' is_assign_call(expression(2*x))       # FALSE
#'
#' @export
is_assign_call = function(expr, stop_on_error=FALSE) {

  funs_assign = c('<-', '=')
  
  expr0 = expr
  
  if(length(as.list(expr))==1) 
    return(FALSE)
    
  expr = print2var(expr)
  expr = formatR::tidy_source(text=expr, arrow=TRUE, output=FALSE)$text.tidy
  expr = try(str2lang(expr), silent=TRUE)
  
  if('try-error' %in% class(expr)) {
    if(stop_on_error) 
      stop('str2lang failed to convert the string to language.')
    return(FALSE)
  }

  call = as.list(as.call(expr))
  string = unlist(lapply(call, deparse))
  parts = expr2terms(string)
  
  parts[1] %in% funs_assign

}

#print(is_assign_call(expression(x <- 44)))
#print(is_assign_call(body(ff)))
#xx = fun2terms_drop_assignments(ff)
#yy = fun2terms_drop_assignments(mean.default)

#' Replacement in expressions
#'
#' Similarly to `base::gsub()` and `base::sub()`, the functions `gsub_lang()` 
#' and `sub_lang()` perform replacement in expressions. The expressions in
#' `code` are first converted to strings, the replacement is executed 
#' (via `base::gsub()` or `base::sub()`) and the result is converted back 
#' to language expressions. The input via `code` can be a list of 
#' expressions or a function. `pattern` and `repl` are the analogues
#' `pattern` and `replacement` in `base::sub()` and `base::gsub()`.
#'
#' @examples
#' # Modify the `get0` function to return an NA when the requested
#' # object is not found, and to change the name of the argument
#' # 'ifnotfound' to 'default_value':
#' get3 = gsub_lang(base::get0, 'ifnotfound', 'default_value')
#' get3 = hijack(get3, default_value = NA)
#' 
#' # Modify the mean function to make a median
#' median2 = lang_sub(mean.default, 'trim = 0','trim = 0.5', fixed=TRUE)
#' x = rnorm(1000)
#' identical(median2(x), median(x))     # TRUE
#' 
#' @export
lang_sub = function(x, ...) {
  UseMethod('lang_sub')
}

lang_sub.function = function(code, pattern, replacement, fixed = TRUE, workhorse = gsub) {
  
  fun_head = print2var(list(args(code))[[1]])
  fun_head[length(fun_head)] = '{}'
  fun_head = paste(fun_head, collapse='')
  fun_body = as.list(body(code))
  
  if(length(fun_body)==0) {
    warning('The supplied function has an empty body. It is probably a generic, which',
            ' `lang_sub()` does not support. Returning the unmodified function.')
    return(code)
  }
  
  fun = try({
    expr_head = lang_sub(fun_head, pattern, replacement, fixed, workhorse) 
    expr_body = lang_sub(fun_body, pattern, replacement, fixed, workhorse) 
    
    fun = do.call(eval, expr_head)
    body(fun) = as.call(expr_body)
    environment(fun) = environment(code)
    
    fun }, silent=TRUE)

  if('try-error' %in% class(fun)) {
    warning('Substitution by `lang_sub` failed. Returning unchanged object.', immediate = TRUE)
    code 
  } else {
    fun
  }
  
}

lang_sub.default = function(code, pattern, replacement, fixed=TRUE, workhorse=gsub) {

  code2 = try({
  
    code2 = list()

    for(i in seq_along(code)) {

      if(identical(code[i], empty_symbol())) {
        code2[[i]] = code[i]
      } else {
        block = as.character(code[i])
        block = gsub(pattern, replacement, block, fixed=fixed)
        block = base::str2lang(block)
        code2[[i]] = block
      }
      
    }
    
    code2
    
  }, silent=TRUE)
  
  if('try-error' %in% class(code2)) {
    warning('Substitution by `lang_sub()` failed. Returning unchanged code.', immediate.=TRUE)
    code 
  } else {
    code2
  }
  
}

#' @rdname lang_sub
#' @export
gsub_lang = function(...) {
  lang_sub(..., workhorse=gsub)
}

#' @rdname lang_sub
#' @export
sub_lang = function(...) {
  lang_sub(..., workhorse=sub)
}

#' This is a slightly modified version of the function `lobstr:::ast_tree()`.
#' It is used by the function `fun2funcalls()`, which required the modifications.
my_ast_tree = function (x, layout = box_chars()) {
    
    if (rlang::is_quosure(x)) {
        x <- rlang::quo_squash(x)
    }

    if (rlang::is_syntactic_literal(x)) {
        return(lobstr:::ast_leaf_constant(x))
    } else if (rlang::is_symbol(x)) {
        return(lobstr:::ast_leaf_symbol(x))
    }

    xt = print2var(x, width=10000)
    if(is.call(x) && grepl('=',xt)) {
      x = str2lang(formatR::tidy_source(text = xt, arrow = TRUE, output = FALSE)[[1]])
    }

    subtrees <- lapply(x, my_ast_tree, layout = layout)
    subtrees <- lobstr:::name_subtree(subtrees)
    
    n <- length(x)
    
    if (n == 0) {
        character()
    } else if (n == 1) {
        lobstr:::str_indent(subtrees[[1]], paste0(layout$n, layout$h), "  ")
    } else {
        c(lobstr:::str_indent(subtrees[[1]], paste0(layout$n, layout$h), paste0(layout$v, " ")), 
          unlist(lapply(subtrees[-c(1, n)], lobstr:::str_indent, paste0(layout$j, layout$h), paste0(layout$v, " "))), 
          lobstr:::str_indent(subtrees[[n]], paste0(layout$l, layout$h), "  "))
    }
}

#' export
#bquote2 = function (expr, where = parent.frame()) {
#  unquote <- function(e) {
#    if (is.pairlist(e)) {
#      as.pairlist(lapply(e, unquote))
#    } else if (length(e) <= 1L) {
#      e
#    } else if (e[[1L]] == as.name(".")) {
#      eval(e[[2L]], where)
#    } else {
#      as.call(lapply(e, unquote))
#    }
#  }
#  unquote(substitute(expr))
#}

###
# Example of the usage of the trace function
#
# Let's insert a call to the browser function at line 4 of the function force_as_real"
#
# trace(force_as_real, browser, at=4)
# force_as_real('a1.4')
# untrace(force_as_real, browser)
#
# Ways to change the arguments of a function
#
# formals(cat)$sep <- ""
# trace(base::cat, tracer=quote(if(missing(sep)) sep=''), at=1)
# cat = purrr::partial(cat, sep="")
#
# assign("cat", cat0, envir=.GlobalEnv)
# assign("cat", cat0, envir=.utilbox)

# cat = utils::getFromNamespace("cat", ns="base")
# trace(cat, tracer=quote(if(missing(sep)) sep <- ""), at=1, print=FALSE)
# #utils::assignInNamespace("cat", cat0, ns="base")

# Useful functions:
#
# utils::getAnywhere() retrieves an R Object, including from a Namespace, whether visible 
#   on the search path, registered as an S3 method or in a namespace but not exported.
# utils::argsAnywhere returns the arguments of objects that are functions.
#
# envnames::environment_name() returns the name of the variable that stores the
#   environment (input example: "< environment: 0x00000000147499b0>")
# envnames::obj_find() looks up objects in all packages and environments including
#   the call's function cascade of frames
#
# base::typeof() returns the type or storage mode of any object (e.g. 'numeric', 'character')
#
# sloop::ftype() returns the type of a function (e.g. "primitive" or c("S3","generic"))
# sloop::s3_dispatch() helps with inspecting how an S3 method is dispatched.
# sloop::s3_get_method() shows the code of the method that gets called regardless
#   whether it is exported by a package or not. Basically, emulated the `:::` call
#   but simpler, since the method name does not have to be looked up first
# sloop::s3_methods_generic() let you find all methods defined for a generic
# sloop::s3_methods_class() does the same for a class
#
# base::NextMethod() delegated work in a method dispatch to the next method in line
#   for an S3 class (see https://adv-r.hadley.nz/s3.html)
#
# The term "S3 method" basically just refers to a scheme of method dispatching,
#   not a specific object type as the name might suggest.
#
# S4 is a stricter version (compared to S3) of the object oriented programming 
# structure in R with many of underlying ideas the same. Two main differences:
#   1. formal class definitions: unlike S3, S4 formally defines the representation 
#      and inheritance for each class
#   2. multiple dispatch: the generic function can be dispatched to a method 
#      based on the class of any number of argument, not just one
# (see http://adv-r.had.co.nz/S4.html)
#
# Pointers in R can be implemented using environments, since environments do not
#   get coppied when used as arguments of a function
#
#
# # How to overwrite a locked function in a package
#
# #library(osmdata)
# environmentIsLocked(asNamespace("curl"))
# unlockBinding(sym = "has_internet", asNamespace("curl"))
# assign(x = "has_internet", value = {function() T}, envir = asNamespace("curl"))
# curl:::has_internet()
#
###

# a = lapply(list.files(), source); list_package_exported('utilbox', dependencies=T)