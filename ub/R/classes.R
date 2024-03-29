#' @title
#' Show all available methods
#'
#' @description
#'
#' This is an alias for [`utils::methods`].
#'
#' @export 
all_methods = function(...) {
  methods(...)
}

#' @title
#' Add class to the object's classes
#'
#' @description
#'
#' `add_class()` adds the class specified in `what` as the first 
#' among the classes of the object `x`.
#'
#' `drop_class()` removes the specified class from the classes of the 
#' given object.
#'
#' @examples
#' add_class(list(1:20), 'huge')
#'
#' @name classes
#' @family class-related functions provided by ub
#' @export
add_class = function(x, class) {
  if(class %in% class(x)) x else `class<-`(x, c(class, class(x)))
}

#' @rdname classes
#' @export
drop_class = function(x, class) {
  `class<-`(x, setdiff(class(x), class))
}

# #' @title
# #' Set class
# #'
# #' @description
# #'
# #' `set_class()` attempts to set the class of `x` to `class` by 
# #' calling `as.*` where `*` is substituted for the character value in 
# #' `class`.
# #'
# #' `has_class()` checks if the class given in `class` is among the 
# #' classes of `x`.
# #'
# #' @examples
# #' set_class(1, 'character')
# #'
# #' has_class(1, 'character')
# #' has_class(set_class(1, 'character'), 'character')
# #'
# #' @export
# set_class = function(x, class) {
#   fun_name = paste0('as.', class)
#   if(exists(fun_name, mode='function')) {
#     do.call(fun_name, list(x))
#   } else {
#     `class<-`(x, class)
#   }
# }

# #' @rdname set_class
# #' @export
# has_class = function(x, class) {
#   inherits(x, class)
# }

#' @title
#' Convert objects to class `huge`
#'
#' @description
#'
#' Class `huge` is intended to be used for objects with many elements 
#' which, if all printed, would clutter the screen. With class `huge` 
#' set, only a limited number of elements are actually printed. 
#' (extracted via `base::head()`).
#'
#' `is_huge()` checks whether class `huge` is among the classes of 
#' the object in `x`.
#'
#' `as.huge()` adds class `huge` as the first class of `x`.
#'
#' @examples
#' as.huge(list(1:100))
#'
#' @name class_huge
#' @family class-related functions provided by ub
#' @export
is.huge = function(x) {
  inherits(x, 'huge')
}

#' @rdname class_huge
#' @export
as.huge = function(...) {
  UseMethod("as.huge")
}

#' @rdname class_huge
#' @export
as.huge.list = function(x) {
  add_class(lapply(x, as.huge), 'huge')
}

#' @rdname class_huge
#' @export
as.huge.default = function(x) {
  add_class(x, 'huge')
}

#' @rdname class_huge
#' @export
`[.huge` = function(x, i) {
  as.huge(NextMethod())
}

#' @rdname class_huge
#' @export
`[[.huge` = function(x, i) {
  as.huge(NextMethod())
}

#' @rdname class_huge
#' @export
print.huge = function(...) {
  init_in_ub_env('trunc_n_hidden')
  init_in_ub_env('trunc_n_limit')
  UseMethod("print.huge")
}

#' @rdname class_huge
#' @export
print.huge.character = function(x, n, len, show_note=TRUE) {

  if(missing(n)) n = get_in_ub_env('trunc_n_limit')
  if(missing(n)) len = get_in_ub_env('abbrev_len_limit')

  y = truncate_huge(x, n)
  y = str_abbreviate(y, len)
  y = drop_class(y, 'huge')

  print(y)

  if(length(x)>n) {
    print_trunc_info(length(x)-n)
  }
  
  invisible(x)
  
}

##' rdname class_huge
##' export
#print.huge.default = function(x, n, len, show_note=TRUE) {
#  NextMethod()
#  browser()
#  print_trunc_note()
#}

#' @rdname class_huge
#' @export
print.huge.default = function(x, n, len, show_note=TRUE) {
  
  if(missing(n)) n = get_in_ub_env('trunc_n_limit')
  if(missing(n)) len = get_in_ub_env('abbrev_len_limit')
  
  y = truncate_huge(x, n)
  print(y)
  #browser()
  print_trunc_note()

}

##' rdname class_huge
##' export
#print.huge.default = function(x, n=10, show_note=TRUE) {
#  print(drop_class(x, 'huge'))
#  invisible(x)
#}

#' @title
#' Methods for class truncated
#'
#' @description
#'
#' `is_truncated()` checks whether and object is of class `truncated`.
#'
#' @name class_truncated
#' @export
is_truncated = function(x) {
  inherits(x, 'truncated')
}

#' @rdname class_truncated
#' @export
`[.truncated` = function(x, i) {
  as.truncated(NextMethod())
}

#' @rdname class_truncated
#' @export
`[[.truncated` = function(x, i) {
  as.truncated(NextMethod())
}

#' @rdname class_truncated
#' @export
as.truncated = function(x) {
  UseMethod('as.truncated')
}

#' @rdname class_truncated
as.truncated.default = function(x) {
  add_class(drop_class(x, 'huge'), 'truncated')
}

#' @rdname class_truncated
print.truncated = function(x) {

  y = drop_class(x, 'truncated')
  attr(y, 'trunc_info') = NULL
  print(y)
  
  trunc_info = list_update(default_trunc_info(), attr(x, 'trunc_info'))
  print_trunc_info(trunc_info$ntrunc)
  
  x

}

#' @title
#' Methods for class \"`abbrevstr`\"
#'
#' @description
#'
#' Printing method for class \"`abbrevstr`\", which extends 
#' [`base::print`].
#'
#' @name class_abbrevstr

print.abbrevstr = function(x) {

  y = x
  attr(y, 'ncut') = NULL
  print(unclass(y))

}

#' @rdname class_huge
#' @export
truncate_huge = function(...) {
  UseMethod('truncate_huge')
}

#' @rdname class_huge
truncate_huge.default = function(x, n) {
  
  y = head(x, n)
  ntrunc = length(x) - length(y)
  y = drop_class(y, 'huge')
  y = add_class(y, 'truncated')
  structure(y, trunc_info=list(length=length(x), nlimit=n, ntrunc=ntrunc))

}

msg_character_shortened = function(ncut, cut_symbol='......') {
  ifelse(ncut<=0, '', paste0(cut_symbol, ' (', ncut, ' characters omitted) ', cut_symbol))
}

default_trunc_info = function() {
  list(ntrunc=0, nlimit=NA)
}

print_trunc_info = function(n_hidden) {
  if(n_hidden>0) {
    msgf("# ... with ",n_hidden," more elements")
    update_in_ub_env("trunc_n_hidden", n_hidden)
  }
}

print_trunc_note = function() {
  n_limit = get_in_ub_env("trunc_n_limit")
  any_hidden = get_in_ub_env("trunc_n_hidden")
  #print(any_hidden)
  if(any_hidden>0) {
    msg = paste0("Note: For objects of class 'huge' only the first ", n_limit, 
                 " elements were printed. Use print(n=...) to change this limit.")
    msgf('\n',msg)
    invisible(msg)
  }
}

