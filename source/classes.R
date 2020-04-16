#' Show all available methods
#'
#' This is only an alias for [base::methods].
#'
#' @export 
all_methods = function(...) {
  methods(...)
}

#' Add class to the object's classes
#'
#' `add_class` adds the class specified in `what` as the 
#' first among the classes of the object `x`. 
#'
#' `drop_class` removes the specified class from the classes of 
#' the given object.
#'
#' @examples
#' add_class(list(1:20), 'huge')
#'
#' @name classes
#' @family class-related functions provided by utilbox
#' @export
add_class = function(x, class) {
  if(class %in% class(x)) x else `class<-`(x, c(class, class(x)))
}

#' @rdname classes
#' @export
drop_class = function(x, class) {
  `class<-`(x, setdiff(class(x), class))
  #`class<-`(x, class(x)[h1(which(class(x)!=class))])
}

#' Set class
#'
#' `set_class` attempts to set the class of `x` to `class` by calling
#' `as.*` where `*` is substituted for the character value 
#' in `class`.
#'
#' `has_class` checks if the class given in `class` is among the classes
#' of `x`.
#'
#' @examples
#' set_class(1, 'character')
#'
#' has_class(1, 'character')
#' has_class(set_class(1, 'character'), 'character')
#'
#' @export
set_class = function(x, class) {
  do.call('as.'%.%class, list(x))
}

#' @rdname set_class
#' @export
has_class = function(x, class) {
  inherits(x, class)
  #class %in% class(x)
}

#' Convert objects to class `huge`
#'
#' Class `huge` is intended to be used for objects with many elements which, 
#' if all printed, would clutter the screen. With class `huge` 
#' set, only a limited number of elements are actually printed.
#' (extracted by [base::head]).
#'
#' `is_huge` checks whether class \"`huge`\" is among the classes
#' of the object in `x`.
#'
#' `as.huge` adds class \"`huge`\" as the first class of `x`. 
#'
#' @examples
#' as.huge(list(1:100))
#'
#' @name class_huge
#' @family class-related functions provided by utilbox
#' @export
is.huge = function(x) {
  has_class(x, 'huge')
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
  init_utilbox('trunc_n_hidden')
  init_utilbox('trunc_n_limit')
  UseMethod("print.huge")
}

#' @rdname class_huge
#' @export
print.huge.character = function(x, n, len, show_note=TRUE) {

  if(missing(n)) n = get_utilbox('trunc_n_limit')
  if(missing(n)) len = get_utilbox('abbrev_len_limit')

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
  
  if(missing(n)) n = get_utilbox('trunc_n_limit')
  if(missing(n)) len = get_utilbox('abbrev_len_limit')
  
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

#' Methods for class truncated
#'
#' 
#'
#' @name class_truncated
#' @export
is_truncated = function(x) {
  has_class(x, 'truncated')
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
as.truncated.default = function(x) {
  add_class(drop_class(x, 'huge'), 'truncated')
}

#' @rdname class_truncated
#' @export
print.truncated = function(x) {

  y = drop_class(x, 'truncated')
  attr(y, 'trunc_info') = NULL
  print(y)
  
  trunc_info = list_update(default_trunc_info(), attr(x, 'trunc_info'))
  print_trunc_info(trunc_info$ntrunc)
  
  x

}

#' Methods for class \"`abbrevstr`\"
#'
#' 
#'
#' @name class_abbrevstr
#' @export
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
#' @export
truncate_huge.default = function(x, n) {
  
  y = head(x, n)
  ntrunc = length(x) - length(y)
  y = drop_class(y, 'huge')
  y = add_class(y, 'truncated')
  structure(y, trunc_info=list(length=length(x), nlimit=n, ntrunc=ntrunc))

}

msg_character_shortened = function(ncut, cut_symbol='......') {
  ifelse(ncut<=0, '', cut_symbol %.% ' (' %.% ncut %.% ' characters omitted) ' %.% cut_symbol)
}

default_trunc_info = function() {
  list(ntrunc=0, nlimit=NA)
}

print_trunc_info = function(n_hidden) {
  if(n_hidden>0) {
    catn("# ... with ",n_hidden," more elements")
    update_utilbox("trunc_n_hidden", n_hidden)
  }
}

print_trunc_note = function() {
  n_limit = get_utilbox("trunc_n_limit")
  any_hidden = get_utilbox("trunc_n_hidden")
  #print(any_hidden)
  if(any_hidden>0) {
    msg = "Note: For objects of class 'huge' only the first " %.% n_limit %.% 
          " elements were printed. Use print(n=...) to change this limit."
    catn('\n',msg)
    invisible(msg)
  }
}

