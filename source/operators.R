
#' @title
#' Not-in-set operator
#'
#' @description
#' Logical negation of the value matching operator \code{\%in\%}. The 
#' operator \code{\%notin\%} (just like its alias \code{\%notin\%}) 
#' returns `TRUE` for all elements of the first argument that are not 
#' contained among the elements of the second argument, and `FALSE` 
#' otherwise.
#'
#' @examples
#' 1 %notin% 1:3
#' 0 %notin% 1:3
#'
#' @name not_in
#' @family operators provided by utilbox
#' @export
`%notin%` = function(...) {
  !`%in%`(...)
}
#`%notin%` = Negate(`%in%`)

#`%notin%` = function(a, b) {
#  !(base::`%in%`(a, b))
#}

#' @rdname not_in
#' @export
`%nin%` = `%notin%`

#' @title
#' Any-in-set operator
#'
#' @description
#' Similar to the 'in-set' operator except the return value is 
#' reduced to a scalar via `any`.
#'
#' @examples
#' 0:1 %anyin% 1:10
#'
#' @family operators provided by utilbox
#' @export
`%anyin%` = function(x, table) {
  any(`%in%`(x, table))
}

#' @title
#' Concatenation operators
#'
#' @description
#' Concatenation operators, which are aliases for `base::paste`.
#'
#' \code{\%.\%} is an operator versions of \code{base::paste0}.
#'
#' \code{\%..\%} is an operator versions of \code{base::paste}.
#'
#' \code{\%_\%} is an operator versions of \code{base::paste} with 
#' `sep='_'`.
#'
#' \code{\%__\%} is an operator versions of \code{base::paste} with 
#' `sep='_'` and which takes symbols as arguments (see the examples 
#' below).
#'
#' @name concatenate
#' @examples
#' a = 'multi'; b = 'tasking'
#' a %.% b            # yields 'multitasking'
#' a %..% b           # yields 'multi tasking'
#' a %_% b            # yields 'multi_tasking'
#' a %__% b           # BEWARE: yields 'a_b' (the inputs are treated as symbolic!)
#'
#' @family operators provided by utilbox
#' @export
`%.%` = function(...) {
  do.call("paste0", list(...))
}

#' @rdname concatenate
#' @export
`%..%` = function(...) {
  do.call("paste", list(...))
}

#' @rdname concatenate
#' @export
`%_%` = function(...) {
  do.call("paste", list(..., sep='_'))
}

#' @rdname concatenate
#' @export
`%__%` = function(..., sep="_") {
  dots = match.call(expand.dots = FALSE)$`...`
  strings = as.list(as.character(dots))
  do.call("paste", append(strings, list(sep=sep)))
}
  
#' @title
#' Regular expression match operator
#'
#' @description
#' `\%match\%` (alias `\%m\%`) is an operator version of 
#' `base::grepl`. It uses the left-hand side argument as the `pattern` 
#' and the right-hand side argument as the `text` arguments of a call to 
#' `base::grepl`. The call to `base::grepl` is vectorized, 
#' which means that the operator also takes vector arguments (on either 
#' side).
#'
#' `\%mic\%` is a version of `\%m\%` which ignores case by default.
#'
#' \code{\%matches\%} does the same except with the roles of the two 
#' arguments reversed.
#'
#' `\%notmatch%` (alias `\%nm\%`) and `\%notmatches%` (alias 
#' `\%nmes\%`) are the negations of the two operators.
#'
#' `\%nmic\%` is the case-insensitive version of `\%notmatch%` and 
#' `\%nm\%`.
#'
#' @examples
#' # in one direction:
#' "ay" %m% "daylight"
#' "ai" %m% "daylight"
#' "ay" %nm% "daylight"
#' "ai" %nm% "daylight"
#'
#' # and in the opposite direction:
#' "daylight" %matches% "ay"
#' "daylight" %matches% "ai"
#'
#' # case insensitive version:
#' "DAYlight" %mic% "ay"
#' "DAYlight" %mic% "ai"
#'
#' @name operator_match
#' @family operators provided by utilbox
#' @export
`%match%` = greplm

#' @rdname operator_match
#' @export
`%m%` = `%match%`

#' @rdname operator_match
#' @export
`%mic%` = function(pattern, x, ...) {
  greplm(pattern, x, ignore.case=TRUE, ...)
}
#`%mic%` = `%match%`
#base::formals(`%mic%`)[['ignore.case']] = TRUE

#' @rdname operator_match 
#' @export
`%notmatch%` = function(pattern, x, ...) {
  !greplm(pattern, x, ...)
}
#`%notmatch%` = `%match%`
#base::formals(`%notmatch%`)[['exclude']] = TRUE
#`%notmatch%` = hijack(`%match%`, exclude=TRUE)
#`%notmatch%` = Negate(`%match%`)

#' @rdname operator_match
#' @export
`%nm%` = `%notmatch%`

#' @rdname operator_match
#' @export
`%nmic%` = function(pattern, x, ...) {
  !greplm(pattern, x, ignore.case=TRUE, ...)
}
#`%nmic%` = `%notmatch%`
#base::formals(`%nmic%`)[['ignore.case']] = TRUE
#`%nmic%` = hijack(`%notmatch%`, ignore.case=TRUE)

#' @rdname operator_match 
#' @export
`%matches%` = function(x, pattern, ...) {
  greplm(pattern, x, ...)
}

#' @rdname operator_match
#' @export
`%mes%` = `%matches%`

#' @rdname operator_match 
#' @export
`%notmatches%` = function(x, pattern, ...) {
  !greplm(pattern, x, ...)
}
#`%notmatches%` = `%matches%`
#base::formals(`%notmatches%`)[['exclude']] = TRUE
#`%notmatches%` = hijack(`%matches%`, exclude=TRUE)
#`%notmatches%` = Negate(`%matches%`)

#' @rdname operator_match
#' @export
`%nmes%` = `%notmatches%`

#' @title
#' Default value for NULL and zero-length objects
#'
#' @description
#' `\%|||\%` is intented to be equivalent "\code{\%||\%}" operator in 
#' the package `rlang`, but named differently as not to clash with it.
#'
#' `\%||||\%` extends this behavior to all zero-length objects.
#'
#' `\%|||||\%` extends this behavior further to include empty strings.
#'
#' `\%NA\%` is an operator version of `ifelse(is.na(x), y, x), which 
#' allows input of various types (vectors, lists, closures, etc.).
#'
#' `\%ERR\%` works analogously except that it checks for class 
#' `try-error`. If its first argument is of class `try-error` it returns 
#' its second argument (`y`). Otherwise it returns the first (`x`).
#'
#' @name operator_NULL
#' @examples
#' 1 %|||% 2
#' NULL %|||% 2
#' NULL %||||% 2
#' numeric(0) %||||% 2
#'
#' @family operators provided by utilbox
#' @export
`%|||%` = function (x, y) {
  if(is.null(x)) y else x
}

#' @rdname operator_NULL
#' @export
`%||||%` = function (x, y) {
  if(is_empty(x)) y else x
}

#' @rdname operator_NULL
#' @export
`%|||||%` = function (x, y) {
  if(is_empty(x) || !nzchar(x)) y else x
}

#' @rdname operator_NULL
#' @export
`%NA%` = function (x, y) {
  if(class(x)=='function') x else ifelse(is.na(x), y, x)
}

#' @rdname operator_NULL
#' @export
`%ERR%` = function (x, y) {
  if(is_error(x)) y else x
}

#' @title
#' Renaming operators
#'
#' @description
#' Operators that change the name of an object by reassigning the 
#' value from its left-hand side (for \code{\%->\%}) or right-hand side 
#' (for \code{\%->\%}) and assigning the value to the variable on the 
#' other side, thus effectively performing renaming of an object.
#'
#' @return The value originally found in `from`.
#'
#' @name renaming_operators
#' @examples
#' a = 100; a %->% b; b; exists('a')  # after the call b is 100 and a does not exist
#' a = 100; b %<-% a; b; exists('a')  # same effect, opposite direction of syntax
#'
#' @family operators provided by utilbox
#' @export
`%->%` = function(from, to, envir=parent.frame()) {
  from = as.character(substitute(from))
  to = as.character(substitute(to))
  if(!identical(from, to)) {
    assign(to, get(from, envir=envir), envir=envir)
    rm(list=from, envir=envir)
  }
  return(invisible(get(to, envir=envir)))
}

#' @rdname renaming_operators
#' @export
`%<-%` = `body<-`(function(to, from, envir=parent.frame()) { }, value=body(`%->%`))

#' @title
#' Append operator
#'
#' @description
#' Append one object to another via [`base::append`]. `\%append\%` 
#' appends the right-hand argument to the left-hand side, while 
#' `\%appendR\%` appends in the reverse order.
#'
#' @returns A list.
#'
#' @examples
#' list(a=1, b=2) %append% list(c=3)    # adds an element to the list on the LHS
#' c(1,2) %append% list(c=3)            # turns the LHS into a list, then appends
#'
#' @name operator_append
#' @family operators provided by utilbox
#' @export
`%append%` = function(left, right) {
  if(!is_empty(left)  && !is.list(left))  left  = list(left)
  if(!is_empty(right) && !is.list(right)) right = list(right)
  append(left, right)
}

#' @rdname operator_append
#' @export
`%appendR%` = `body<-`(function(right, left) { }, value = body(`%append%`))

#' @rdname operator_append
#' @export
`%ap%` = `%append%`

#' @rdname operator_append
#' @export
`%apR%` = `%appendR%`

#' @title
#' Append operator
#'
#' @description
#' Append one object to another via [`base::c`].
#'
#' @examples
#' 1:10 %c% 11:15
#'
#' @name operator_c
#' @family operators provided by utilbox
#' @export
`%c%` = function(x, y) {
  c(x, y)
}

#' @rdname operator_c
#' @export
`%cR%` = function(y, x) {
  c(x, y)
}

#' @title
#' List modify operator
#'
#' @description
#' Merges two list together while updating the elements of the first 
#' list with the elements of the second list for the elements found in 
#' both lists.
#'
#' @examples
#' list(a=1, b=2) %modify% list(b=3, c=4)
#'
#' @family operators provided by utilbox
#' @export
`%modify%` = function(left, right) {
  if(!is_empty(left)  && !is.list(left))  left  = list(left)
  if(!is_empty(right) && !is.list(right)) right = list(right)
  modifyList(left, right)
}

