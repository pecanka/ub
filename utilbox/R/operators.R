
#' @title
#' Not-in-set operator
#'
#' @description
#'
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
#'
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
#'
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
`%.%` = function(x, y) {
  paste0(x, y)
  #do.call("paste0", list(...))
}

#' @rdname concatenate
#' @export
`%..%` = function(x, y) {
  paste(x, y)
  #do.call("paste", list(...))
}

#' @rdname concatenate
#' @export
`%_%` = function(x, y) {
  paste(x, y, sep="_")
  #do.call("paste", list(..., sep='_'))
}

#' @rdname concatenate
#' @export
`%__%` = function(..., sep="_") {
  dots = as.character(dots_to_nlist()) #match.call(expand.dots = FALSE)$`...`
  strings = as.list(as.character(dots))
  do.call("paste", append(strings, list(sep=sep)))
}
  
#' @rdname concatenate
#' @export
`%.^%` = function(x, y) {
  ifelse(greplm('^('%.%x%.%')', y), y, x%.%y)
}

#' @title
#' Regular expression match operator
#'
#' @description
#'
#' `\%match\%` (alias `\%m\%`) is an operator version of 
#' `base::grepl`. It uses the left-hand side argument as the `pattern` 
#' and the right-hand side argument as the `text` arguments of a call to 
#' `base::grepl`. The call to `base::grepl` is vectorized, 
#' which means that the operator also takes vector arguments (on either 
#' side).
#'
#' `\%m_ic\%` is a version of `\%m\%` which ignores case by default.
#'
#' \code{\%matches\%} does the same except with the roles of the two 
#' arguments reversed.
#'
#' `\%notmatch%` (alias `\%nm\%`) and `\%notmatches%` (alias 
#' `\%nmes\%`) are the negations of the two operators.
#'
#' `\%nm_ic\%` is the case-insensitive version of `\%notmatch%` and 
#' `\%nm\%`.
#'
#' `\%m_any\%` is an \"any pattern\" matching operator. For its RHS,
#' it checks whether its elements match any of the patterns on the
#' LHS, and returns logical indicators of this match. If an element
#' on the RHS does not match any of the patters specified on the LHS,
#' its corresponding returns value is `FALSE`, otherwise it is `TRUE`.
#'
#' `\%m_any_ic%` is a case-insensitive version of `\%m_any%`.
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
#' "DAYlight" %m_ic% "ay"
#' "DAYlight" %m_ic% "ai"
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
`%m_ic%` = function(pattern, x, ...) {
  `%match%`(pattern, x, ignore.case=TRUE, ...)
}
#`%m_ic%` = `%match%`
#base::formals(`%m_ic%`)[['ignore.case']] = TRUE

#' @rdname operator_match 
#' @export
`%notmatch%` = function(pattern, x, ...) {
  !`%match%`(pattern, x, ...)
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
`%nm_ic%` = function(pattern, x, ...) {
  !`%match%`(pattern, x, ignore.case=TRUE, ...)
}
#`%nm_ic%` = `%notmatch%`
#base::formals(`%nm_ic%`)[['ignore.case']] = TRUE
#`%nm_ic%` = hijack(`%notmatch%`, ignore.case=TRUE)

#' @rdname operator_match 
#' @export
`%matches%` = function(x, pattern, ...) {
  `%match%`(pattern, x, ...)
}

#' @rdname operator_match
#' @export
`%mes%` = `%matches%`

#' @rdname operator_match 
#' @export
`%notmatches%` = function(x, pattern, ...) {
  !`%matches%`(pattern, x, ...)
}
#`%notmatches%` = `%matches%`
#base::formals(`%notmatches%`)[['exclude']] = TRUE
#`%notmatches%` = hijack(`%matches%`, exclude=TRUE)
#`%notmatches%` = Negate(`%matches%`)

#' @rdname operator_match
#' @export
`%nmes%` = `%notmatches%`

#' @rdname operator_match
#' @export
`%m_any%` = function(pattern, x, ...) {
  sapply(x, function(y) any(`%match%`(pattern, y, ...)))
}

#' @rdname operator_match
#' @export
`%m_any_ic%` = function(pattern, x, ignore.case=TRUE, ...) {
  sapply(x, function(y) any(`%match%`(pattern, y, ignore.case=ignore.case, ...)))
}

#' @rdname operator_match
#' @export
`%mes_any%` = function(x, pattern, ...) {
  `%m_any%`(pattern , x, ...)
}

#' @rdname operator_match
#' @export
`%mes_any_ic%` = function(x, pattern, ignore.case=TRUE, ...) {
  `%m_any%`(pattern, x, ignore.case=ignore.case, ...)
}

#' @rdname operator_match
#' @export
`%nm_any%` = function(pattern, x, ...) {
  !`%m_any%`(pattern, x, ...)
}

#' @rdname operator_match
#' @export
`%nm_any_ic%` = function(pattern, x, ignore.case=TRUE, ...) {
  !`%m_any%`(pattern, x, ignore.case=ignore.case, ...)
}

#' @rdname operator_match
#' @export
`%nmes_any%` = function(x, pattern, ...) {
  !(pattern %m_any% x)
}

#' @rdname operator_match
#' @export
`%nmes_any_ic%` = function(x, pattern, ignore.case=TRUE, ...) {
  !(pattern %m_any_ic% x)
}

#' @title
#' Default value for NULL and zero-length objects
#'
#' @description
#'
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
#'
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
#'
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
#'
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
#'
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

