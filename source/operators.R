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
#' \code{\%has\%} is equivalent to \code{\%in\%} except with the roles
#' of the two sides switched.
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

#' @rdname not_in
#' @export
`%has%` = function(table, x) {
  match(x, table, nomatch = 0L) > 0L
}

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
#' Concatenation operators, which are aliases for `base::paste`.
#'
#' \code{\%_\%} is an operator versions of \code{base::paste} with 
#' `sep='_'` and which takes symbols as arguments (see the examples 
#' below).
#'
#' \code{\%pastefront\%} is a conditional concatenatation operator, 
#' which appends the string in `lhs` onto `rhs` from the left but 
#' only if `rhs` does not already start with the string in `lhs`. 
#'
#' \code{\%pasteend\%} appends the string in `lhs` onto `rhs` from
#' the right (i.e., onto the end) but only if `rhs` does not already
#' end with the string inside the `lhs`.
#'
#' @examples
#' a = 'multi'; b = 'tasking'
#' a %_% b                    # BEWARE: yields 'a_b' (the inputs are treated as symbolic!)
#' 'name:' %pastefront% 'james'        # yields 'name:james'
#' 'name:' %pastefront% 'name:james'   # yields 'name:james'
#' 'james' %pasteend% 'name:'          # yields 'name:james'
#' 'james' %pasteend% 'name:james'     # yields 'name:james'
#'
#' @name concatenate
#' @export
`%_%` = function(..., sep="_") {
  dots = as.character(dots_to_nlist()) #match.call(expand.dots = FALSE)$`...`
  strings = as.list(as.character(dots))
  do.call("paste", append(strings, list(sep=sep)))
}
  
#' @rdname concatenate
#' @export
`%pastefront%` = function(lhs, rhs) {
  ifelse(base::startsWith(rhs, lhs), rhs, paste0(lhs, rhs))
}

#' @rdname concatenate
#' @export
`%pasteend%` = function(lhs, rhs) {
  ifelse(base::endsWith(rhs, lhs), rhs, paste0(rhs, lhs))
}

#' @title
#' Regular expression match operator
#'
#' @description
#'
#' `\%m\%` is an operator version of `base::grepl`. It uses the left-hand 
#' side argument (`lhs`) as the pattern and the right-hand side argument
#' (`rhs`) as the string that is compared with the pattern using a vectorized
#' version of `base::grepl`. The vectorization means that the operator can 
#' take non-scalar arguments (on either side). It is required that the two 
#' arguments have the same length or that at least one of them has length 1. 
#' The return value has length of the longer argument.
#'
#' \code{\%like\%} does the same except with the roles of the two 
#' arguments are reversed, i.e., `lhs` is the string and `rhs` is the 
#' pattern (analogous to the keyword 'LIKE' in SQL).
#'
#' `\%nm%` and `\%notlike%` are the negations of the two operators. 
#' `\%mi\%`, `\%likei\%`, `\%nmi\%`, `\%notlikei\%` are case-insensitive 
#' versions of the four operators above.
#'
#' `\%likeany\%` is an \"any pattern\" version of `\%like\%`. It checks
#' whether the elements of `lhs` match any of the elements in `rhs`, which
#' are the patterns. The two arguments can have any length. The return value 
#' has the same length as `lhs`.
#'
#' `\%likeanyi%` is a case-insensitive version of `\%likeany%`.
#'
#' @examples
#' # pattern on the left-hand side:
#' 'ay' %m% 'daylight'              # TRUE
#' 'ai' %m% 'daylight'              # FALSE
#' 'ai' %m% 'daylight'              # FALSE
#' 'ay' %nm% 'daylight'             # FALSE
#' 'ai' %nm% 'daylight'             # TRUE
#'
#' # pattern on the left-hand side (vectors):
#' c('ay','x') %m% 'daylight'       # TRUE, FALSE
#' 'lig' %m% c('day','daylight')    # FALSE, TRUE
#' rep('lig',3) %m% c('day','ops')  # error
#'
#' # pattern on the right-hand side:
#' 'daylight' %like% 'ay'           # TRUE
#' 'daylight' %like% 'ai'           # FALSE
#' 'daylight' %notlike% 'ay'        # FALSE
#' 'daylight' %notlike% 'ai'        # TRUE
#'
#' # pattern on the right-hand side:
#' 'daylight' %like% c('ay','x')    # TRUE, FALSE
#'
#' # case insensitive versions:
#' 'ay' %mi% 'DAYlight'             # TRUE
#' 'aY' %mi% 'DAYlight'             # TRUE
#' 'DAYlight' %likei% 'ay'          # TRUE
#' 'DAYlight' %likei% 'aY'          # TRUE
#'
#' # any-pattern matching:
#' c('Monday','January') %likeany% c('on','day','j')   # TRUE, FALSE, FALSE
#' c('Monday','January') %likeanyi% c('on','day','j')  # TRUE, FALSE, TRUE
#'
#' @name operator_match
#' @family operators provided by utilbox
#' @export
`%m%` = greplm

#' @rdname operator_match 
#' @export
`%nm%` = function(lhs, rhs) {
  !greplm(lhs, rhs)
}

#' @rdname operator_match
#' @export
`%mi%` = function(lhs, x, ...) {
  greplm(lhs, x, ignore.case=TRUE, ...)
}

#' @rdname operator_match
#' @export
`%nmi%` = function(lhs, x, ...) {
  !greplm(lhs, x, ignore.case=TRUE, ...)
}

#' @rdname operator_match 
#' @export
`%like%` = function(x, rhs, ...) {
  greplm(rhs, x, ...)
}

#' @rdname operator_match 
#' @export
`%notlike%` = function(x, rhs, ...) {
  !greplm(rhs, x, ...)
}

#' @rdname operator_match 
#' @export
`%likei%` = function(x, rhs, ...) {
  greplm(rhs, x, ignore.case=TRUE, ...)
}

#' @rdname operator_match 
#' @export
`%notlikei%` = function(x, rhs, ...) {
  !greplm(rhs, x, ignore.case=TRUE, ...)
}

#' @rdname operator_match 
#' @export
`%likef%` = function(x, rhs, ...) {
  greplm(rhs, x, fixed=TRUE, ...)
}

#' @rdname operator_match 
#' @export
`%notlikef%` = function(x, rhs, ...) {
  !greplm(rhs, x, fixed=TRUE, ...)
}

#' @rdname operator_match 
#' @export
`%likefi%` = function(x, rhs, ...) {
  greplm(rhs, x, fixed=TRUE, ignore.case=TRUE, ...)
}

#' @rdname operator_match 
#' @export
`%notlikefi%` = function(x, rhs, ...) {
  !greplm(rhs, x, fixed=TRUE, ignore.case=TRUE, ...)
}

#' @rdname operator_match
#' @export
`%likeany%` = function(x, rhs, ...) {
  sapply(x, function(y) any(grepl(rhs, y, ...)))
}

#' @rdname operator_match
#' @export
`%notlikeany%` = function(x, rhs, ...) {
  !`%likeany%`(x, rhs, ...)
}

#' @rdname operator_match
#' @export
`%likeanyi%` = function(x, rhs, ...) {
  sapply(x, function(y) any(grepl(rhs, y, ignore.case=TRUE, ...)))
}

#' @rdname operator_match
#' @export
`%notlikeanyi%` = function(x, rhs, ...) {
  !`%likeany%`(rhs, x, ignore.case=TRUE, ...)
}

#' @rdname operator_match
#' @export
`%likeanyf%` = function(x, rhs, ...) {
  sapply(x, function(y) any(grepl(rhs, y, fixed=TRUE, ...)))
}

#' @rdname operator_match
#' @export
`%notlikeanyf%` = function(x, rhs, ...) {
  !`%likeany%`(rhs, x, fixed=TRUE, ...)
}

#' @rdname operator_match
#' @export
`%likeanyfi%` = function(x, rhs, ...) {
  sapply(x, function(y) any(grepl(rhs, y, fixed=TRUE, ignore.case=TRUE, ...)))
}

#' @rdname operator_match
#' @export
`%notlikeanyfi%` = function(x, rhs, ...) {
  !`%likeany%`(rhs, x, fixed=TRUE, ignore.case=TRUE, ...)
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
#' its second argument (`y`). Otherwise it returns the first argument (`x`).
#'
#' `\%NEXISTS\%` works analogously except that it avoids an error due to
#' the non-existence of the first argument by returning the second argument
#' instead.
#'
#' @name operator_NULL
#' @examples
#' 1 %|||% 2                                   # returns 2
#' NULL %|||% 2                                # returns 2
#' NULL %||||% 2                               # returns 2
#' numeric(0) %||||% 2                         # returns 2
#' round('a') %ERR% 0                          # returns 0
#' try(round('a'), silent=TRUE) %ERRCLS% 0     # returns 0
#' .name.of.a.nonexisting.object. %NEXISTS% 0  # returns 0
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
`%ERRCLS%` = function(x, y) {
  if(is_error(x)) y else x
}

#' @rdname operator_NULL
#' @export
`%ERR%` = function(x, y) {
  tryCatch(x, error = function(e) y)
}

`%ERR_OLD%` = function (x, y) {
  if(is_error(suppressWarnings(try(eval(x), silent=TRUE)))) y else x
}

#' @title
#' Renaming operators
#'
#' @description
#'
#' Operators that change the name of an object by reassigning the 
#' value from its left-hand side (for \code{\%->\%}) or right-hand side 
#' (for \code{\%<-\%}) and assigning the value to the variable on the 
#' other side, thus effectively performing renaming of an object.
#'
#' @return The value originally found in `from`.
#'
#' @name renaming_operators
#' @examples
#' a = 100; a %->% b; b; exists('a')  # after the call b has the value 100 and a does not exist
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
#' list(a=1, b=2) %append% list(c=3)    # adds an element to the list on the lhs
#' c(1,2) %append% list(c=3)            # turns the lhs into a list, then appends
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

#' @title
#' Operators for trying calls and recovering from failures
#'
#' @description
#'
#' `%try%` attempts to execute a block of code on the right-hand
#' side (rhs) using `base::tryCatch`. The left-hand side (lhs)
#' is ignored.
#'
#' `%otherwise%` looks for an error result on the lhs and if one
#' is found, it executes the rhs code.
#'
#' `%finally%` executes the rhs code and returns the outcome whenever
#' the lhs was an error, otherwise it returns the lhs.
#'
#' After the execution of %otherwise% or %finally% the latest error 
#' is found in `.e` in the parent frame.
#'
#' `%recover%` attempts to execute the lhs and on failure it executes 
#' the rhs. It is similar to `%ERR%`, which works a little differently.
#'
#' @examples
#' . %try% print(fsdafsdaf) %otherwise% message('the printing did not work')
#' print(fsdafsdaf) %recover% message('the printing did not work')
#'
#' @family operators provided by utilbox
#' @name operator_try
#' @export
`%try%` = function(., ...) {
  tryCatch(
    with(parent.frame(), ...),
    error = function(e) e
  )
}

#' @rdname operator_try
#' @export
`%otherwise%` = function(e, ...) {
  if ('error' %in% class(e)) {
    assign('.e', e, envir = parent.frame())
    with(parent.frame(), ...)
  }
  else
    e
}

#' @rdname operator_try
#' @export
`%finally%` = function (x, ...) {
  if ('error' %in% class(x)) {
    assign('.e', x, envir = parent.frame())
    assign('.result', NULL, envir = parent.frame())
   
    with(parent.frame(), ...)
  } else {
    assign('.e', NULL, envir = parent.frame())
    assign('.result', x, envir = parent.frame())
   
    with(parent.frame(), ...)
    x
  }
}

#' @rdname operator_try
#' @export
`%recover%` = function(lhs, ...) {
  res_lhs = tryCatch(
    with(parent.frame(), lhs),
    error = function(e) e
  )
  if ('error' %in% class(res_lhs)) {
    with(parent.frame(), ...)
  } else {
    e
  }
}
