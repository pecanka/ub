#' @title
#' Empty string check
#'
#' @description
#'
#' `str_is_empty()` checks for empty strings, i.e. strings with zero 
#' length. By default, white space is not trimmed prior to the check, 
#' but this can be enabled by adding `trim=TRUE` to the call.
#'
#' `str_is_empty_any()` checks for any empty strings inside a vector.
#'
#' `str_is_empty_not()` is returns the negation of what `str_is_empty()` 
#' returns.
#'
#' `str_n_empty()` counts the number of empty string in a character 
#' vector. Same defaults as `str_is_empty()`.
#'
#' @examples
#' str = c('hello','world','   ','')
#'
#' str_is_empty(str)                   # only the first is empty
#' str_is_empty(str, TRUE)             # both empty
#'
#' str_is_empty_not(str)               # both non-empty
#' str_is_empty_not(str, TRUE)         # only the second is non-empty
#'
#' str_is_empty_any(str)               # TRUE (because of the 4th)
#' str_is_empty_any(str[1:3])          # FALSE (without trimming 1st-3rd are not empty)
#' str_is_empty_any(str[1:3], TRUE)    # TRUE (with trimming the 3rd is empty)
#'
#' str_n_empty(str)                    # counts 1 empty
#' str_n_empty(str, TRUE)              # counts 2 empty
#' 
#' @name str_is_empty
#' @family string-manipulation functions provided by utilbox
#' @export
str_is_empty = function(x, trim=FALSE) {
  !nzchar(if(trim) str_trim_space(x) else x)
}

#' @rdname str_is_empty
#' @export
str_is_empty_not = function(x, trim=FALSE) {
  nzchar(if(trim) str_trim_space(x) else x)
}

#' @rdname str_is_empty
#' @export
str_n_empty = function(x, trim=FALSE) {
  sum(str_is_empty(x, trim))
}

#' @rdname str_is_empty
#' @export
str_is_empty_any = function(x, trim=FALSE) {
  str_n_empty(x, trim) > 0
}

