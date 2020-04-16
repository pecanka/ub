#' Vectorized regular pattern matching
#'
#' Vectorized regular pattern matching, which takes multiple patterns.
#'
#' @export
vregexpr = Vectorize(base::regexpr)

#' Recursive string substitution
#'
#' Allows for multiple arguments in \code{what} and \code{with} which are replaced
#' inside \code{where}. \code{what} and \code{with} are processed in parallel which
#' means that they must have the same length or \code{with} must be of length 1. 
#'
#' \code{subm} leverages \code{base::sub}, while \code{gsubm} leverages \code{base::gsub}.
#'
#' @examples
#' require(magrittr)
#' subm(c('a','b'), paste0('|',c('a','b'),'|'), 'abracadabra')
#' gsubm(c('a','b'), paste0('|',c('a','b'),'|'), 'abracadabra')
#'
#' @name subm
#' @family string-manipulation functions provided by utilbox
#' @export
subm = function(x, what, with) {
  multi_sub(x, what, with, workhorse=base::sub)
}

#' @rdname subm
#' @family string-manipulation functions provided by utilbox
#' @export
gsubm = function(x, what, with) {
  multi_sub(x, what, with, workhorse=base::gsub)
}

#' @rdname subm
#' @family string-manipulation functions provided by utilbox
#' @export
multi_sub = function(x, what, with, workhorse=sub, ...) {
  
  if(length(what)==0) {
    return(x)
  }
  
  if(length(with)==1) {
    with = rep(with, length(what))
  }
  if(length(what)==1) {
    what = rep(what, length(with))
  }
  
  stopifnot(length(what)==length(with))
  
  y = do.call(workhorse, list(what[1], with[1], x, ...))
  
  Recall(y, what[-1], with[-1], workhorse=workhorse, ...)
  
}

#' Substitution of \"@\" substrings
#'
#' `sub_at` and `gsub_at` perform the replacement of \"@-substrings\"
#' inside a string `x`. \"@-substrings\" are strings that start with
#' the character '@' (or the value supplied in `at`, more generally).
#' The replacement of these substrings is governed by what follows the
#' '@'. The string is then replaced (together with the '@') with the 
#' values in the supplied arguments. The matching of the \"@-substrings\" 
#' with the arguments can be controlled in three ways:

#' 1. based on the name attributes of the argument when supplied as 
#' named vectors or named lists (e.g. 
#' `gsub_at(x, c(name='John', city='Prague'))`.
#'
#' 2. as the actual arguments when supplied as named arguments (e.g. 
#' `gsub_at(x, name='John', city='Prague')`).
#'
#' 3. from the symbols supplied in the function call 
#' (e.g. `name='John'; city='Prague'; gsub_at(x, name, city)`).
#'
#' In principal, these methods can be combined (see examples below). 
#' Furthermore, one can leverage a combination of naming arguments 
#' and supplying named vectors/lists, where the names of the arguments
#' and the names in the named vectors/lists are concatenated. Keep in
#' mind that whenever the argument is a named vector/list, the names
#' are concatenated with an added dot ('.'), while for unnamed vectors/lists
#' the concatenation happens without adding the dot.
#'
#' `substitute_at` is the general function which takes named vectors/lists 
#' only and allows the specification of the workhorse function 
#' (e.g. `sub`, `gsub`).
#'
#' @examples
#' x = '@name lived in @city1 and @city2. @city2 became @whose home.'
#' name = 'Jeff'; whose = 'his'; city1 = 'New York'; city2 = 'Prague'
#' gsub_at(x, name, city1, city2, whose)
#'
#' # specify by naming arguments
#' gsub_at(x, name='Donna', city1='Prague', city2='Bonn', whose='her') 
#'
#' # a combination of the two
#' gsub_at(x, name='Donna', whose='her', city1, city2) 
#'
#' # or via a named vector
#' person = c(name='Donna', whose='her', city1='Prague', city2='Bonn')
#' gsub_at(x, person) 
#'
#' # even a combination works
#' gsub_at(x, person[1:2], city2='Berlin', city1='Amsterdam') 
#'
#' # relying on concatenation
#' gsub_at(x, person[1:2], city=c('Berlin','Amsterdam'))
#'
#' @name sub_at
#' @family string-manipulation functions provided by utilbox
#' @export
gsub_at = function(x, ..., at='@') {
  dots = dots_to_nlist(flatten=TRUE)
  substitute_at(x, dots, at=at, workhorse=gsubm)
}

#' @rdname sub_at
#' @export
sub_at = function(x, ..., at='@') {
  dots = dots_to_nlist(flatten=TRUE)
  substitute_at(x, dots, at=at, workhorse=subm)
}

#' @rdname sub_at
#' @export
substitute_at = function(x, sub_list, at='@', workhorse=subm) {
  workhorse(x, at %.% names(sub_list), sub_list)
}

#' Patternize a string
#'
#' `str_patternize` wraps special characters in string name (possibly a vector) by 
#' brackets so that it can be matched within regular expression matching (the case of 
#' "\\" has to be treated differently). Useful for instance when working with file names.
#'
#' `str_unpatternize` does the reverse of \code{patternize()}.
#'
#' @examples
#' regexpr('notes.txt', 'notes_txt')>0                # TRUE
#' regexpr(str_patternize('notes.txt'), 'notes_txt')>0    # FALSE
#' regexpr(str_patternize('notes.txt'), 'notes.txt')>0    # TRUE
#'
#' @name patternize
#' @family string-manipulation functions provided by utilbox
#' @export
str_patternize = function(name, special=c("+",".","(",")","$","?","\\")) {
  for(i in seq_along(name))
    for(s in special) 
      name[i] = gsub(paste0("[",s,"]"), 
                     paste0("[", paste(rep(s,1+I(s=="\\")), collapse=""),"]"), 
                     name[i])
  return(name)
}

#' @rdname patternize
#' @export
patternize = str_patternize

#' @rdname patternize
#' @export
str_unpatternize = function(pattern) {
  gsub("[][]","",pattern)
}

#' @rdname patternize
#' @export
unpatternize = str_unpatternize

#' Escape special characters
#'
#' Escapes special characters in a string. It replaces `\\n` with a `\\\\n`, etc.
#'
#' @examples
#' str_escape('\n')
#'
#' @export
str_escape = function(x, specials=NULL, specials0 = c('\\n'='\\\\n')) {

  s = c(specials, specials0)
  for(i in seq_along(s)) {
    x = gsub(names(s)[i], s[i], x)
  }
  x

}

#' @rdname str_escape
#' @export
str_unescape = function(x,  specials=NULL, specials0 = c('\\n'='\\\\n')) {

  s = c(specials, specials0)
  for(i in seq_along(s)) {
    x = gsub(s[i], names(s)[i], x)
  }
  x

}

