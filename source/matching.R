#' @title
#' Vectorized regular pattern matching
#'
#' @description
#'
#' Vectorized regular pattern matching, which takes multiple patterns.
#'
#' @rdname multi_grep
#' @export
multi_grep = function(pattern, x, ..., workhorse=base::grepl) {

  if(is_empty(x)) {
    return(x)
  }
  
  if(!is.character(pattern))
    stop("pattern must be type 'character'.")
    
  if(length(x)>1 && length(pattern) %nin% c(1,length(x)))
    stop("With 'x' non-scalar, 'pattern' must be a scalar or same length as 'x'.")

  if(length(pattern)>1 && length(x) %nin% c(1,length(pattern)))
    stop("With 'pattern' non-scalar, 'x' must be a scalar or same length as 'pattern'.")
    
  n = max(length(x), length(pattern))
  pattern = rep(pattern, length.out=n)
  x = rep(x, length.out=n)
  
  sapply(seq_along(x), function(i) workhorse(pattern[i], x[i], ...))
  
}

#' @rdname multi_grep
#' @export
grepm = function(pattern, x, ..., exclude=FALSE) {

  not_if(multi_grep(pattern, x, ..., workhorse=base::grep), exclude)
  
}

#' @rdname multi_grep
#' @export
greplm = function(pattern, x, ..., exclude=FALSE) {

  not_if(multi_grep(pattern, x, ..., workhorse=base::grepl), exclude)
  
}

#' @title
#' Recursive string substitution
#'
#' @description
#'
#' Allows for multiple arguments in `what` and `with` which are 
#' replaced inside `where`. `what` and `with` are processed in parallel 
#' which means that they must have the same length or `with` must be of 
#' length 1.
#'
#' `subm` leverages `base::sub`, while `gsubm` leverages 
#' `base::gsub`.
#'
#' @examples
#' subm(c('a','b'), paste0('|',c('a','b'),'|'), 'abracadabra')
#' gsubm(c('a','b'), paste0('|',c('a','b'),'|'), 'abracadabra')
#'
#' @family string-manipulation functions provided by utilbox
#' @export
multi_sub = function(x, what, with, workhorse=base::sub, ...) {
  
  if(is_empty(what)) {
    return(x)
  }
  
  if(is_scalar(with)) {
    with = rep(with, length(what))
  }
  if(is_scalar(what)) {
    what = rep(what, length(with))
  }
  
  stopifnot(length(what)==length(with))
  
  y = do.call(workhorse, list(what[1], with[1], x, ...))
  
  Recall(y, what[-1], with[-1], workhorse=workhorse, ...)
  
}
#' @rdname multi_sub
#' @export
subm = function(x, what, with) {
  multi_sub(x, what, with, workhorse=base::sub)
}

#' @rdname multi_sub
#' @export
gsubm = function(x, what, with) {
  multi_sub(x, what, with, workhorse=base::gsub)
}

#' @title
#' Substitution of \"@\"-substrings
#'
#' @description
#'
#' `sub_at()` and `gsub_at()` perform the replacement of 
#' \"@-substrings\" inside a string `x`. \"@-substrings\" are strings 
#' that start with the character '@' (or the value supplied in `at`, 
#' more generally). The replacement of these substrings is governed by 
#' what follows the '@'. The string is then replaced (together with the 
#' '@') with the values in the supplied arguments. The matching of the 
#' \"@-substrings\" with the arguments can be controlled in three ways:

#' @title
#' 1. based on the name attributes of the argument when supplied as
#' @description
#'
#' named vectors or named lists (e.g. `gsub_at(x, c(name='John', 
#' city='Prague'))`.
#'
#' 2. as the actual arguments when supplied as named arguments (e.g. 
#' `gsub_at(x, name='John', city='Prague')`).
#'
#' 3. from the symbols supplied in the function call (e.g. 
#' `name='John'; city='Prague'; gsub_at(x, name, city)`).
#'
#' In principal, these methods can be combined (see examples below). 
#' Furthermore, one can leverage a combination of naming arguments and 
#' supplying named vectors/lists, where the names of the arguments and 
#' the names in the named vectors/lists are concatenated. Keep in mind 
#' that whenever the argument is a named vector/list, the names are 
#' concatenated with an added dot ('.'), while for unnamed vectors/lists 
#' the concatenation happens without adding the dot.
#'
#' `substitute_at()` is the general function which takes named 
#' vectors/lists only and allows the specification of the workhorse 
#' function (e.g. `sub`, `gsub`).
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
  workhorse(x, paste0(at, names(sub_list)), sub_list)
}

#' @title
#' Patternize a string
#'
#' @description
#'
#' `str_patternize()` wraps special characters in string name 
#' (possibly a vector) by brackets so that it can be matched within 
#' regular expression matching (the case of "\\" has to be treated 
#' differently). Useful for instance when working with file names.
#'
#' `str_unpatternize()` does the reverse of `patternize()`.
#'
#' `str_escape()` escapes special characters in a string. By default, 
#' it escapes only the new line symbol (e.g. replaces `\\n` with a 
#' `\\\\n`).
#'
#' @examples
#' regexpr('notes.txt', 'notes_txt')>0                # TRUE
#' regexpr(str_patternize('notes.txt'), 'notes_txt')>0    # FALSE
#' regexpr(str_patternize('notes.txt'), 'notes.txt')>0    # TRUE
#'
#' str_escape('\n')
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

#' @title
#' Escape special characters
#' @rdname patternize
#' @export
str_escape = function(x, specials=NULL, specials0 = c('\\n'='\\\\n')) {

  s = c(specials, specials0)
  for(i in seq_along(s)) {
    x = gsub(names(s)[i], s[i], x)
  }
  x

}

#' @rdname patternize
#' @export
str_unescape = function(x,  specials=NULL, specials0 = c('\\n'='\\\\n')) {

  s = c(specials, specials0)
  for(i in seq_along(s)) {
    x = gsub(s[i], names(s)[i], x)
  }
  x

}

