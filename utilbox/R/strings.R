#' Cumulative paste0
#'
#' Takes a vector and cumulatively pastes it together.
#'
#' @examples
#' cumpaste0(c(0,1,1,1,0,1,0,0))
#'
#' @family string-manipulation functions provided by utilbox
#' @export
cumpaste0 = function(x, .sep = "") {
  Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)
}

#' Collapse a string vector
#'
#' Takes a vector and collapses it into a single string. Equivalent to
#' [base::paste] with `collapse=''`.
#'
#' @examples
#' collapse0(c(0,1,1,1,0,1,0,0))
#'
#' @name collapse
#' @family string-manipulation functions provided by utilbox
#' @export
collapse0 = function(x, sep = "") {
  paste(x, collapse=sep)
}

#' @rdname collapse
#' @export
collapse0n = function(x, sep = "\n") {
  paste(x, collapse=sep)
}

#' @rdname collapse
#' @export
collapse0nq = function(x, sep = "'\n'") {
  paste(x, collapse=sep)
}

#' Capitalization
#'
#' Capitalize first letters of each element in the vector \code{string}.
#'
#' @examples
#' toupperfirst('hello')
#'
#' @family string-manipulation functions provided by utilbox
#' @export
#toupperfirst = function(string) 
toupperfirst = function(...) {
  string = do.call('paste0', list(...)) 
  paste0(toupper(substring(string, 1, 1)), substring(string, 2))
}
      
#' Lagged differences for strings
#'
#' Compares neighbouring elements in a string vector and indicates
#' as `TRUE/FALSE` the differences. Returns `TRUE` if the corresponding
#' elements are different, and `FALSE` whet they are the same. Similar to
#' [base::diff] but for strings.
#'
#' @examples
#' str_diff(c('a','a','b','b','b','c'))
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_diff = function(x) {
  tail(x,-1) != head(x,-1)
}

#' String splitting
#'
#' Splits a string vector `x` by character string `split`. 
#' `str_split` is basically the same as [base::strsplit]
#' except that for zero-length strings it does not return 
#' a zero-length element in the returned list. `str2vector`
#' is an alias with the default value for split set to '' 
#' (an empty string) so that it ends up producing a vector
#' of individual characters. It also unlists the returned
#' value for scalar `x`.
#'
#' @examples
#' str_split(c('a-b','a-c','a-d','','c-d','c-e'),'-')
#'
#' @name str_split
#' @family string-manipulation functions provided by utilbox
#' @export
str_split = function(x, split, ..., unlist_for_scalar=FALSE) {
  y = lapply(base::strsplit(x, split=split, ...), `%||||%`, "")
  if(length(x)==1 && unlist_for_scalar) unlist(y) else y
}

#' String splitting
#'
#' Splits string into a vector of individual characters.
#'
#' @rdname str_split
#' @family string-manipulation functions provided by utilbox
#' @export
str2vector = function(x, split='', ..., unlist_for_scalar=TRUE) {
  str_split(x, split=split, ..., unlist_for_scalar=unlist_for_scalar)
}


#' String trimming
#'
#' Removes trailing spaces from the beginning and end of a string.
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_trim_space = function(x, side=c('both','left','right')) {
  side = match.arg(side)
  pattern = switch(side, both='^\\s+|\\s+$', left='^\\s+', right="\\s+$")
  gsub(pattern, '', x)
}

#' Remove extra white space
#'
#' Replaces multiple consecutive white spaces with a single one.
#'
#' @examples
#' str_scrub_space(' fas  fdas    fdsfas fsdafdfs         dfsa ')
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_scrub_space = function(x, pattern='[ ]+', s=' ', fixed=FALSE) {
  gsub(pattern, s, x, fixed=fixed)
}

#' Add punctuation
#'
#' Adds a punctuation (\code{p}) to the ends of all elements in a 
#' character vector that do not end in one of the punctuation marks
#' (\code{punct}).
#'
#' @examples
#' str_add_punct(c('hello','world!'), '!') # adds to the 1st, but not the 2nd
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_add_punct = function(x, p='.', punct='.!?', trim=TRUE, split_punct=TRUE) {
  if(split_punct && length(punct)==1 && nchar(punct)>1) {
    punct = unlist(strsplit(punct, ''))
  }
  no_punc = str_last(str_trim_space(x, side='right')) %notin% punct
  ifelse(no_punc, x %.% p, x)
}

#' Empty string check
#'
#' `str_is_empty` checks for empty strings, i.e. strings with zero
#' length. By default, white space is not trimmed prior to the check, 
#' but this can be enabled by adding `trim=TRUE` to the call.
#'
#' `str_is_empty_not` is returns the negation of what `str_is_empty` returns.
#'
#' `str_n_empty` counts the number of empty string in a character
#' vector. Same defaults as `str_is_empty`.
#'
#' @examples
#' str = c('hello','world','   ','')
#' str_is_empty(str)             # only the first is empty
#' str_is_empty(str, TRUE)       # both empty
#' str_is_empty_not(str)         # both non-empty
#' str_is_empty_not(str, TRUE)   # only the second is non-empty
#' str_n_empty(str)              # counts 1 empty
#' str_n_empty(str, TRUE)        # counts 2 empty
#' 
#' @name str_is_empty
#' @family string-manipulation functions provided by utilbox
#' @export
str_is_empty = function(x, trim=FALSE) {
  !nzchar(if(trim) str_trim_space(x) else x)
}

#' @name str_is_empty
#' @export
str_is_empty_not = function(x, trim=FALSE) {
  nzchar(if(trim) str_trim_space(x) else x)
}

#' @rdname str_is_empty
#' @export
str_n_empty = function(x, trim=FALSE) {
  sum(str_is_empty(x, trim=trim))
}

#' Insert substring into a string (additive)
#'
#' Inserts a substring into a string at a given position.
#' If the position in \code{pos} is larger than the character
#' count of the string, the substring either simply attached
#' to the end of the string (when \code{insert_white=FALSE},
#' default behavior) or the appropriate number of trailing 
#' white spaces are inserted before the substring is
#' attached.
#'
#' @examples
#' str_insert('My name is John Doe', ' not', 10)
#' str_insert(c('First name: ','Last name: ') , c('John','Doe'), 11)
#' str_insert(c('First name: ','Last name: ') , c('John','Doe'), 20, insert=TRUE)
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_insert = function(x, what, pos, insert_white=FALSE, str2_shift=0, workhorse=substring) {
  str1 = do.call(workhorse, list(x, 1, pos-1))
  if(insert_white) {
    nspace = pos - 1 - nchar(str1) + nchar(what)
    what = sprintf('%'%.%nspace%.%'s', what)
  }
  str2 = do.call(workhorse, list(x, pos + str2_shift, nchar(x)))
  str1 %.% what %.% str2
}

#' Insert substring into a string (replacive)
#'
#' Inserts a substring into a string at a given position instead 
#' of the characters contained in the string.
#'
#' If the position in \code{pos} is larger than the character
#' count of the string, the substring either simply attached
#' to the end of the string (when \code{insert_white=FALSE},
#' default behavior) or the appropriate number of trailing 
#' white spaces are inserted before the substring is
#' attached.
#'
#' @examples
#' str_replace('My name is John Doe', ' Jack', 11)
#' str_replace(c('First name: ','Last name: ') , c('John','Doe'), 20)
#' str_replace(c('First name: ','Last name: ') , c('John','Doe'), 20, insert=TRUE)
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_replace = function(x, substring, pos, insert_white=FALSE, str2_shift=0) {
  str_insert(x, substring, pos, insert_white, nchar(substring)-str2_shift)
}

#' Last characters in a string
#'
#' Gets the last \code{n} characters from a string. Vectorized.
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_last = function(x, n=1) {
  substr(x, nchar(x)-n+1, nchar(x))
}

#' Reverse the order of characters in a string
#'
#' @family string-manipulation functions provided by utilbox
#' @export
strrev = function(x) {
  sapply(x, function(y) paste(rev(str2vector(y)), collapse=""))
}

#' Find the first or last occurrence of a substring
#'
#' Find the first/last occurrence of a substring (given as regular pattern) 
#' in a string
#' 
#' @examples
#' strpos('hello world.', 'o', first=TRUE)
#' strpos('hello world.', 'o', last=TRUE)
#' strpos('hello world.', '[el]', first=TRUE)
#' strpos('hello world.', '[el]', last=TRUE)
#' strpos('hello world.', '.')
#' strpos('hello world.', '.', patternize=TRUE)
#'
#'
#' @family string-manipulation functions provided by utilbox
#' @export
strpos = function(string, substring, first=TRUE, last=FALSE, patternize=FALSE, fixed=TRUE) {
  if(missing(first)) first = !last
  if(missing(last)) last = !first
  if(patternize) substring = patternize(substring)
  all_pos = gregexpr(substring, string, fixed=fixed)
  sapply(all_pos, function(p) if(first) head(p,1) else tail(p,1))
}

#' Get ASCII code
#'
#' Returns an ASCII table code of a character in \code{x}.
#'
#' @family string-manipulation functions provided by utilbox
#' @export
ascii = function(x) {
  strtoi(charToRaw(x), 16L)
}

#' Format string
#'
#' `str_pad` pads to input to a given width (`width`). 
#' It formats the contents of `x` to a minimum width (character count) 
#' or other specified format (via `format`). The minimum length is 
#' easiest controlled via `min_width`. Non-character values are 
#' converted to character using `base::as.character`.
#'
#' `int_pad` pads an integer with leading zeros.
#'
#' `num_pad` pads a real/double with leading zeros.
#'
#' Note: Check out the function in options()$str$formatNum to see how R formats numbers.
#'
#' @name padding
#' @family string-manipulation functions provided by utilbox
#' @export
str_pad = function(x, min_width, format) {
  if(!is.character(x)) x = as.character(x)
  if(missing(min_width)) min_width = max(ndigits(x))
  if(missing(format)) format = '%0'%.%min_width%.%'d'
  sprintf(format, x)
}

#' @rdname padding
#' @export
int_pad = function(x, min_width, format, fmt='d') {
  
  if(missing(format)) {
    if(missing(min_width)) {
      min_width = max(ndigits(x))
    }  
    format = '%0' %.% min_width %.% fmt
  }
  
  sprintf(format, x)
  
}

#' @rdname padding
#' @export
num_pad = function(x, min_nint, min_ndig=6, fmt='f', format) {
  if(missing(format)) {
    if(missing(min_nint)) {
      min_nint = max(ndigits(int_part(x)))
    }  
    min_width = min_nint + I(x<0) + min_ndig + 1
    format = '%0' %.% min_width %.% fmt
  }
  
  sprintf(format, x)
}

#' Names for a list of combinations of vectors
#'
#' Produces names for a list of combinations of vectors. Takes vectors with 
#' parameter values and a vector of names and pastes them together in a 
#' cartesian product way. Can be used to names the elements of a list
#' which contains the results of a run of analysis for each combination 
#' on a grid (cartesian product) of parameter combinations.
#'
#' @examples
#' a = 1:3; b = 10:11; combine_names(a, b, names=c('a','b'))
#'
#' @family string-manipulation functions provided by utilbox
#' @export
combine_names = function(..., vars, sep1='=', sep2='_') {
  
  dots = match.call(expand.dots = FALSE)$`...`
  values = list(...)
  names(values) = dots
  
  if(missing(vars)) {
    last = values[[length(values)]]
    if(length(last)==length(values)-1) {
      vars = last 
      values[length(values)] = NULL
    } else {
      vars = names(values)
    }
  }
  
  stopifnot(length(values)==length(vars))
  
  v = lapply(seq_along(values), function(i) paste(vars[i],values[[i]], sep=sep1))
  do.call(paste, append(rev(do.call("expand.grid", rev(v))), list(sep=sep2)))
  
}
