#' @title
#' Get ASCII code
#'
#' @description
#'
#' Returns an ASCII table code of a character in `x`.
#'
#' @family string-manipulation functions provided by utilbox
#' @export
ascii = function(x) {
  strtoi(charToRaw(x), 16L)
}

#' @title
#' Cumulative paste0
#'
#' @description
#'
#' Takes a vector and cumulatively pastes it together.
#'
#' @examples
#' cumpaste0(c(0,1,1,1,0,1,0,0))
#'
#' @family string-manipulation functions provided by utilbox
#' @export
cumpaste0 = function(x, .sep="") {
  Reduce(function(x1, x2) paste(x1, x2, sep=.sep), x, accumulate = TRUE)
}

#' @title
#' Collapse a string vector
#'
#' @description
#'
#' `collapse0` takes a vector and collapses it into a single string. 
#' Equivalent to [`base::paste`] with `collapse=''`. The functions 
#' `collapse1`, `collapse0n`, `collapse0nq` are aliases for `collapse0` 
#' with different default values for `sep`.
#'
#' @examples
#' collapse0(c(0,1,1,1,0,1,0,0))
#'
#' @name collapse
#' @family string-manipulation functions provided by utilbox
#' @export
collapse0 = function(x, ..., sep="") {
  if(is.null(sep)) {
    x
  } else {
    paste(x, ..., collapse=sep)
  }
}

#' @rdname collapse
#' @export
str_collapse = collapse0

#' @rdname collapse
#' @export
collapse1 = function(x, ..., sep=" ") {
  paste(x, ..., collapse=sep)
}

#' @rdname collapse
#' @export
collapse0n = function(x, ..., sep="\n") {
  paste(x, ..., collapse=sep)
}

#' @rdname collapse
#' @export
collapsen = collapse0n

#' @rdname collapse
#' @export
collapse0nq = function(x, ..., sep="'\n'") {
  paste(x, ..., collapse=sep)
}

#' @title
#' Capitalization
#'
#' @description
#'
#' Capitalize first letters of each element in the vector `string`.
#'
#' @examples
#' toupperfirst('hello')
#'
#' @family string-manipulation functions provided by utilbox
#' @export
#toupperfirst = function(string) 
toupperfirst = function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
      
#' @title
#' Lagged differences for strings
#'
#' @description
#'
#' Compares neighbouring elements in a string vector and indicates as 
#' `TRUE/FALSE` the differences. Returns `TRUE` if the corresponding 
#' elements are different, and `FALSE` whet they are the same. Similar 
#' to [`base::diff`] but for strings.
#'
#' @examples
#' str_diff(c('a','a','b','b','b','c'))
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_diff = function(x) {
  tail(x,-1) != head(x,-1)
}

#' @title
#' String splitting
#'
#' @description
#'
#' Splits a string vector `x` by character string `split`. 
#' `str_split` is basically the same as [`base::strsplit`] except that 
#' for zero-length strings it does not return a zero-length element in 
#' the returned list. `str2vector` is an alias with the default value 
#' for split set to '' (an empty string) so that it ends up producing a 
#' vector of individual characters. It also unlists the returned value 
#' for scalar `x`.
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

#' @title
#' String splitting
#'
#' @description
#'
#' Splits string into a vector of individual characters.
#'
#' @rdname str_split
#' @family string-manipulation functions provided by utilbox
#' @export
str2vector = function(x, split='', ..., unlist_for_scalar=TRUE) {
  str_split(x, split=split, ..., unlist_for_scalar=unlist_for_scalar)
}


#' @title
#' String trimming
#'
#' @description
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

#' @title
#' Remove extra white space
#'
#' @description
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

#' @title
#' Add punctuation
#'
#' @description
#'
#' Adds a punctuation (`p`) to the ends of all elements in a 
#' character vector that do not end in one of the punctuation marks 
#' (`punct`).
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

#' @title
#' Insert substring into a string (additive)
#'
#' @description
#'
#' `str_insert()` inserts a substring into a string at a given 
#' position. If the position in `pos` is larger than the character count 
#' of the string, the substring either simply attached to the end of the 
#' string (when \code{insert_white=FALSE}, default behavior) or the 
#' appropriate number of trailing white spaces are inserted before the 
#' substring is attached.
#'
#' `str_replace()` does replacive insertions.
#'
#' @examples
#' str_insert('My name is John Doe', ' not', 10)
#' str_insert(c('First name: ','Last name: ') , c('John','Doe'), 11)
#' str_insert(c('First name: ','Last name: ') , c('John','Doe'), 20, insert=TRUE)
#'
#' str_replace('My name is John Doe', ' Jack', 11)
#' str_replace(c('First name: ','Last name: ') , c('John','Doe'), 20)
#' str_replace(c('First name: ','Last name: ') , c('John','Doe'), 20, insert=TRUE)
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

#' @rdname str_insert
#' @export
str_replace = function(x, substring, pos, insert_white=FALSE, str2_shift=0) {
  str_insert(x, substring, pos, insert_white, nchar(substring)-str2_shift)
}

#' @title
#' Extract the last characters in a string
#'
#' @description
#'
#' `str_last()` gets the last `n` characters from a string. 
#' Vectorized.
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_last = function(x, n=1) {
  substr(x, nchar(x)-n+1, nchar(x))
}

#' @title
#' Reverse the order of characters in a string
#'
#' @description
#'
#' `str_rev` takes character input (scalar, vector, list, etc) and 
#' reverses the order of characters in each of its elements. Generally, 
#' tries to preserve input class (i.e. lists stay lists).
#'
#' @examples
#' str_rev('world')
#' str_rev(list('hello', 'world'))
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_rev = function(x, simplify=!is.list(x)) {
  sapply(x, function(y) collapse0(rev(str2vector(y))), simplify=simplify, USE.NAMES=FALSE)
}

#' @title
#' Find the first or last occurrence of a substring
#'
#' @description
#'
#' `str_pos` finds the first (when `first=TRUE`) and/or the last 
#' (`last=TRUE`) occurrence of a substring (given as regular pattern) in 
#' a string.
#'
#' `str_first_occurence` returns the position of the first occurrence 
#' of `what` inside `x`, or the value in `miss` (-1 by default) if none 
#' find. It is functionally similar to `str_pos(..., first=TRUE)` except 
#' that it escapes (via [`str_escape`]) the substring in `what` first, 
#' allows a custom missingness indicator (`miss`) and is slightly faster.
#'
#' `str_last_occurence` is analogous to `str_first_occurence` except 
#' that it returns the position of the last occurrence of `what`
#'
#' @examples
#' str_pos('hello world.', 'o', first=TRUE)
#' str_pos('hello world.', 'o', last=TRUE)
#' str_pos('hello world.', '[el]', first=TRUE)
#' str_pos('hello world.', '[el]', last=TRUE)
#' str_pos('hello world.', '.')
#' str_pos('hello world.', '.', patternize=TRUE)
#'
#' str_first_occurence('hello world!', 'e')
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_pos = function(string, what, first=TRUE, last=FALSE, patternize=FALSE, escape=FALSE, fixed=TRUE) {

  if(missing(first)) first = !last
  if(missing(last)) last = !first

  if(patternize) what = str_patternize(what)
  if(escape) what = str_escape(what)
  
  all_pos = gregexpr(what, string, fixed=fixed)
  
  sapply(all_pos, function(p) if(first) head(p,1) else tail(p,1))
  
}

#' @rdname str_pos
#' @export
str_first_occurence = function(x, what, miss=-1) {
  p = c(unlist(regexpr('('%.%str_escape(what)%.%')', x)))
  ifelse(p < 0, miss, p)
}

#' @rdname str_pos
#' @export
str_last_occurence = function(x, what, miss=-1, escape=TRUE) {
  w = if(escape) str_escape(what) else what
  p = c(t1(unlist(gregexpr('(' %.%w%.%')', x))))
  ifelse(p < 0, miss, p)
}

#' @title
#' Format string
#'
#' @description
#'
#' `str_pad` pads to input to a given width (`width`).  It formats 
#' the contents of `x` to a minimum width (character count) or other 
#' specified format (via `format`). The minimum length is easiest 
#' controlled via `min_width`. Non-character values are converted to 
#' character using `base::as.character`. `str_lengthen()` is an alias
#' for `str_pad()` that exists primarily to allow easy disambiguation 
#' with `stringr::str_pad()`.
#'
#' `int_pad` pads an integer with leading zeros.
#'
#' `num_pad` pads a real/double with leading zeros.
#'
#' `spaces` produces an empty string of length `n`.
#'
#' Check the function in `options()$str$formatNum` to see how R 
#' formats numbers.
#'
#' @examples
#' str_pad('hello', 20)
#' str_pad('hello', 20, '.')
#' str_pad('hello', 20, 'right')
#'
#' @name padding
#' @family string-manipulation functions provided by utilbox
#' @export
str_pad = function(x, min_width, side=c('left','right'), padding=' ', nextra=0) {

  if(is_empty(x)) return(x)
  
  side = match.arg(side)

  if(!is.character(x)) x = as.character(x)
  if(missing(min_width)) min_width = max(nchar(x))
  min_width = min_width + nextra
  
  npd = ceiling((min_width - nchar(x))/max(1,nchar(padding)))
  pad = substr(spaces(npd, padding), 1, min_width-nchar(x))

  if(side=='left') {
    pad %.% x
  } else {
    x %.% pad
  }

}

#' @rdname padding
#' @export
str_lengthen = str_pad

#' @rdname padding
#' @export
int_pad = function(x, min_width, format, fmt='d') {
  
  if(is_empty(x)) return(x)
  
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

  if(is_empty(x)) return(x)
  
  if(missing(format)) {
    if(missing(min_nint)) {
      min_nint = max(ndigits(int_part(x)))
    }  
    min_width = min_nint + I(x<0) + min_ndig + 1
    format = '%0' %.% min_width %.% fmt
  }
  
  sprintf(format, x)
}

#' @rdname padding
#' @export
spaces = function(n, char=' ') {

  if(missing(n)) return(NULL)
  
  strrep(char, pmax(0,n))
  
}

#' @title
#' Wrap text
#'
#' @description
#'
#' Wrap text. In other words, place newline symbols (`\\n`) along a 
#' string to make its printout (via e.g. `base::cat`) \"wrap nicely\", 
#' that is not exceed a given width (set by `max_width`).
#'
#' `str_wrap_one` does the actual work for input of length 1, 
#' `str_wrap` is a wrapper around it useful for multivariate input (`x`).
#'
#' @examples
#' string = collapse0(rep(collapse0(letters[1:8]),10), sep=" ")
#'
#' # insert line breaks at given width
#' catn(str_wrap(string, max_width=20))
#'
#' # insert only at spaces
#' catn(str_wrap(string, max_width=20, break_only_at_space=TRUE))
#'
#' @export
str_wrap = function(x, max_width=Inf, eol='\n', break_only_at_space=FALSE) {

  sapply(x, str_wrap_one, max_width, eol, break_only_at_space, USE.NAMES=FALSE)
  
}

#' @title
#' @name str_wrap
#' @export
str_wrap_one = function(x, max_width=Inf, eol='\n', break_only_at_space=FALSE, max_nlines=Inf) {
  
  stopifnot(length(x)==1, length(max_width)==1, length(eol)==1)
  
  n = nchar(x)
  
  if(n<=max_width) return(x)
  
  wb = n
  weol = str_first_occurence(x, eol, Inf)
  wsp = 1 + str_last_occurence(substr(x,2,max_width), '\\s+', miss=Inf)
  
  if(weol<wb) wb = weol
  if(!break_only_at_space) {
    wb = max_width
  } else {
    if(wsp<wb) wb = wsp
    if(wb>max_width) {
      wb = str_first_occurence(substr(x,max_width,n), '\\s+', miss=n)
    }
  }
  
  if(wb >= n || wb==n-1 && str_last(x)==eol) {
    x
  } else {
    args = nlist(max_width, eol, break_only_at_space, max_nlines)
    substr(x, 1, wb) %.% ifelse(wb==weol, '', eol) %.% 
      do.call(Recall, list(substr(x, wb+1, n)) %append% args)
  }
  
}

#' @title
#' Grid pasting of strings
#'
#' @description
#'
#' `str_paste_grid()` produces a concatenation of all possible 
#' combinations of elements in in the supplied vectors.  for a list of 
#' combinations of vectors. Takes vectors with parameter values and a 
#' vector of names and pastes them together in a cartesian product way. 
#' Can be used to names the elements of a list which contains the 
#' results of a run of analysis for each combination on a grid 
#' (cartesian product) of parameter combinations.
#'
#' @examples
#' cities = c("Prague","London")
#' parts = c("InnerCity","Suburbs")
#' number = 1:5
#'
#' # combine two and three
#' str_paste_grid(cities, parts)
#' str_paste_grid(cities, parts, number)
#'
#' # specify the names directly
#' str_paste_grid(cities, parts, number, vars=c('City','Part','#'))
#'
#' # leave some names out
#' str_paste_grid(cities, parts, number, vars=c('City','Part'))
#'
#' # leave all names out
#' str_paste_grid(cities, parts, number, vars=NULL)
#'
#' # alter the separator
#' str_paste_grid(cities, parts, number, vars=NULL, sep2=":")
#'
#' @family string-manipulation functions provided by utilbox
#' @export
str_paste_grid = function(..., vars, sep1='=', sep2='_') {
  
  values = dots_to_nlist()
  
  if(missing(vars)) {
    vars = names(values)
  } 
  
  if(is_between(length(vars), 1, length(values)-1)) {
    vars = c(vars, rep("", length(values)))
  }
  
  v = if(is_empty(vars)) {
    values
  } else {
    lapply(seq_along(values), function(i) {
      if(str_is_empty(vars[i])) values[[i]] else paste(vars[i],values[[i]], sep=sep1)
    })
  }
  
  do.call(paste, append(rev(do.call("expand.grid", rev(v))), list(sep=sep2)))
  
}

#' @title
#' Shorten a string
#'
#' @description
#'
#' `str_abbreviate` takes a string and whenever it is long, it cuts 
#' out the middle section and replaces it with information about how 
#' many characters were cut out.
#'
#' @examples
#' let = collapse0(letters)
#' LET = collapse0(LETTERS)
#' str_abbreviate(collapse0(c(let, LET, let, LET)))
#'
#' @export
str_abbreviate = function(x, n1=12, n2) {

  if(missing(n2)) n2 = n1
  part1 = substr(x, 1, n1)
  part2 = substr(x, pmax(nchar(x)-n2+1, n1+1), nchar(x))
  ncut = nchar(x) - nchar(part1) - nchar(part2)
  x = part1 %.% msg_character_shortened(ncut) %.% part2
  
  structure(x, ncut=ncut, class=c('abbrevstr', class(x)))

}

