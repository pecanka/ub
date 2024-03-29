#' @title
#' Get ASCII code
#'
#' @description
#'
#' Returns an ASCII table code of a character in `x`.
#'
#' @family string-manipulation functions provided by ub
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
#' @family string-manipulation functions provided by ub
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
#' @family string-manipulation functions provided by ub
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
#' @family string-manipulation functions provided by ub
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
#' @family string-manipulation functions provided by ub
#' @export
str_diff = function(x) {
  tail(x,-1) != head(x,-1)
}

#' @title
#' String splitting
#'
#' @description
#'
#' `str_cut()` splits a string vector `x` by character string `split`. 
#' Basically, it is the same as [`base::strsplit`] except that 
#' for zero-length strings it does not return a zero-length element in 
#' the returned list. 
#'
#' `str2vector()` is an alias for `str_cut()` with the default value 
#' for split set to '' (an empty string) so that it ends up producing a 
#' vector of individual characters. It also unlists the returned value 
#' for scalar `x`.
#'
#' @examples
#' str_cut(c('a-b','a-c','a-d','','c-d','c-e'),'-')
#'
#' @name str_cut
#' @family string-manipulation functions provided by ub
#' @export
str_cut = function(x, split, ..., unlist_for_scalar=FALSE) {

  y = lapply(base::strsplit(x, split=split, ...), `%||||%`, "")
  
  if(length(x)==1 && unlist_for_scalar) unlist(y) else y
  
}

#' @rdname str_cut
#' @export
str2vector = function(x, split='', ..., unlist_for_scalar=TRUE) {
  str_cut(x, split=split, ..., unlist_for_scalar=unlist_for_scalar)
}

#' Remove empty substrings
#'
#' `str_drop_empty()` drops all elements in `x` that have only spaces in them.
#
#' @examples
#' str_drop_empty(c('a','',' ','   '))
#'
#' @export
str_drop_empty = function(x) {
  filter_out(x, '^[ ]*$')
}

#' @title
#' String trimming
#'
#' @description
#'
#' `str_trim_space()` removes trailing spaces from the beginning and 
#' the end of a string.
#'
#' `str_scrub_space()` replaces multiple consecutive white spaces with 
#' a single one. With `newline_is_space=TRUE` it treats the new line 
#' symbol `\\n` as a white space (and replaces it with a space).
#'
#' `str_add_punct()` adds a punctuation mark (given in `p`) to the ends 
#' of all elements in a character vector that do not end in one of the 
#' punctuation marks (`punct`).
#'
#' @examples
#' x = ' fas  fdas    fdsfas fsdafdfs         dfsa '
#' str_trim_space(x)
#' str_scrub_space(x)
#'
#' str_add_punct(c('hello','world!'), '!') # adds to the 1st, but not the 2nd
#'
#' @family string-manipulation functions provided by ub
#' @export
str_trim_space = function(x, side=c('both','left','right')) {
  side = match.arg(side)
  pattern = switch(side, both='^\\s+|\\s+$', left='^\\s+', right="\\s+$")
  gsub(pattern, '', x)
}

#' @rdname str_trim_space
#' @export
str_scrub_space = function(x, newline_is_space=FALSE) {
  if(newline_is_space) x = gsub('\\n', ' ', x, fixed=FALSE)
  gsub('[ ]+', ' ', x, fixed=FALSE)
}

#' @rdname str_trim_space
#' @export
str_add_punct = function(x, p='.', punct='.!?', trim=TRUE, split_punct=TRUE) {

  if(split_punct && length(punct)==1 && nchar(punct)>1) {
    punct = unlist(strsplit(punct, ''))
  }

  no_punc = str_last(str_trim_space(x, side='right')) %notin% punct

  ifelse(no_punc, paste0(x, p), x)

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
#' `str_overwrite()` does replacive insertions.
#'
#' @examples
#' str_insert('My name is John Doe', ' not', 10)
#' str_insert(c('First name: ','Last name: ') , c('John','Doe'), 11)
#' str_insert(c('First name: ','Last name: ') , c('John','Doe'), 20, insert=TRUE)
#'
#' str_overwrite('My name is John Doe', ' Jack', 11)
#' str_overwrite(c('First name: ','Last name: ') , c('John','Doe'), 20)
#' str_overwrite(c('First name: ','Last name: ') , c('John','Doe'), 20, insert=TRUE)
#'
#' @family string-manipulation functions provided by ub
#' @export
str_insert = function(x, what, pos, insert_white=FALSE, str2_shift=0, workhorse=substring) {

  str1 = do.call(workhorse, list(x, 1, pos-1))
  
  if(insert_white) {
    nspace = pos - 1 - nchar(str1) + nchar(what)
    what = sprintf(paste0('%', nspace, 's'), what)
  }
  
  str2 = do.call(workhorse, list(x, pos + str2_shift, nchar(x)))
  
  paste0(str1, what, str2)
  
}

#' @rdname str_insert
#' @export
str_overwrite = function(x, substring, pos, insert_white=FALSE, str2_shift=0) {
  str_insert(x, substring, pos, insert_white, nchar(substring)-str2_shift)
}

#' @title
#' Extract the last characters in a string
#'
#' @description
#'
#' `str_grab()` extracts the substring that matches the pattern
#' in `pattern` from a string.
#'
#' `str_first()` extracts (`drop=FALSE`) or removes (`drop=TRUE`) the first 
#' `n` characters from a string. 
#'
#' `str_last()` extracts (`drop=FALSE`) or removes (`drop=TRUE`) the last 
#' `n` characters from a string. 
#'
#' @family string-manipulation functions provided by ub
#' @export
str_grab = function(pattern, x, ..., workhorse=gregexpr) {
  regmatches(x, workhorse(pattern, x, ...))
}

#' @rdname str_grab
#' @export
str_first = function(x, n=1, drop=FALSE) {
  if(drop) {
    substr(x, n+1, nchar(x))
  } else {
    substr(x, 1, n)
  }
}

#' @rdname str_grab
#' @export
str_last = function(x, n=1, drop=FALSE) {
  if(drop) {
    substr(x, 1, nchar(x)-n)
  } else {
    substr(x, nchar(x)-n+1, nchar(x))
  }
}

#' @title
#' Reverse the order of characters in a string
#'
#' @description
#'
#' `str_rev` takes character input (character or numeric vector or a list 
#' of such vectors) and reverses the order of characters in each of its 
#' elements.
#'
#' @examples
#' str_rev('world')
#' str_rev(list('hello', 'world'))
#'
#' @family string-manipulation functions provided by ub
#' @export
str_rev.character = function(x) {
  unlist(lapply(strsplit(x, NULL), function(y) paste(rev(y), collapse='')))
}

str_rev.numeric = function(x) {
  str_rev.character(as.character(x))
}

str_rev.list = function(x) {
  lapply(strsplit(unlist(x), NULL), function(y) paste(rev(y), collapse=''))
  #lapply(x, function(y) paste(rev(unlist(strsplit(y, NULL))), collapse=''))
}

str_rev = function(x, ...) {
  UseMethod('str_rev')
}

#str_rev0 = function(x, simplify=!is.list(x)) {
#  sapply(x, function(y) paste(rev(str2vector(y)), collapse=''), simplify=simplify, USE.NAMES=FALSE)
#}

#' @title
#' Find the first or last occurrence of a substring
#'
#' @description
#'
#' `str_pos` finds the first (when `first=TRUE`) and/or the last (`last=TRUE`)
#' occurrence of a substring (given as regular pattern when `fixed=FALSE`, or
#' as a plain string when `fixed=TRUE`) in a string.
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
#' @family string-manipulation functions provided by ub
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
  p = c(unlist(regexpr(paste0('(', str_escape(what), ')'), x)))
  ifelse(p < 0, miss, p)
}

#' @rdname str_pos
#' @export
str_last_occurence = function(x, what, miss=-1, escape=TRUE) {
  w = if(escape) str_escape(what) else what
  p = c(tail(unlist(gregexpr(paste0('(', w, ')'), x)),1))
  ifelse(p < 0, miss, p)
}

#' @title
#' Format string
#'
#' @description
#'
#' `str_pad1` pads the input to a given width (`width`).  It formats 
#' the contents of `x` to a minimum width (i.e. character count) or other 
#' specified format (via `format`). The minimum length is controlled via 
#' `min_width` and `nextra`. Non-character values are converted to 
#' character using `as.character()`. 
#'
#' `str_pad0()` pads with '0' (by default at least one '0' is padded)
#'
#' `str_lengthen()` is an alias for `str_pad1()`.
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
#' str_pad0('21')
#' str_pad0(c('21','1'))
#' str_pad0(c('21','1'), 3)
#'
#' str_pad1('hello', 20)
#' str_pad1('hello', 20, '.')
#' str_pad1('hello', 20, side='right')
#'
#' str_pad1('hello', 20)
#' str_pad1('hello', 20, '.')
#' str_pad1('hello', 20, side='right')
#' str_pad1(c('hello','prague'), 8, '.')
#' str_pad1(c('hello','prague'), 4, '.')
#' str_pad1(c('hello','prague'), 4, '.', 1)
#'
#' @name padding
#' @family string-manipulation functions provided by ub
#' @export
str_pad0 = function(x, min_width=max(nchar(x))+1, padding='0', nextra=0, side=c('left','right')) {

  if(is_empty(x)) return(x)
  
  side = match.arg(side)

  if(!is.character(x)) x = as.character(x)
  min_width = min_width + nextra
  
  npd = ceiling((min_width - nchar(x))/max(1,nchar(padding)))
  pad = substr(spaces(npd, padding), 1, min_width-nchar(x))

  if(side=='left') {
    paste0(pad, x)
  } else {
    paste0(x, pad)
  }

}

#' @rdname padding
#' @export
str_pad1 = function(x, min_width=nchar(x), padding=' ', nextra=1, side=c('left','right')) {
  str_pad0(x, min_width, padding=padding, nextra=nextra, side=side)
}

#' @rdname padding
#' @export
str_lengthen = str_pad1

#' @rdname padding
#' @export
int_pad = function(x, min_width, format, fmt='d') {
  
  if(is_empty(x)) return(x)
  
  if(missing(format)) {
    if(missing(min_width)) {
      min_width = max(ndigits(x))
    }  
    format = paste0('%0', min_width, fmt)
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
    format = paste0('%0', min_width, fmt)
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
#' `str_break()` wraps (or more generally breaks) a string text. In other words, 
#' it places the value in `eol` (the new line symbol `\\n` by default) along a 
#' string to make its printout (via e.g. `base::cat`) \"wrap nicely\", 
#' that is not exceed a given width (set by `max_width`). By default,
#' it places the breaks (`eol`) anywhere, while with `break_only_at_space=TRUE` 
#' it only places the breaks between words.
#'
#' @examples
#' string = paste(rep('Rododendrons are pretty.',4), collapse=" ")
#'
#' # insert line breaks at given width
#' message(str_break(string, max_width=20))
#'
#' # insert only at spaces
#' message(str_break(string, max_width=20, break_only_at_space=TRUE))
#'
#' @export
str_break = function(x, max_width=Inf, break_only_at_space=FALSE, drop_space_after_newline=FALSE, eol='\n') {

  if(break_only_at_space) {
    str_break_at_space(x, max_width, drop_space_after_newline, eol)
  } else {
    str_break_anywhere(x, max_width, drop_space_after_newline, eol)
  }

}

#' Break string into lines (anywhere)
#'
#' Break strings into lines so that the maximum length of a line does
#' not exceed `max_width`. Line breaks are place anywhere (i.e., even
#' in the middle of words).
#'
str_break_anywhere = function(x, max_width=Inf, drop_space_after_newline=FALSE, eol='\n') {

  if(length(x)>1) {
    return(unlist(lapply(x, str_break_anywhere, max_width, drop_space_after_newline, eol)))
  }
  
  stopifnot(length(x)==1, length(max_width)==1, max_width>=1, length(eol)==1)
  
  n = nchar(x)
  
  if(n <= max_width) 
    return(x)

  nl = rep(c(rep('',max_width-1),'\n'), length.out=n)
  nl[length(nl)] = ''
  
  txt = unlist(strsplit(x, '\\n'))
  if(grepl('\\n$',x)) 
    txt = c(txt, '')
  
  if(drop_space_after_newline) {
    txt = lapply(txt, str_strip_eol_space, max_width)
  }
  
  txt = lapply(txt, function(y) paste(paste0(unlist(strsplit(y, NULL)), head(nl, nchar(y))), collapse=''))
  txt = lapply(txt, gsub, pattern='\\n$', replacement='')
  txt = paste(unlist(txt), collapse='\n')
  
  txt
  
}

#' Break string into lines (at white spaces only)
#'
#' Break strings into lines so that the maximum length of a line does
#' not exceed `max_width` except when there is a single word longer
#' than `max_width`). Lines are places only at white spaces.
#'
str_break_at_space = function(x, max_width=Inf, drop_space_after_newline=FALSE, eol='\n') {

  if(length(x)>1) {
    return(unlist(lapply(x, str_break_at_space, max_width, drop_space_after_newline, eol)))
  }
  
  stopifnot(length(x)==1, length(max_width)==1, max_width>=1, length(eol)==1)
  
  n = nchar(x)
  
  if(n <= max_width) 
    return(x)
    
  txt = unlist(strsplit(x, '\\n'))
  if(grepl('\\n$',x)) 
    txt = c(txt, '')
  
  #if(drop_space_after_newline) {
  #  txt = lapply(txt, str_strip_eol_space, max_width)
  #}
  
  txt = lapply(txt, function(y) {
                      ws = unlist(strsplit(y, '\\s+'))
                      W = ''
                      for(w in ws) {
                        nW = nchar(sub('.*\\n','',W))
                        sep = if(nW>0 && nW + nchar(w) > max_width) '\n' else if(nW>0) ' ' else ''
                        W = paste0(W, sep, w)
                      }
                      W
                    })

  txt = paste(unlist(txt), collapse='\n')
  
  txt
  
}

#' Strip space at the end of a line
#'
#' If a string in `y` has a space as each `width`+1 position, it is 
#' removed.
#'
str_strip_eol_space = function(y, width) { 
  
  if(nchar(y)<=width) return(y)
  
  ys = unlist(strsplit(y, NULL))
  
  idx = seq(width, length(ys)-1, by=width) 
  idx = idx + 1:length(idx)
  idx = idx[idx <= length(ys)]
  
  ys[idx] = ifelse(ys[idx]==' ', '', ys[idx])
  
  paste(ys, collapse='')
  
}

#' Count the number of lines in a string
#'
#' `str_n_lines()` counts the number of lines a string will print on 
#' (without further wrapping). Effectively, it counts the new line symbols 
#' (in `eol`) and adds 1.
#'
#' @examples
#' str_n_lines('this string\nhas only\nthree lines.')
#'
#' @export
str_n_lines = function(x, eol='\\n') {
  sapply(gregexpr(eol, x), length) + 1
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
#' @family string-manipulation functions provided by ub
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
#' let = paste(letters, collapse='')
#' LET = paste(LETTERS, collapse='')
#' str_abbreviate(paste(c(let, LET, let, LET), collapse=''))
#'
#' @export
str_abbreviate = function(x, n1=12, n2) {

  if(missing(n2)) n2 = n1
  part1 = substr(x, 1, n1)
  part2 = substr(x, pmax(nchar(x)-n2+1, n1+1), nchar(x))
  ncut = nchar(x) - nchar(part1) - nchar(part2)
  x = paste0(part1, msg_character_shortened(ncut), part2)
  
  structure(x, ncut=ncut, class=c('abbrevstr', class(x)))

}

