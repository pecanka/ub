#' Wrap R code explanations
#'
#' Takes an R script and \"wraps\" (i.e. add line breaks to limits width) 
#' the help explanations of what the functions do so that the lines do not exceed \code{max_width} 
#' too much. It places a new line right after the end of each word
#' whose presence made the line length on which it is exceed \code{max_width}.
#'
#' @examples
#' # Example with code supplied directly:
#' snippet1 = "#' Simple printing function\n#'\n#' " 
#' snippet2 = paste0(rep('Printing function.', t=30), collapse=' ')
#' snippet3 = "\n#'\n#' @example\n#' f('Hello world!')\n#'\n#' @export\n"
#' snippet4 = "f = function(x) {\n  print(x)\n}"
#' script_wrap_text(code=snippet1 %.% snippet2 %.% snippet3 %.% snippet4)
#'
#' @export
script_wrap_text = function(file, code, max_width=70, eol="\n#' ", punctuation='.!?', 
  split_code=TRUE, split='\\n', verbose=TRUE) {
  
  # --------------------- #
  # function to strip out the strings that indicate help lines
  strip_start = function(x) 
    gsub("#'[ ]*","",x)
    
  # function to indicate and "empty" (at most "#'" on it) line
  is_empty_line = function(x) 
    str_is_empty(strip_start(x), TRUE)

  # function to indicate and "empty" (at most "#'" on it) line
  is_title_line = function(x) 
    "#'\\s*@title" %m% x
    
  replace_code_tag = function(x, extra=":", as_link=TRUE)
    gsub('[\\]code[{]([a-zA-Z0-9_.()'%.%extra%.%']+)+[}]',
         ifelse(as_link,'[`','`')%.%'\\1'%.%ifelse(as_link,'`]','`'),x)
  
  # --------------------- #
  
  if(equals(missing(file), missing(code))) {
    error("Supply either file name (via file) or source code (via code).")
  }
  
  # read the file
  if(!missing(file)) {
    if(verbose) catn("Adding wrapping to file '",file,"' ...")
    code = readLines(file)
  } else if(split_code) {
    code = unlist(str_split(code, split))
  }
  
  # fix typos (where #" is in the place of #')
  code = gsub("^#\"","#'",code)
  
  # identify help explanation lines
  is_help = regexpr("^#'.*$",code)>0
  is_help_empty = (regexpr("^\\s*$",code)>0) & de_na(shift(is_help, 1, rotate=FALSE), FALSE)
  if(any(is_help_empty)) is_help = is_help | is_help_empty
  
  # replace the empty lines within help with lines signifying help
  code[is_help_empty] = "#'"
  
  # find the starts and ends of help blocks
  hb_begs = which(is_start_of_run(is_help) & is_help)
  hb_ends = which(is_end_of_run(is_help) & is_help)
 
  # check if the counts match
  stopifnot(length(hb_begs)==length(hb_ends))
  
  if(is_empty(hb_begs)) return(invisible(nlist(code)))
  
  nblocks = length(hb_begs)
  
  # split the help lines into blocks (corresponding to each section of code)
  hb_lines = lapply(1:nblocks, function(i) seq(hb_begs[i],hb_ends[i]))
  hb_code = lapply(hb_lines, function(n) code[n])  
  
  # find the first non-empty line in each block (i.e. the title line)
  hbt_begs_rel = lapply(hb_code, function(b) w_first_nonzero(!is_empty_line(b)))
  hbt_begs = lapply(seq(nblocks), function(i) hbt_begs_rel[[i]] + hb_begs[i] - 1)
  
  # find the second non-empty line in each block (i.e. the first description line)
  hbd_begs_rel = lapply(hb_code, function(b) w_nth_nonzero(!is_empty_line(b),2))
  hbd_begs = lapply(seq(nblocks), function(i) hbd_begs_rel[[i]] + hb_begs[i] - 1)

  # find the positions of the examples and family "specials" (@examples, @family)
  h_specials = sapply(hb_code, function(b) {
      is_specials = which(regexpr("#'[ ]*[@](example|family)", b)>0)
      if(!any(is_specials)) length(b)+1 else is_specials[1]})
  
  # alter the block ends to ignore the lines below "specials"
  hb_ends = hb_begs - 1 + h_specials - 1
  
  # store the ranges of lines to modify in a list (input for or_between)
  ht_modify = split_rows(cbind(hb_begs, hb_ends))
  
  # loop through the lines and merge those that correspond to a single
  # description in the blocks (j counts the lines in C, k counts the 
  # lines in the current description
  C = rep("", length(code))
  is_code = NULL
  j = 0
  for(i in 1:length(code)) {
    
    j = j+1
    ib = which(sapply(hb_lines, function(bl) i %in% bl))
    
    # current line is not a description => save it unaltered
    if(!or_between(i, ht_modify)) {
    
      k = b = 0
      C[j] = code[i]
      is_code = c(is_code, j)
      
    } else {
    
      ## if there are no non-empty lines in the current block, just ignore it
      #if(remove_empty_help_blocks && is_empty(hbt_begs[[ib]])) {
      #  j = j-1
      #  next
      #}

      is_t_beg = i == (hbt_begs[[ib]] %||||% -1)
      is_d_beg = i == (hbd_begs[[ib]] %||||% -1)
      is_p_beg = '@param' %m% tolower(code[i])
      
      if(is_t_beg && '@title' %nm% tolower(code[i])) 
        C[j] = "#' @title\n"
      if(is_d_beg && '@description' %nm% tolower(code[i])) {
        C[j] = "#' @description\n"
        #j = j+1
      }

      # current line is the first line in the block or it is
      # an empty line (only "#'" on it) => save it unaltered
      if(i %in% hb_begs || is_empty_line(code[i]) || is_t_beg || is_p_beg) {

        if(!is_p_beg) k = 0
        C[j] = C[j] %.% code[i]
      
      # current line is a continuation of a description started
      # on the previous line => append it to the previous line
      } else {
      
        k = k+1
        if(!is_d_beg) j = j-1
        C[j] = str_trim_space(C[j] %.% ifelse(is_d_beg, "#'", "")) %..% strip_start(code[i])
        #C[j] = C[j] %.% ifelse(is_d_beg, code[i], strip_start(code[i]))
        #focus2 = c(focus2, j)
        
      }
    }
    #print(i); print(code[i]); print(C[j]); if(i>=11) browser()
    
  } # for(...)
  
  # get rid of trailing spaces
  Code = str_trim_space(h1(C, j), 'right')
  
  ## get rid of duplicate indicators of which lines were merged
  focus = setdiff(seq_along(Code), is_code)
  
  # replace the long-winded \code{...} with `...`. first do the
  # references that are local, then separately do the ones that
  # are external and turn them into links via the use of square
  # brackets, that is '[...]'
  Code[focus] = replace_code_tag(Code[focus], '', FALSE)
  Code[focus] = replace_code_tag(Code[focus])
  
  # get rid of multiple consecutive spaces
  Code[focus] = str_scrub_space(Code[focus], pattern='[ ]+')

  ## and add full stops but only in the descriptions
  ##Code[focus2] = str_add_punct(Code[focus2], punct=punctuation)
  
  # focus only on the lines that exceed the character limit
  long = focus[nchar(Code[focus])>max_width]

  # split the line into multiple in case there are "\n" on it
  Code_long1 = lapply(Code[long], function(Cj) unlist(str_split(Cj, split)))
  #print(Code_long1); browser()
  
  # wrap the lines
  Code_long = lapply(Code_long1, function(Cjs) unlist(lapply(Cjs,  wrap_help_line, max_width, eol)))
  
  # insert the altered lines instead of the old ones
  Code = unlist(insert(Code, Code_long, long, replace=TRUE))
  
  # split the wrapped code into separate lines
  Code = unlist(str_split(Code, split))
  
  #browser()
  
  # save the results to file
  if(!missing(file)) {
    writeLines(Code, file2 <- file%.%'.wrapped') 
    if(verbose) catn("Output saved to file '",file2,"'.")
  } else {
    file = file2 = NULL
  }  

  invisible(as_huge(nlist(Code, input_file=file, output_file=file2)))
  
}

#' Wrap line of help
#'
#'
#' function that performs the actual wrapping
#' @export
wrap_help_line = function(text, max_width=70, eol="\n#' ") {

  Nj = nchar(text)
  
  # identify the positions of spaces for possible insertions of 'eol'
  w_space = unlist(unname(gregexpr('\\s+',text)))
  
  w_last_all = attempt = 0
  max_attempts = Nj
  while(Nj>max_width && any(w_space>0) && attempt<=max_attempts) {
  
    attempt = attempt + 1
    
    # find the last space currently
    w_last_now = w_space[w_last_below(w_space, max_width)]
    
    # remember the sum of all last space positions
    w_last_all = w_last_all + w_last_now
    
    # replace the last space with 'eol'
    text = str_replace(text, eol, w_last_all, str2_shift=nchar(eol)-1)
    
    # remove the space that was just altered and shift the 
    # positions of the spaces and the string length match
    # the positions and the lengths relative to the substring
    # after the insertion of 'eol'
    w_space = setdiff(w_space, w_last_now) - w_last_now + nchar(eol)-1
    Nj = Nj - w_last_now + nchar(eol)-1
    
  }

  if(attempt>max_attempts) {
    catn("PROBLEM: Maximum number of attempts to split the current line has been exceeded.")
    if(interactive()) {
      catn("Check the line ",i," (call 'Code[j]' (original) and 'c' (altered) to see the current line)")
      browser()
    }
  }
  
  return(text)

}

    