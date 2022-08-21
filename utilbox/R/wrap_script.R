#' @title
#' Wrap R code explanations
#'
#' @description
#' Takes an R script and \"wraps\" (i.e. add line breaks to limits 
#' width) the help explanations of what the functions do so that the 
#' lines do not exceed `max_width` too much. It places a new line right 
#' after the end of each word whose presence made the line length on 
#' which it is exceed `max_width`.
#'
#' @examples
#'
#' # Example with code supplied directly:
#' snippet1 = "#' Simple printing function\n#'\n#' " 
#' snippet2 = paste0(rep('Printing function.', t=30), collapse=' ')
#' snippet3 = "\n#'\n#' @example\n#' f('Hello world!')\n#'\n#' @export\n"
#' snippet4 = "f = function(x) {\n  print(x)\n}"
#' script_help_fix(code=paste0(snippet1, snippet2, snippet3, snippet4))
#'
#' @export
script_help_fix = function(file, code, max_width=70, eol="\n#' ", punctuation='.!?', 
    split_code=TRUE, split='\\n', verbose=TRUE) {
  
  if(equals(missing(file), missing(code))) {
    stop("Supply either file name (via `file`) or source code (via `code`).")
  }
  
  # read the file
  if(!missing(file)) {
    if(verbose) msgf("Adding wrapping to file '",file,"' ...")
    code = readLines(file)
  } else if(split_code) {
    code = unlist(str_cut(code, split))
  }
  
  # fix typos (where #" is in the place of #')
  code = gsub("^#\"","#'",code)
  
  # remove existing line breaks from the help lines
  Code = script_help_unwrap(code)
  
  # separate the code that has the "help lines of focus" on it
  focus = Code$focus
  RDs = Code$RDs
  Code = Code$Code
  
  # clean and wrap the help lines
  Code[focus] = script_help_clean_and_wrap(Code[focus], RDs[focus], max_width, eol, punctuation)

    # insert the altered lines instead of the old ones
  #Code = insert(Code, Help, Code$focus, replace_old=TRUE)
  #code[focus] = Help

  # save the results to file
  if(!missing(file)) {
  
    file2 = paste0(file, '.wrapped')
  
    writeLines(Code, file2)
    
    if(verbose) 
      msgf("Output saved to file '",file2,"'.")
      
  } else {
    file = file2 = NULL
  }  

  invisible(as.huge(nlist(Code, input_file=file, output_file=file2)))
  
}

#' @rdname script_help_fix
#' @export
script_help_unwrap = function(code, help_string="#'", pattern_help_line="^#'", 
    pattern_specials="#'[ ]*[@](example|family|export|rdname)", 
    pattern_rdname="@name|@rdname") {
  
  # identify help explanation lines
  is_help = grepl(pattern_help_line, code)
  is_help_empty = grepl("^[#]?\\s*$",code) & 
    de_na(rotate(is_help, 1, value=NA), FALSE) & 
    de_na(rotate(is_help, -1, value=NA), FALSE)
    
  if(any(is_help_empty)) is_help = is_help | is_help_empty
  
  # replace the empty lines within help with lines signifying help
  code[is_help_empty] = help_string
  
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
  hbT_begs_rel = lapply(hb_code, function(b) w_first_nonzero(!is_empty_line(b)))
  hbT_begs = lapply(seq(nblocks), function(i) hbT_begs_rel[[i]] + hb_begs[i] - 1)
  #rm(hbT_begs_rel)
  
  # find the second non-empty line in each block (i.e. the first description line)
  hbD_begs_rel = lapply(hb_code, function(b) 
    if(any(b %likei% "@description")) NULL else w_kth_nonzero(!is_empty_line(b),2))
  hbD_begs = lapply(seq(nblocks), function(i) hbD_begs_rel[[i]] + hb_begs[i] - 1)
  #rm(hbD_begs_rel)

  # figure out what the name of the current help block is (either via @name, 
  # @rdname or at looking at the name of the function that follows it)
  hb_rdnames = sapply(hb_code, function(b) {
      is_rdname = grep(pattern_rdname, b)
      ifelse(!any(is_rdname), length(b)+1, is_rdname[1])
    })
    
  hb_rdnames = extract_rdname(code[hb_begs - 1 + hb_rdnames])
  stopifnot(length(hb_rdnames)==length(hb_code))
  #hb_rdnames = rep(hb_rdnames, t=sapply(hb_code, length))
  
  # find the positions of the examples and family "specials" (@examples, @family)
  hb_specials = sapply(hb_code, function(b) {
      is_special = grep(pattern_specials, b)
      ifelse(!any(is_special), length(b)+1, is_special[1])
  })
  
  hb_is_exported = sapply(hb_code, function(b) any(b %likei% '@export'))
  
  # alter the block ends to ignore the lines below "specials"
  hb_ends = hb_begs - 1 + hb_specials - 1
  
  # store the ranges of lines to modify in a list (input for or_between)
  ht_modify = split_rows(cbind(hb_begs, hb_ends))
  
  # loop through the lines and merge those that correspond to a single
  # description in the blocks (j counts the lines in C, k counts the 
  # lines in the current description
  C = C_rdnames = rep("", length(code))
  is_code = NULL
  last_time_made_bottom = -1
  j = 0
  for(i in 1:length(code)) {
    
    j = j+1
    ib = which(sapply(hb_lines, function(bl) i %in% bl))
    
    # current line is not a description => save it unaltered
    if(!or_between(i, ht_modify)) {
      #k = b = 0
      C[j] = code[i]
      is_code = c(is_code, j)
      next
    }
    
    is_t_beg = i == (hbT_begs[[ib]] %||||% -1) && hb_is_exported[i]
    is_d_beg = i == (hbD_begs[[ib]] %||||% -1) && hb_is_exported[i]
    is_p_beg = code[i] %likei% '@param'
    is_s_beg = code[i] %likei% '@section'
    is_i_beg = code[i] %likei% '(\\item|\\enum|[!][[])'
    is_b_beg = code[i] %likei% "#'[ ]*[}]"
    is_empt = is_empty_line(code[i])
    is_empt_p = i>1 && is_empty_line(code[i-1])
    
    #if(i>=27) {print(code[i]); print(is_d_beg); browser()}
    
    if(is_t_beg && code[i] %notlikei% '@title') 
      C[j] = "#' @title\n"
      
    if(is_d_beg && code[i] %notlikei% '@description')
      C[j] = "#' @description\n"
    
    is_beg = is_t_beg || is_p_beg || is_s_beg || is_i_beg || is_b_beg
    
    # current line is the first line in the block or it is
    # an empty line (only "#'" on it) => save it unaltered
    if(i %in% hb_begs || is_empt || is_beg || hb_is_exported[i]) {
      #if(!is_p_beg) k = 0
      C[j] = paste0(C[j], code[i])
      C_rdnames[j] = hb_rdnames[ib]
      next
    }
    
    # current line is a continuation of a description started
    # on the previous line => append it to the previous line
    is_d_beg = is_d_beg || is_empt_p
    #k = k+1
    if(!is_d_beg) j = j-1
    C[j] = paste(str_trim_space(paste0(C[j], ifelse(is_d_beg, "#'", ""))), strip_start(code[i]))
    # save the current rdname    
    C_rdnames[j] = hb_rdnames[ib]
    
    last_time_made_bottom = i
    
  } # for(...)
  
  Code = head(C, j)
  RDs = head(C_rdnames, j)
  
  ## get rid of duplicate indicators of which lines were merged
  focus = setdiff(seq_along(Code), is_code)
  
  nlist(Code, focus, RDs)
  
}

#' @rdname script_help_fix
#' @export
script_help_clean_and_wrap = function(Help, RDs, max_width=70, eol="\n#' ", punctuation='.!?', 
  trim_trailing_space=TRUE, replace_tag_code=TRUE, scrub_multiple_spaces=TRUE) {

  # get rid of trailing spaces
  if(trim_trailing_space) 
    Help = str_trim_space(Help, 'right')

  # replace the long-winded \code{...} with `...`
  if(replace_tag_code) 
    Help = replace_code_tag(Help, RDs)
  
  # get rid of multiple consecutive spaces
  if(scrub_multiple_spaces)
    #Help = str_scrub_space(Help, pattern="([^#]')[ ]+", s="\\1 ")
    Help = gsub("([^#]')[ ]+", "\\1 ", Help, fixed=FALSE)

  ## and add full stops but only in the descriptions
  ##Help[focus2] = str_add_punct(Help[focus2], punct=punctuation)
  
  # wrap the lines
  long = nchar(Help) > max_width
  Help[long] = str_break(Help[long], max_width, eol=eol, break_only_at_space=TRUE)
  
  Help
  
}

#'
#' `strip_start()` removes the strings that indicate help lines (i.e., "#'").
#'
strip_start = function(x) {
  gsub("#'[ ]*","",x)
}

#'
#' `is_empty_line()` identifies "empty" lines, where "empty" means
#' that there is at most "#'" on the line.
#'
is_empty_line = function(x) {
  str_is_empty(strip_start(x), TRUE)
}

#'
#' `is_title_line()` identifies "title lines", which are those 
#'
is_title_line = function(x) {
  grepl("#'\\s*@title", x)
}
  
extract_rdname = function(x) {
  sub('[ ].*$','',sub("#'[ ]*@[r]?[d]?(name)[ ]", '', x))
}
  
#' Replace code tag in R help text
#' 
#' `replace_code_tag()` replaces all occurences of "\code{word}" with "`word`" 
#' or "`word()`" (when `add_brackets=TRUE`) or "[`word`]" (when `as_link=TRUE`).
#"
replace_code_tag = function(x, rds, add_brackets=FALSE, as_link=FALSE) {
  
  #pattern = '[\\]code[{]([a-zA-Z0-9:+_.()]+)[}]' 
  pattern = '[\\]code[{]([^{}}]+)[}]' 

  # replace the code tag with ticks
  repl = paste0(if(as_link) '[', '`\\1`', if(add_brackets) '()', if(as_link) ']')
  
  x = gsub(pattern, repl, x)
  
  # put all links in "code" font
  x = gsub('[[]([^`].+[^`])[]]', '[`\\1`]', x)
  
  # place brackets outside the ticks inside them
  x = gsub('[`]([]]?)[(][)]', '()`\\1', x)
  
  # remove potential double brackets
  x = gsub('[(][)][(][)]','()',x)
    
  x

}

