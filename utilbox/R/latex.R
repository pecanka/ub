#' @title
#' Fix LaTeX Bibliography
#'
#' @description
#'
#' `fix_bibliography()` puts author names into the 'Lastname1, 
#' Firstname1 and Lastname2, Firstname2' format in a bibtex bibliography 
#' file `infile`. It also abbreviates the first names with/without a dot 
#' and outputs the new bibliography into outfile Assumes that records 
#' are wrapped by '{' and '}' and not '"'. This limitation might be 
#' removed in the future.
#'
#' @export
fix_bibliography = function(infile, outfile, dot=".", lastname_first=TRUE,
  special_words="^[Vv]an$|^[Dd]e$|^[Dd]er$|^[Dd]os$") {

  # Read the bibliography
  cat("Reading bibliography from file '",infile,"' ...\n", sep="")
  x = scan(infile, what='character', sep='\n', blank.lines.skip=FALSE)

  # Loop through the author lines to process them
  cat("Processing bibliography ...\n")
  for(k in seq_along(x)) {

    if(!grepl("author.=", x[k])) next
  
    ## Extract the names only
    # Find the record opening opening 
    b1 = strpos(x[k], "{", first=TRUE)
    if(b1<0) stop("Could not locate '{' for line ",k,": ",x[k])
    b2 = strpos(x[k], "}", last=TRUE)
    if(b2<0) stop("Could not locate '}' for line ",k,": ",x[k])
    s1 = substr(x[k], 1, b1-1)
    s2 = substr(x[k], b1+1, b2-1)
    s3 = substr(x[k], b2+1, nchar(x[k]))
    
    ## An empty author found
    if(b1+1 > b2-1) {
      note("An empty author record found on line ",k,".")
      next
    }
    
    # Separate the names on 'and'
    n = unlist(strsplit(s2, " [Aa]nd "))
    
    # Check if author name contains "LaTeX protected strings"
    if(grepl("[{}]",s2)) {
      cat("Check if manual edit for author '",s2,"' required (line ",k,").\n")
      next
    }
    
    # Go through the names and abbreviate the first names
    for(i in seq_along(n)) {
      
      # If in "last name comma first names" format, transform it
      if(grepl("[,]",n[i])) {
        a = unlist(strsplit(n[i], "[,]"))
        n[i] = paste(str_trim(tail(a,-1)), str_trim(head(a,1)))
      }
      
      # Abbreviate first names
      a = unlist(strsplit(n[i], "[ .]"))
      a = a[nchar(a)>0]
      for(j in seq2(1,(length(a)-1),1)) {
        a[j] = if(!grepl(special_words,a[j])) {
          substr(a[j],1,1) %p% dot 
        } else {
          " " %p% a[j]
        }
      }
      
      # Paste the names back together
      if(lastname_first) {
      
        a = str_trim(a)
        split = length(a) - 1
        while(split>0) {
          if(!grepl(special_words,a[split])) break
          split = split - 1
        }
        
        n[i] = if(split==0) {
          collapse1(a) 
        } else {
          collapse1(tail(a,-split)) %p% ", " %p% collapse1(head(a, split))
        }
        
      } else {
        n[i] = collapse0(head(a, -1)) %p% tail(a,1)
      }
    }
    
    # Paste the author line back together
    x[k] = s1 %p% "{" %p% paste(n, collapse=" and ") %p% "},"
    
  }

  # Update the bibliography and save it to a file
  msgf("Saving bibliography to file '",outfile,"' ...")
  catn(collapse0n(x), file=outfile)
  
}

