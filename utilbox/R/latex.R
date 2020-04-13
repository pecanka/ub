#' Fix LaTeX Bibliography
#'
#' Puts authors' names into 'Lastname1, Firstname1 and Lastname2, Firstname2'
#' format in a bibtex bibliography file in 'infile', abbreviates the first 
#' names with/without a dot and outputs the new bibliography into outfile
#' Assumes that records are wrapped by '{' and '}' and not '"'. 
#' (Consider removing this limitation)
#' @export
fix_bibliography = function(infile, outfile, dot=".", lastname_first=TRUE,
  special_words="^[Vv]an$|^[Dd]e$|^[Dd]er$|^[Dd]os$") {

  # Read the bibliography
  cat("Reading bibliography from file '",infile,"' ...\n", sep="")
  x = scan(infile, what='character', sep='\n', blank.lines.skip=FALSE)

  # Loop through the author lines to process them
  cat("Processing bibliography ...\n")
  for(k in seq_along(x)) {

    if(regexpr("author.=", x[k])<0) next
  
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
    if(regexpr("[{}]",s2)>0) {
      cat("Check if manual edit for author '",s2,"' required (line ",k,").\n")
      next
    }
    
    # Go through the names and abbreviate the first names
    for(i in seq_along(n)) {
      
      # If in "last name comma first names" format, transform it
      if(regexpr("[,]",n[i])>0) {
        a = unlist(strsplit(n[i], "[,]"))
        n[i] = paste(str_trim(tail(a,-1)), str_trim(head(a,1)))
      }
      
      # Abbreviate first names
      a = unlist(strsplit(n[i], "[ .]"))
      a = a[nchar(a)>0]
      for(j in seq2(1,(length(a)-1),1))
        a[j] = if(regexpr(special_words,a[j])<=0) paste0(substr(a[j],1,1),dot) else paste0(" ",a[j])
      
      # Paste the names back together
      if(lastname_first) {
        a = str_trim(a)
        split = length(a) - 1
        while(split>0) {
          if(regexpr(special_words,a[split])<0) break
          split = split - 1
        }
        n[i] = if(split==0) paste(a, collapse=" ") else 
          paste0(paste(tail(a,-split), collapse=" "), ", ", paste(head(a, split), collapse=" "))
      } else {
        n[i] = paste(paste(head(a, -1), collapse=""), tail(a,1))
      }
    }
    
    # Paste the author line back together
    x[k] = paste0(s1,"{",paste(n, collapse=" and "),"},")
    
  }

  # Update the bibliography and save it to a file
  cat("Saving bibliography to file '",outfile,"' ...\n", sep="")
  cat(paste(x, collapse="\n"), file=outfile)
  
}

