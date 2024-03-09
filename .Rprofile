.First = function() {
  pckgs = c('ub','magrittr','dplyr')
  
  for(pckg in pckgs) {
    cat('Loading ', pckg, ' ...\n', sep = '')
    try(library(pckg, character.only=TRUE))
    cat('Library ',pckg,ifelse('ub' %in% names(utils::sessionInfo()$otherPkgs),
        ' successfully loaded',' failed to load'),'\n', sep = '')
  }
  
}
