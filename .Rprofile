.First = function() {
  pckgs = c('ub','magrittr','dplyr')
  
  for(pckg in pckgs) {
    cat('Loading ',pckg,'  ...\n')
    try(library(pckg, character.only=TRUE))
    cat('Library ',pckg,' ',ifelse('utilbox'%in%names(utils::sessionInfo()$otherPkgs),
        'successfully','could not be'),' loaded.\n', sep='')
  }
  
}
