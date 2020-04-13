#source('d:/Dropbox/Projects/R/utilbox/fix_wrap_utilbox_source.r')

source('d:/Dropbox/Projects/R/utilbox/source/script_management.r')

setwd2()

catn("THE HELP PORTIONS OF SOURCE CODE OF 'UTILBOX' WILL BE \"WRAPPED\":")

files = list.files('source', pattern='.R$', full=TRUE)

##########
files = 'test.R'
##########

#wait('The following files will be altered:',files,sep='\n  ')

catn("Altering files ...")
ofiles = lapply(files, script_wrap_text)
ofiles = sapply(ofiles, function(o) o$output_file)
catn('Files altered.')

catn('Checking for any unintended changes to the actual code ...')
check_ok = sapply(seq_along(files), function(i) check_same_code(files[i], ofiles[i]))
catn('Finished.')

if(all(check_ok)) {
  catn("\nAll produced files match (in terms of code) the originals.\n\nEVERYTHING SEEMS TO HAVE GONE OK.\n")
} else {
  warn("Some files have been unintentionally altered! Manually check what went wrong.")
  catn("Problematic files:",files[!check_ok], sep='\n  ')
}

