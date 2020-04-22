#source('d:/Dropbox/Projects/R/utilbox/wrap/fix_wrap_utilbox_source.r')

source('d:/Dropbox/Projects/R/utilbox/source/wrap_script.R')

setwd2()

catn("THE HELP PORTIONS OF SOURCE CODE OF 'UTILBOX' WILL BE \"WRAPPED\":")

files = list.files('d:/Dropbox/Projects/R/utilbox/source', pattern='.R$', full=TRUE)
for(f in files) source(f)

##########
#files = files['R.R' %m% files]
files = files['sequences.R' %m% files]
##########

#wait('The following files will be altered:',files,sep='\n  ')

catn("Altering files ...")
ofiles = lapply(files, script_help_fix)
ofiles = sapply(ofiles, function(o) o$output_file)
catn('Files altered.')

catn('Checking for any unintended changes to the actual code ...')
check_ok = sapply(seq_along(files), function(i) compare_script_code(files[i], ofiles[i]))
catn('Finished.')

if(all(check_ok)) {
  catn("\nAll of the altered files seem to match (in terms of code) the originals.")
  catn("\nEVERYTHING SEEMS TO HAVE GONE OK.\n")
} else {
  warn("Some files have been unintentionally altered! Manually check what went wrong.")
  catn("Problematic files:",files[!check_ok], sep='\n  ')
}

