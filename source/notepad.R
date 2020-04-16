#' Add all utilbox objects into Notepad++ highlighting
#'
#' Modifies the notepad language keyword groups in order
#' for the utilbox objects to be highlighted by Notepad++.
#'
#' @export
notepad_highlighting = function(keywords=NULL, field='type1', npp_path, npp_langs, backup=TRUE) {

  catn("Notepad++ R highlighting settings will be modified so that all utilbox",
       " functions are highlighted.")

  # default locations of Notepad++ instalation and the name of the 'langs' file
  if(missing(npp_path)) npp_path = 'd:/Dropbox/Tools/Notepad++/Notepad++7/'
  if(missing(npp_langs)) npp_langs = 'langs.xml'

  file = file_path(npp_path, npp_langs)
  
  # get the default R keywords for the current "field"
  kw_df = keywords_default(field)

  # backup the current settings
  if(backup) {
    file_bak = file %.% '_' %.% timest(add_pid=FALSE) %.% '.bak'
    catn("Creating backup file for the settings file in ",file_bak," ...")
    file.copy (file, file_bak)
  }

  # read the file
  catn("Reading file '",file,"' ...")
  x = readLines(file)

  catn("Processing keywords ...")

  # locate the given R keyword entry (via 'keyword=' on input)
  update_lang_open = '<Language name=\"r\"'
  update_lang_close = '</Language>'
  update_kwrd_open = '<Keywords name=\"' %.% field %.% '\">'
  update_kwrd_close = '</Keywords>'

  w_lang_open = which(update_lang_open %mic% x)
  w_lang_close = first_above(which(update_lang_close %mic% x), w_lang_open)

  w_kwrd_open = first_above(which(update_kwrd_open %mic% x), w_lang_open)
  w_kwrd_close = first_above_soft(which(update_kwrd_close %mic% x), w_kwrd_open)
  stopifnot(!is_empty(w_kwrd_open), w_kwrd_open<=w_lang_close)
  stopifnot(!is_empty(w_kwrd_close), w_kwrd_close<=w_lang_close)

  # update the keywords
  kw_extra = if(field=='type1') {
    list_package_all(utilbox, pattern='^[^%]+$', mode='function')$name
  } else if(field=='instre2') {
    fbase = list_package_exported(base, pattern='^[.a-zA-Z0-9_]+$', mode='function')$name
    fstats = list_package_exported(stats, pattern='^[.a-zA-Z0-9_]+$', mode='function')$name
    futils = list_package_exported(utils, pattern='^[.a-zA-Z0-9_]+$', mode='function')$name
    c(fbase, fstats, futils)
  } else {
    catn("No names added with field '",field,"'.")
  }

  kw = sort(unique(c(kw_df, keywords, kw_extra)))

  # update the field
  wkw = w_kwrd_open:w_kwrd_close
  x_kw = collapse0(x[wkw])
  new_kw = '>' %.% collapse0(kw, sep=' ') %.% '<'
  x_kw = sub('>[^<>]+<', new_kw, x_kw)
  x[wkw] = c(x_kw, rep('', w_kwrd_close-w_kwrd_open))

  # save the results back to the file
  catn("Saving changes to file '", file,"' ...")
  writeLines(x, file)

  catn("Finished.")
  
}

keywords_default = function(nam) {
  
  kw = list(
    type1 = c(
      'acme','aids','aircondit','amis','aml','banking','barchart','barley',
      'beaver','bigcity','boot','brambles','breslow','bs','bwplot','calcium',
      'cane','capability','cav','censboot','channing','city','claridge',
      'cloth','cloud','coal','condense','contourplot','control','corr','darwin',
      'densityplot','dogs','dotplot','ducks','empinf','envelope','environmental',
      'ethanol','fir','frets','gpar','grav','gravity','grob','hirose','histogram',
      'islay','knn','larrows','levelplot','llines','logit','lpoints','lsegments',
      'lset','ltext','lvqinit','lvqtest','manaus','melanoma','motor','multiedit',
      'neuro','nitrofen','nodal','ns','nuclear','oneway','parallel','paulsen',
      'poisons','polar','qq','qqmath','remission','rfs','saddle','salinity',
      'shingle','simplex','singer','somgrid','splom','stripplot','survival',
      'tau','tmd','tsboot','tuna','unit','urine','viewport','wireframe','wool',
      'xyplot'),
    instre2 = c(
      'abbreviate','abline','abs','acf','acos','acosh','addmargins','aggregate',
      'agrep','alarm','alias','alist','all','anova','any','aov','aperm','append',
      'apply','approx','approxfun','apropos','ar','args','arima','array','arrows',
      'asin','asinh','assign','assocplot','atan','atanh','attach','attr','attributes',
      'autoload','autoloader','ave','axis','backsolve','barplot','basename','beta',
      'bindtextdomain','binomial','biplot','bitmap','bmp','body','box','boxplot',
      'bquote','break','browser','builtins','bxp','by','bzfile','c','call','cancor',
      'capabilities','casefold','cat','category','cbind','ccf','ceiling','character',
      'charmatch','chartr','chol','choose','chull','citation','class','close','cm',
      'cmdscale','codes','coef','coefficients','col','colnames','colors','colorspaces',
      'colours','comment','complex','confint','conflicts','contour','contrasts',
      'contributors','convolve','cophenetic','coplot','cor','cos','cosh','cov',
      'covratio','cpgram','crossprod','cummax','cummin','cumprod','cumsum','curve',
      'cut','cutree','cycle','data','dataentry','date','dbeta','dbinom','dcauchy',
      'dchisq','de','debug','debugger','decompose','delay','deltat','demo','dendrapply',
      'density','deparse','deriv','det','detach','determinant','deviance','dexp','df',
      'dfbeta','dfbetas','dffits','dgamma','dgeom','dget','dhyper','diag','diff',
      'diffinv','difftime','digamma','dim','dimnames','dir','dirname','dist','dlnorm',
      'dlogis','dmultinom','dnbinom','dnorm','dotchart','double','dpois','dput','drop',
      'dsignrank','dt','dump','dunif','duplicated','dweibull','dwilcox','eapply','ecdf',
      'edit','effects','eigen','emacs','embed','end','environment','eval','evalq',
      'example','exists','exp','expression','factanal','factor','factorial','family',
      'fft','fifo','file','filter','find','fitted','fivenum','fix','floor','flush',
      'for','force','formals','format','formula','forwardsolve','fourfoldplot','frame',
      'frequency','ftable','function','gamma','gaussian','gc','gcinfo','gctorture','get',
      'getenv','geterrmessage','gettext','gettextf','getwd','gl','glm','globalenv',
      'gray','grep','grey','grid','gsub','gzcon','gzfile','hat','hatvalues','hcl',
      'hclust','head','heatmap','help','hist','history','hsv','httpclient','iconv',
      'iconvlist','identical','identify','if','ifelse','image','influence','inherits','integer','integrate','interaction','interactive','intersect','invisible','isoreg','jitter','jpeg','julian','kappa','kernapply','kernel','kmeans','knots','kronecker','ksmooth','labels','lag','lapply','layout','lbeta','lchoose','lcm','legend','length','letters','levels','lfactorial','lgamma','library','licence','line','lines','list','lm','load','loadhistory','loadings','local','locator','loess','log','logb','logical','loglin','lowess','ls','lsfit','machine','mad','mahalanobis','makepredictcall','manova','mapply','match','matlines','matplot','matpoints','matrix','max','mean','median','medpolish','menu','merge','message','methods','mget','min','missing','mode','monthplot','months','mosaicplot','mtext','mvfft','names','napredict','naprint','naresid','nargs','nchar','ncol','next','nextn','ngettext','nlevels','nlm','nls','noquote','nrow','numeric','objects','offset','open','optim','optimise','optimize','options','order','ordered','outer','pacf','page','pairlist','pairs','palette','par','parse','paste','pbeta','pbinom','pbirthday','pcauchy','pchisq','pdf','pentagamma','person','persp','pexp','pf','pgamma','pgeom','phyper','pi','pico','pictex','pie','piechart','pipe','plclust','plnorm','plogis','plot','pmatch','pmax','pmin','pnbinom','png','pnorm','points','poisson','poly','polygon','polym','polyroot','postscript','power','ppoints','ppois','ppr','prcomp','predict','preplot','pretty','princomp','print','prmatrix','prod','profile','profiler','proj','promax','prompt','provide','psigamma','psignrank','pt','ptukey','punif','pweibull','pwilcox','q','qbeta','qbinom','qbirthday','qcauchy','qchisq','qexp','qf','qgamma','qgeom','qhyper','qlnorm','qlogis','qnbinom','qnorm','qpois','qqline','qqnorm','qqplot','qr','qsignrank','qt','qtukey','quantile','quarters','quasi','quasibinomial','quasipoisson','quit','qunif','quote','qweibull','qwilcox','rainbow','range','rank','raw','rbeta','rbind','rbinom','rcauchy','rchisq','readline','real','recover','rect','reformulate','regexpr','relevel','remove','reorder','rep','repeat','replace','replicate','replications','require','reshape','resid','residuals','restart','return','rev','rexp','rf','rgamma','rgb','rgeom','rhyper','rle','rlnorm','rlogis','rm','rmultinom','rnbinom','rnorm','round','row','rownames','rowsum','rpois','rsignrank','rstandard','rstudent','rt','rug','runif','runmed','rweibull','rwilcox','sample','sapply','save','savehistory','scale','scan','screen','screeplot','sd','search','searchpaths','seek','segments','seq','seq_along','sequence','serialize','setdiff','setequal','setwd','shell','sign','signif','sin','single','sinh','sink','smooth','solve','sort','source','spectrum','spline','splinefun','split','sprintf','sqrt','stack','stars','start','stderr','stdin','stdout','stem','step','stepfun','stl','stop','stopifnot','str','strftime','strheight','stripchart','strptime','strsplit','strtrim','structure','strwidth','strwrap','sub','subset','substitute','substr','substring','sum','summary','sunflowerplot','supsmu','svd','sweep','switch','symbols','symnum','system','t','table','tabulate','tail','tan','tanh','tapply','tempdir','tempfile','termplot','terms','tetragamma','text','time','title','toeplitz','tolower','topenv','toupper','trace','traceback','transform','trigamma','trunc','truncate','try','ts','tsdiag','tsp','typeof','unclass','undebug','union','unique','uniroot','unix','unlink','unlist','unname','unserialize','unsplit','unstack','untrace','unz','update','upgrade','url','UseMethod','var','varimax','vcov','vector','version','vi','vignette','warning','warnings','weekdays','weights','which','while','window','windows','with','write','wsbrowser','xedit','xemacs','xfig','xinch','xor','xtabs','xyinch','yinch','zapsmall'
    ))
    
  kw[[nam]]
  
}
