#' Start a parallel computation cluster
#'
#' `launchCluster()` starts a parallel cluster. It also creates several objects
#' in the parent functions calling frame, namely `.cl.` (the actual cluster),
#' `.stop.cluster.` (a function for stopping of the cluster), `.pb.` (progress bar)
#' and `.stop.pb.` (a function for stopping the progress bar). A call to `launchCluster()`
#' also executes `on.exit()` in the the parent functions calling frame,
#' which registers the cluster stopping function (i.e., `.stop.cluster.()`) to be
#' called when the parent function exits.
#'
#' `endCluster()` stops the parallel cluster and finishes the progress bar.
#
#' @examples
#' n = 100
#' opts_snow = launchCluster(pb_max=100)
#'  
#' v = foreach(i=1:n, .combine=rbind, .options.snow = opts_snow) %dopar% {
#'   mean(1:1000000)
#' }
#'
#' .stop.pb.()
#' .stop.cluster.()
#'
#' @export  
launchCluster = function(n_cores = parallel::detectCores()-1, max_n_cores=parallel::detectCores()-1,
    progress_bar = TRUE, pb_max, pb_print_every_n=1, pb_width=min(options()$width, 100), pb_initial=0,
    pb_show=TRUE, pb_hint=TRUE, announce=TRUE, con=stdout()) {
 
  if(announce) {
    message('Registering parallel cluster with ',n_cores,' cores ... ', appendLF=FALSE)
    flush.console()
  }
 
  n_cores = min(n_cores, max_n_cores)
  .cl. = parallel::makeCluster(n_cores)
  doSNOW::registerDoSNOW(.cl.)
 
  .stop.cluster. = function() try(parallel::stopCluster(.cl.), silent=TRUE)
 
  assign('.cl.', .cl., envir=parent.frame())
  assign('.stop.cluster.', .stop.cluster., envir=parent.frame())
  do.call("on.exit", list(quote(.stop.cluster.())), envir = parent.frame())
 
  if(announce) message('done.')

  if(progress_bar) {
 
    if(missing(pb_max)) {
      if(pb_hint) message('To also initialize a progress bar supply `pb_max` to `launchCluster()`.')
      return(invisible(list()))
    }

    .stop.pb. = eval(str2lang(paste0('function() { setTxtProgressBar(.pb., ',pb_max,'); close(.pb.) }')))

    .pb. = txtProgressBar(min=0, max=pb_max, initial=if(pb_show) pb_initial else NA, style=3, width=pb_width, file=con)
    assign('.pb.', .pb., envir=parent.frame())
    assign('.stop.pb.', .stop.pb., envir=parent.frame())
 
    progress = eval(str2lang(paste0('function(n, loop=',pb_print_every_n,') if(n %% loop == 0) setTxtProgressBar(.pb., n)')))

    assign('.pb.progress.', progress, envir=parent.frame())
    opts = list(progress = progress)
   
  } else {
    opts = list()
  }
 
 
 
  return(invisible(opts))

}

#' @rdname launchCluster
#' @export  
endCluster = function() {
  if(exists('.stop.pb.', envir=parent.frame()) && is.function(get('.stop.pb.', envir=parent.frame())))
    do.call('.stop.pb.', list(), envir=parent.frame())
  if(exists('.stop.cluster.', envir=parent.frame()) && is.function(get('.stop.cluster.', envir=parent.frame()))) {
    do.call('.stop.cluster.', list(), envir=parent.frame())
  } else {
    warning('Function `.stop.cluster.()` does not exist in the parent frame. No cluster to stop.', immediate.=TRUE)
  }
}