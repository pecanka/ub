#' Start a parallel computation cluster
#'
#' `launchCluster()` starts a parallel cluster (unless `launch=FALSE`). While doing so,
#' it also creates several objects in the parent functions calling frame, namely `..cl..` 
#' (the actual cluster), and `..cl_stop..` (a function for stopping of the cluster), 
#' and `..pb..` (progress bar), `.pb.update()` (a function for updating the progress bar) 
#' and `..pb_stop..()` (a function for stopping the progress bar). 
#' A call to `launchCluster()` also executes `on.exit()` in the the parent functions 
#' calling frame, which registers the cluster stopping function (i.e., `..cl_stop..()`) 
#' to be called when the parent function exits. Use `launch=FALSE` with `progress_bar=TRUE` 
#' to only initialize the progress bar without starting a new cluster (e.g., when an
#' existing cluster is to be used instead of a new one).
#'
#' `endCluster()` stops the parallel cluster and finishes the progress bar (by executing
#' `..cl_stop..()` and `..pb_stop..()` in its parent frame.
#
#' @examples
#' n = 100
#' opts_snow = launchCluster(pb_max=100)
#'  
#' v = foreach(i=1:n, .combine=rbind, .options.snow = opts_snow) %dopar% {
#'   mean(1:1000000)
#' }
#'
#' # stop the cluster and progress_bar
#' endCluster()
#' # or alternatively call the two functions directly:
#' # ..pb_stop..()
#' # ..cl_stop..()
#'
#' @export  
launchCluster = function(n_cores = parallel::detectCores()-1, max_n_cores=parallel::detectCores()-1, 
    launch = TRUE, progress_bar = TRUE, pb_max, pb_print_every_n = 1, pb_width=min(options()$width, 100), 
    pb_initial=0, pb_show=TRUE, pb_hint=TRUE, announce=TRUE, pb_con=stdout(), envir=parent.frame()) {
  
  if(launch) {
  
    n_cores = min(n_cores, max_n_cores)

    if(announce) {
      message('Registering parallel cluster with ',n_cores,' cores ... ', appendLF=FALSE)
      flush.console()
    }
    
    #stop_cluster = function() try(parallel::stopCluster(..cl..), silent=TRUE)
    stop_cluster = function() try(parallel::stopCluster(..cl..))
    cl = parallel::makeCluster(n_cores)
    
    environment(stop_cluster) = environment(cl) = envir
    
    assign('..cl..', cl, envir=envir)
    assign('..cl_stop..', stop_cluster, envir=envir)
    do.call('on.exit', list(quote(..cl_stop..())), envir = envir)
    
    doSNOW::registerDoSNOW(..cl..)
    
    if(announce) message('done.')
    
  }

  if(progress_bar) {
  
    if(missing(pb_max)) {
      if(pb_hint) 
        message('To also initialize a progress bar supply `pb_max` to `launchCluster()`.')
      return(invisible(list()))
    }

    progress = launchProgressBar(pb_max, pb_print_every_n=pb_print_every_n, pb_show=pb_show, 
                                 pb_width=pb_width, pb_initial=pb_initial, pb_con=pb_con, 
                                 envir=envir)

    opts = list(progress = progress)
    
  } else {
    opts = list()
  }
  
  return(invisible(opts))

}

#' @rdname launchCluster
#' @export  
endCluster = function(envir=parent.frame(), warn=TRUE) {

  if(exists('..pb_stop..', envir=envir) && is.function(get('..pb_stop..', envir=envir))) 
    do.call('..pb_stop..', list(), envir=envir)
    
  if(exists('..cl_stop..', envir=envir) && is.function(get('..cl_stop..', envir=envir))) {
    do.call('..cl_stop..', list(), envir=envir)
  } else if(warn) {
    warning('Function `..cl_stop..()` does not exist in the parent frame. No cluster to stop.', immediate.=TRUE)
  }
  
}
