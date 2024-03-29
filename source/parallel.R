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
#'
#' Info: parallel is the 'official' parallelization backend for R (basically).
#' doParallel is a package meant for use with foreach. Foreach lets you use a particular 
#' type of for-loop, called 'foreach', that looks like:
#'     foreach(i=listOfThings) %do% {thing with i}.
#' `foreach` allows this to be parallelized, using %dopar%:
#'     foreach(i=listOfThings) %dopar% {thing with i}.
#' 
#' *How* you parallelize with `%dopar%` depends on which backend you use. 
#' doParallel is one such backend - it tells foreach to use parallel. There are others, 
#' like doFuture (another parallel backend, using promises), doMPI (another parallel backend, 
#' using message passing interface; meant for high performance clusters), doRedis (another 
#' backend, using a Redis server/daemon to spin up workers), doSnow (another backend, using 
#' the SNOW package for creating parallel processes).
#' 
#' TLDR: parallel is the core parallelization package, now shipped with R as part of its core 
#' libraries. doParallel is a package addon to foreach that tells foreach to use a cluster 
#' defined by the parallel package.
#'
#' Using parallel requires making the cluster (makeCluster) and to export the object list 
#' (clusterExport) and packages (clusterEvalQ). 
#'
#' From https://www.reddit.com/r/rstats/comments/bkwr36/what_is_the_difference_between_the_doparallel_and/
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
launchCluster = function(n_cores = parallel::detectCores() - 1, max_n_cores = parallel::detectCores() - 1,
    launch = TRUE, progress_bar = TRUE, pb_max, pb_print_every_n = 1, pb_width = min(options()$width, 100),
    pb_initial = 0, pb_show = TRUE, pb_hint = TRUE, announce = TRUE, pb_con = stdout(), attach_on_exit = TRUE,
    envir = parent.frame()) {
 
  if(launch) {
 
    n_cores = min(n_cores, max_n_cores)

    if(announce) {
      message('Registering parallel cluster with ',n_cores,' cores ... ', appendLF=FALSE)
      flush.console()
    }
   
    stop_cluster = function() {
      try(parallel::stopCluster(..cl..))
    }
   
    cl = parallel::makeCluster(n_cores)
   
    environment(stop_cluster) = environment(cl) = envir
   
    assign('..cl..', cl, envir=envir)
    assign('..cl_stop..', stop_cluster, envir=envir)
   
    if(attach_on_exit) {
      do.call('on.exit', list(quote(..cl_stop..())), envir = envir)
    }
   
    doSNOW::registerDoSNOW(..cl..)
   
    if(announce) {
      message('done.')
    }
   
  }

  if(progress_bar) {
 
    if(missing(pb_max)) {
      if(pb_hint)
        message('To also initialize a progress bar supply `pb_max` to `launchCluster()`.')
      return(invisible(list()))
    }

    progress = launchProgressBar(pb_max, pb_print_every_n=pb_print_every_n, pb_show=pb_show, pb_width=pb_width,
                                 pb_initial=pb_initial, pb_con=pb_con, envir=envir)

    opts = progress
   
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
