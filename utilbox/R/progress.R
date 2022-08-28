#' Start progress bar
#'
#' `launchProgressBar()` creates a progress bar (object `..pb..` in the environment `envir`) 
#' and returns the updating function (invisibly). Besides returning it, it also puts the
#' updating function as `..pb_update..()` in `envir`.
#'
#' @export
launchProgressBar = function(pb_max, pb_print_every_n = 1, pb_show = TRUE, pb_initial=0, 
    pb_width = min(options()$width, 100), pb_con=stdout(), envir=parent.frame()) 
{

  if(pb_print_every_n < 1.0)
    pb_print_every_n = pb_print_every_n * pb_max
    
  pb_print_every_n = max(1, floor(pb_print_every_n))
    
  pb_stop_code = paste0('function() { setTxtProgressBar(..pb.., ',pb_max,'); close(..pb..) }')
  pb_update_code = paste0('function(n, loop=',pb_print_every_n,')',
                          ' { if(n %% loop == 0) setTxtProgressBar(..pb.., n) }')

  pb_stop = eval(str2lang(pb_stop_code))
  pb_update = eval(str2lang(pb_update_code))
  
  pb = txtProgressBar(min=0, max=pb_max, initial=if(pb_show) pb_initial else NA, 
                      style=3, width=pb_width, file=pb_con)
  
  environment(pb) = environment(pb_stop) = environment(pb_update) = envir

  assign('..pb..', pb, envir=envir)
  assign('..pb_stop..', pb_stop, envir=envir)
  assign('..pb_update..', pb_update, envir=envir)

  invisible(list(pb = pb, pb_update = pb_update, pb_stop = pb_stop))
  
}
