#' Start progress bar
#'
#' `launchProgressBar()` creates a progress bar (object `..pb..` in the environment `envir`) 
#' and returns the updating function (invisibly). Besides returning it, it also puts the
#' updating function as `..pb_update..()` in `envir`.
#'
#' @export
launchProgressBar = function(pb_max, pb_min = 0, pb_initial = pb_min, pb_print_every_n = 1, pb_show = TRUE, 
    pb_width = min(options()$width, 100), pb_con = stdout(), envir = parent.frame()) 
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

#' Read input with a countdown
#'
#' `ask_enter_with_countdown()` asks for ENTER key for `n` seconds while
#' showing a coundown from `n` down to `1` after which point it exits
#' returning either the user input as character or `NULL` (on no input).
#'
#' @examples
#' x = ask_enter_with_countdown(3)
#'
#' @export
ask_enter_with_countdown = function(n = 10, msg = 'Press <ENTER> or continuing in {i} seconds ...') {

  n = max(1, n)
  timeout = round(n / 4, 1)
  j = 0
  i = n
  repeat {
  
    if(!is.null(msg)) {
      message(gsub('[{]i[}]', i, msg)); flush.console()
    }

    res = R.utils::withTimeout(readLines(n = 1), timeout = timeout, onTimeout = 'silent'); print('b')
    
    i = i - timeout
    j = j + timeout
    
    if(is.character(res) || i <= 0) {
      break
    }
    
  }
  
  if(is.null(res)) {
    message('Countdown reached zero. Continuing ...')
  } else {
    message('<ENTER> pressed.')
  }
  
  invisible(res)

}
