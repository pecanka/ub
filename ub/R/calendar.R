#' @title
#' Get today's date
#'
#' @description
#'
#' Returns today's date (in the current time zone) in the specified 
#' format.
#'
#' @param format Format of the output.
#' @param set_return_class A function that determines the class of 
#' the returned date. Uses \code{base::as.character} by default but for 
#' full dates, while \code{base::as.Date} can be also useful.
#'
#' @examples
#' t_day()     # current day
#' t_day('%Y') # current year
#' t_day('%m') # current month
#'
#' @family calendar functions provided by ub
#' @export
t_day = function(format="%Y-%m-%d", set_return_class=base::as.character) {
  set_return_class(format(Sys.time(), format))
}
  
#' @title
#' Get yesterday's date
#'
#' @description
#'
#' Calculated the date of the previous day or any other day relative 
#' to today in the current time zone in the specified format. How many 
#' days are shifted to the past is controlled via `lag`, which accepts 
#' vector input. Negative values in `lag` shift the date to the past, 
#' positive values shift the date into the future.
#'
#' @examples
#' y_day()          # yesterday
#' y_day(lag=7)     # a week ago
#' y_day(lag=-1)    # tomorrow
#' y_day(lag=-7)    # a week from today
#' y_day('%Y')      # the year of yesterday
#' y_day('%m')      # the month of yesterday
#'
#' @family calendar functions provided by ub
#' @export
y_day = function(format="%Y-%m-%d", lag=-1, relative_to_day, set_return_class=as.character) {
  if(missing(relative_to_day)) relative_to_day = t_day()
  day = as.Date(relative_to_day) + lag
  set_return_class(format(day, format))
}

#' @title
#' Find the closest day among dates
#'
#' @description
#'
#' Takes a vector of dates in `dates` and a single date in `day`, 
#' which are assumed to be in the 'YYYY-MM-DD' format, and find the 
#' closest value in `dates` to the value in `day`. If `position` is 
#' `TRUE`, the index of the closest date is returned, otherwise the 
#' closest date itself is returned (default).
#'
#' @examples
#' closest_day(c('2010-01-01', '2010-03-15', '2012-06-20'), '2011-01-08')
#'
#' @family calendar functions provided by ub
#' @export
closest_day = function(dates, day, position=FALSE) {
  w = which.min(abs(force_as_integer(dates)-force_as_integer(day)))
  if(position) w else dates[w]
}
  
