% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timer.R
\name{clock}
\alias{clock}
\alias{start_clock}
\alias{read_clock}
\alias{stop_clock}
\title{Clock control functions}
\usage{
clock(digs = 3)

start_clock(
  announce = TRUE,
  envir = ub_environment(),
  lead = "",
  what = "",
  out = ""
)

read_clock(announce = TRUE, envir = ub_environment())

stop_clock(
  announce = TRUE,
  envir = ub_environment(),
  lead = "",
  what = "",
  out = ""
)
}
\description{
Approximate timing of code execution. Useful to announce at what
times different steps in a (long) running script were started and
finished. The times are only approximate because when \code{announce=TRUE}
there is some time spent on the actual printing of the information
and this time (~0.1s) is not subtracted from the runtimes.

\code{start_clock()} starts the runtime clock.

\code{read_clock()} retrieves the runtime clock.

\code{stop_clock()} stops the runtime clock.

\code{clock()} gets the current system time.
}
\examples{
clock()

# start the clock
start_clock()
# do something that takes time
sleep(1.1)
# check how long it took
read_clock()
# do something else
sleep(1.5)
# another check how long it took and final stopping
read_clock()
stop_clock()

}
