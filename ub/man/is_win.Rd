% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system.R
\name{is_win}
\alias{is_win}
\alias{is_linux}
\alias{is_term}
\alias{is_rgui}
\alias{is_rstudio}
\title{Determine the kind of session and the system.}
\usage{
is_win()

is_linux()

is_term()

is_rgui()

is_rstudio()
}
\description{
\code{is_win} returns \code{TRUE} when the current system is Windows.

\code{is_linux} returns \code{TRUE} when the current system is Linux/Unix.

\code{is_term} returns \code{TRUE} when the current session runs inside a
terminal (as opposed to inside an interactive GUI).

\code{is_rgui} returns \code{TRUE} when the current session runs inside an
interactive GUI (as opposed to inside a terminal).

\code{is_rstudio} returns \code{TRUE} when the current session runs inside
RStudio.
}
\seealso{
Other system-related functions provided by ub: 
\code{\link{R_defaults}},
\code{\link{R_gui_adjust}()},
\code{\link{R_session_info}()},
\code{\link{environment_variables}},
\code{\link{stopping_control}},
\code{\link{system_path}},
\code{\link{timest}()},
\code{\link{wait}()}
}
\concept{system-related functions provided by ub}
