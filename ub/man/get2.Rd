% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environments.R
\name{get2}
\alias{get2}
\alias{mget2}
\title{Get an object}
\usage{
get2(what, envir = parent.frame(), ifnotfound, mode = "any", inherits = TRUE)

mget2(what, envir = parent.frame(), ...)
}
\description{
\code{get2()} is nothing but a wrapper around \code{base::get0()}, which extracts
an object from the given environment (\code{envir}) unless the object does not
exist at which point the value in \code{ifnotfound} is returned. It differs from
\code{base::get0()} by the order of arguments and in that it does not have any
value set for \code{ifnotfound} by default.

\code{get2m()} extracts multiple objects and returns them in a list.
}
\examples{
get2('fklasdfjskadfjlsd', envir = .GlobalEnv, ifnotfound = NA)

}
