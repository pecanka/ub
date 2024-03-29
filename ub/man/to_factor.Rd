% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_factor.R
\name{to_factor}
\alias{to_factor}
\alias{un_factor}
\title{Convert to factor and back}
\usage{
to_factor(
  x = character(),
  levels = unique(x),
  labels = levels,
  exclude = NA,
  ordered = FALSE,
  nmax = NA,
  fun = sort
)

un_factor(x, to = c("character", "numeric"))
}
\description{
\code{to_factor()} converts a vector to class \code{factor} with levels
determined by order of appearance and \strong{without} sorting of
levels (unlike \code{base::as.factor}). It also completes the
levels to contain all unique values in \code{x}, which is useful when
one wants to only supply the first few values and let the rest be
taken from \code{x}. The rest of the values are sorted by default, but
this can be changed via \code{fun} (e.g., \code{fun=identity} to keep the
order of appearance). The completion of levels can be useful for
instance when using \code{to_factor} instead of factor when sorting
a data set (see the examples with \code{dplyr::arrange} below).

\code{un_factor()} removes the type factor produced by \code{to_factor()}.
}
\examples{
to_factor(c('Germany','Italy','Czechia'))

un_factor(to_factor(c('Germany','Italy','Czechia')))
un_factor(to_factor(c('Germany','Italy','Czechia')), 'numeric')

# sorting (both achieve the same, to_factor is more succinct)
\dontrun{
dplyr::arrange(mtcars, to_factor(carb, 3:2))
dplyr::arrange(mtcars, factor(carb, union(3:2, sort(carb))))
}

}
\seealso{
Other sequence-related functions provided by ub: 
\code{\link{every_kth}()},
\code{\link{filter_by_position}},
\code{\link{filter_by}},
\code{\link{groups_of_unique}()},
\code{\link{h1}()},
\code{\link{insert}()},
\code{\link{is_between}()},
\code{\link{is_run}},
\code{\link{last_element}()},
\code{\link{rep2}()},
\code{\link{replace2}()},
\code{\link{replace3}()},
\code{\link{seq2}()},
\code{\link{seq_around}()},
\code{\link{sizechecks}},
\code{\link{split_into_groups}()},
\code{\link{what_bag}()}
}
\concept{sequence-related functions provided by ub}
