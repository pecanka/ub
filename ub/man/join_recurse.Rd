% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_joins.R
\name{join_recurse}
\alias{join_recurse}
\alias{full_join_recurse}
\alias{left_join_recurse}
\alias{right_join_recurse}
\alias{anti_join_recurse}
\alias{semi_join_recurse}
\alias{inner_join_recurse}
\title{Recursive joins}
\usage{
join_recurse(list, ..., join_fun)

full_join_recurse(list, ..., join_fun = "full_join")

left_join_recurse(list, ..., join_fun = "left_join")

right_join_recurse(list, ..., join_fun = "right_join")

anti_join_recurse(list, ..., join_fun = "anti_join")

semi_join_recurse(list, ..., join_fun = "semi_join")

inner_join_recurse(list, ..., join_fun = "inner_join")
}
\arguments{
\item{list}{A list of tibbles to be recursively joined.}

\item{...}{Arguments passed on to the join function.}

\item{join_fun}{The name of the join function to call (e.g.
\code{full_join} from package \code{dplyr})}
}
\value{
The result of joining the elements of \code{list} recursively.
}
\description{
\code{join_recurse()} performs a join of multiple data frames (or
tibbles) supplied in a list. In order to function properly it
requires a join function to be supplied and to exist. This can be for
instance those found in the package \code{dplyr} (\code{left_join},
\code{full_join}, etc.).
}
\section{Recursive joins}{


\code{full_join_recurse} is alias for \code{join_recurse} with
\code{join_fun=full_join}.

\code{left_join_recurse} is alias for \code{join_recurse} with
\code{join_fun=left_join}.

\code{right_join_recurse} is alias for \code{join_recurse} with
\code{join_fun=right_join}.

\code{anti_join_recurse} is alias for \code{join_recurse} with
\code{join_fun=anti_join}.

\code{semi_join_recurse} is alias for \code{join_recurse} with
\code{join_fun=semi_join}.

\code{inner_join_recurse} is alias for \code{join_recurse} with
\code{join_fun=inner_join}.

Naturally, all of these will fail if the function supplied in
\code{join_fun} does exists in the search path.
}

\examples{
a = tibble(day=c('Mon','Tue','Wed'), a=1:3)
b = tibble(day=c('Tue','Wed'), b=3:4)
c = tibble(day=c('Mon','Wed'), c=5:6)
join_recurse(list(a, b, c), by='day', join_fun=full_join)

}
\seealso{
Other matrix/data-frame functions provided by ub: 
\code{\link{add_col}()},
\code{\link{anti_join_sym}()},
\code{\link{identical_cols}()},
\code{\link{rep_df}()},
\code{\link{rev_rows}()},
\code{\link{split_rows}()}
}
\concept{matrix/data-frame functions provided by ub}
