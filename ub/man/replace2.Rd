% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace.R
\name{replace2}
\alias{replace2}
\title{Value replacement}
\usage{
replace2(
  x,
  new_values,
  old_values,
  is_pattern = TRUE,
  number_new_values = FALSE
)
}
\description{
Replaces the values in \code{x} with \code{new_values} either in the order
of occurrence if \code{old_values} is missing, or using \code{old_values} for
reference.
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
\code{\link{replace3}()},
\code{\link{seq2}()},
\code{\link{seq_around}()},
\code{\link{sizechecks}},
\code{\link{split_into_groups}()},
\code{\link{to_factor}()},
\code{\link{what_bag}()}
}
\concept{sequence-related functions provided by ub}
