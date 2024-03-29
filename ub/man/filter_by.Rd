% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{filter_by}
\alias{filter_by}
\alias{filter_by_value}
\alias{filter_by_pattern}
\alias{filter_by_name}
\alias{filter_by_bool}
\alias{filter_by_call}
\alias{filter_out}
\title{Filter an object based}
\usage{
filter_by_value(x, pattern, fixed = TRUE, exclude = FALSE, ignore.case = FALSE)

filter_by_pattern(
  x,
  pattern,
  fixed = FALSE,
  exclude = FALSE,
  ignore.case = FALSE
)

filter_by_name(x, pattern, fixed = FALSE, exclude = FALSE, ignore.case = FALSE)

filter_by_bool(x, keep, exclude = FALSE)

filter_by_call(...)

filter_out(x, pattern, fixed = FALSE, ignore.case = FALSE)
}
\description{
\code{filter_by_value} filters \code{x} based on whether its values match
contain \code{pattern} as substring (fixed pattern matching).

\code{filter_by_pattern} filters \code{x} based on whether its values match
the pattern in \code{pattern} (regular pattern matching).

\code{filter_by_name} filters \code{x} based on whether its names match the
pattern in \code{pattern}.

\code{filter_by_bool} filters \code{x} according to logical values in
\code{keep}. It is just an alias for \verb{x[}keep\verb{]}.

\code{filter_by_call} filters an object (\code{x}) based on whether the
results of a function (call given in \code{call}) match the pattern in
\code{pattern}. The \code{call} argument can be a standard R function (class
\code{function}) or a symbolic \code{tidyverse}-style representation of a
function, which uses the formula sign \code{~} to represent a
function with a single argument \code{.x} and which is evaluated with \code{x}
supplied to it as the argument \code{.x}.

In all of these function the pattern can be a vector at a match
against any of its components is sufficient.
}
\examples{
v = c(first='hello',second='amsterdam')

filter_by_value(v, 'ams')

filter_by_name(v, 'ams')

filter_by_bool(v, c(TRUE,FALSE))

filter_by_call(v, ~nchar(.x)>6)

filter_by_call(1:10, ~.x<5)

x = 1; f = function() { x = 2; filter_by_call(1:10, ~.x>x) }; print(f())

# both of these should work and print the sequence 3:10
f = function() { x = 2; filter_by_call(data.frame(z=1:10), ~.x$z>x) }; print(f())
f = function() { x = 2; filter_by_call(data.frame(z=1:10), ~z>x) }; print(f())
f = function() { x = 2; d = data.frame(z=1:10); filter_by_call(d, ~.x$z>x) }; print(f())
f = function() { x = 2; d = data.frame(z=1:10); filter_by_call(d, ~z>x) }; print(f())

}
\seealso{
Other sequence-related functions provided by ub: 
\code{\link{every_kth}()},
\code{\link{filter_by_position}},
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
\code{\link{to_factor}()},
\code{\link{what_bag}()}
}
\concept{sequence-related functions provided by ub}
