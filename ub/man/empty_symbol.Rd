% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expressions.R
\name{empty_symbol}
\alias{empty_symbol}
\alias{is_empty_symbol}
\title{Empty symbols}
\usage{
empty_symbol(x)

is_empty_symbol(x)
}
\description{
Some operations in R can result in a special object: the empty symbol.
The symbol can be obtained directly (e.g., via \code{quote(expr=)} as in
\code{empty_symbol()}) or indirectly (e.g., when applying \code{as.list()} to
a function, \code{as.list(function(x) {})$x}).

\code{empty_symbol()} returns the empty symbol object. \code{is_empty_symbol()}
checks its argument for identity with it.
}
\examples{
print(empty_symbol())  # prints the empty symbol
is_empty_symbol(empty_symbol())              # TRUE
is_empty_symbol(as.list(function(x) {})$x)   # TRUE

}
