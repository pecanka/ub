% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{diag_matrix}
\alias{diag_matrix}
\title{Diagonal matrix}
\usage{
diag_matrix(x, offdiag = 0)
}
\description{
\code{diag_matrix()} creates a diagonal matrix with \code{x} on the diagonal and
\code{offdiag} off of the diagonal in a row-wise fashion. If \code{offdiag}
has only half the required number of elements, it gets symmetrically
placed both above and below the diagonal, thereby leading to a
symmetric matrix.
}
\examples{
# diagonal matrix with 1:4 on the diagonal
diag_matrix(1:4)
# symmetric matrix with 1:4 on the diagonal and 3 everywhere else
diag_matrix(1:4, offdiag = 3)

}
