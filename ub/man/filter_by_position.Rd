% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{filter_by_position}
\alias{filter_by_position}
\alias{kth}
\alias{kthm}
\alias{kthr}
\alias{kthmr}
\alias{kth<-}
\alias{kthm<-}
\alias{kthr<-}
\alias{kthmr<-}
\title{Get every n-th element of an object}
\usage{
kth(x, k)

kthm(x, k)

kthr(x, k)

kthmr(x, k)

kth(x, k) <- value

kthm(x, k) <- value

kthr(x, k) <- value

kthmr(x, k) <- value
}
\description{
\code{kth} extracts the n-th element (single value) of an object \code{x}
(vector, list, etc.). It relies on \code{[[} for the extraction.

\code{kthm} allows for multivariate input in \code{n}. It relies on \code{[}
for the extraction.

\code{kthr} and \verb{kthmr are the analogue of }kth\code{and}kthm` which
determine index the position in reversed order.
}
\examples{
# extaction of value
kth(1:10, 2)             # second
kthm(1:10, 3:4)          # third and fourth
kthr(1:10, 2)            # second from the end
kthmr(1:10, 3:4)         # third and fourth from the end

# extaction and assignment of value
y = as.list(1:10)
kth(y, 2) = Inf       # second
kthm(y, 3:5) = Inf    # third thru fifth
kthr(y, 2) = Inf      # second from the end
kthmr(y, 3:5) = Inf   # third thru fifth from the end

# extract via logicals
kthm(y, sapply(y, is.finite))

}
\seealso{
Other sequence-related functions provided by ub: 
\code{\link{every_kth}()},
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
\code{\link{to_factor}()},
\code{\link{what_bag}()}
}
\concept{sequence-related functions provided by ub}
