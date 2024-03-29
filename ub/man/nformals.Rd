% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{nformals}
\alias{nformals}
\title{Number of arguments a function accepts}
\usage{
nformals(fun, envir = parent.frame())
}
\description{
Returns the number of arguments that a given function \code{fun}
accepts. If the arguments of \code{fun} contain an ellipse (\code{...}), it
returns \code{Inf}.
}
\examples{
nformals(mode)
nformals(mean)

}
\seealso{
Other coding-related functions provided by ub: 
\code{\link{call_to_list}},
\code{\link{compare_functions}()},
\code{\link{fun_name}},
\code{\link{function_code_to_text}()},
\code{\link{function_dump_code}()},
\code{\link{function_find_dependencies}()},
\code{\link{function_modify}},
\code{\link{function_separate_code}()},
\code{\link{hide}()},
\code{\link{hijack}()},
\code{\link{is_error}()},
\code{\link{namespaces}},
\code{\link{null}()}
}
\concept{coding-related functions provided by ub}
