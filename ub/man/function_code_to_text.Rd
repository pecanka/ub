% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{function_code_to_text}
\alias{function_code_to_text}
\title{Get the source code of a function}
\usage{
function_code_to_text(fun, file = NULL)
}
\arguments{
\item{fun}{function for which the body of the code is desired}
}
\value{
A character vector with each command of the input
function as an element.
}
\description{
\code{function_code_to_text()} returns the body of the supplied function as
a character string.
}
\details{
This function takes an R function on input and returns
its source code as character vector.
}
\examples{
f <- function(x) { 
  print(x)
  x = 2
  invisible(x)
}
ff <- function_code_to_text(f)
print(ff)

}
\seealso{
Other coding-related functions provided by ub: 
\code{\link{call_to_list}},
\code{\link{compare_functions}()},
\code{\link{fun_name}},
\code{\link{function_dump_code}()},
\code{\link{function_find_dependencies}()},
\code{\link{function_modify}},
\code{\link{function_separate_code}()},
\code{\link{hide}()},
\code{\link{hijack}()},
\code{\link{is_error}()},
\code{\link{namespaces}},
\code{\link{nformals}()},
\code{\link{null}()}
}
\concept{coding-related functions provided by ub}
