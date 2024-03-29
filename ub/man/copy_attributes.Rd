% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attributes.R
\name{copy_attributes}
\alias{copy_attributes}
\title{Copy attributes from one object to another}
\usage{
copy_attributes(to, from, ..., match_pattern = FALSE, fixed = FALSE)
}
\description{
Copy specified attributes from one object to another. The specification
can be done either by name or by pattern.
}
\examples{
x = structure(999, 'name_first'='john', name_second='doe')
y = 100
copy_attributes(y, x, 'name_first')

}
