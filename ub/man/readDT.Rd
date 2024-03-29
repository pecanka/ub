% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{readDT}
\alias{readDT}
\title{Read data table from an RDS file}
\usage{
readDT(file)
}
\description{
Function for reading data.table objects, which avoids losing its "identity"
(i.e., where the external pointer points) when being read from the disk,
which can be diagnosed by looking at whether the output of \code{data.table::truelength()}
on the data.table object is 0 or not (with 0 indicating that there is an issue).
}
