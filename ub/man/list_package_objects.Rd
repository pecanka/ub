% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packages.R
\name{list_package_objects}
\alias{list_package_objects}
\alias{list_package_exported}
\alias{list_package_all}
\alias{list_package_duplicates}
\alias{as_object_table}
\title{List objects in a package}
\usage{
list_package_objects(
  pckg,
  pattern,
  all.names = TRUE,
  exclude = FALSE,
  what = c("all", "exported"),
  dependencies = FALSE,
  mode = NULL,
  warn = TRUE,
  quietly = FALSE
)

list_package_exported(
  pckg,
  pattern,
  all.names = TRUE,
  exclude = FALSE,
  dependencies = FALSE,
  mode = NULL,
  warn = TRUE,
  quietly = FALSE
)

list_package_all(
  pckg,
  pattern,
  all.names = TRUE,
  exclude = FALSE,
  dependencies = FALSE,
  mode = NULL,
  warn = TRUE,
  quietly = FALSE
)

list_package_duplicates(pckgs, quietly = FALSE, sep = "|")

as_object_table(objs, pckg, pattern, exclude = FALSE, mode)
}
\description{
List all/exported objects in a package.

\code{list_package_all()} lists all objects defined in a package
(including non-exported objects).

\code{list_package_exported()} lists all exported objects by a package.

\code{as_object_table()} puts the objects and their main characteristics into
a single table (class \code{data.frame}).
}
\examples{
\dontrun{
list_package_exported('ub')
list_package_all('ub')
}

# lsf.str("package:dplyr")   # see also
# ls("package:dplyr")        # see also (works when the package is loaded)
# help(package = dplyr)
# see https://stackoverflow.com/questions/30392542/is-there-a-command-in-r-to-view-all-the-functions-present-in-a-package

}
