% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesystem.R
\name{check_file_locked}
\alias{check_file_locked}
\title{File lock check}
\usage{
check_file_locked(file)
}
\description{
Check if a given file is locked by another application (e.g. by
Excel). Currently relies on a call to \code{wmic} and works on Windows
only.
}
\seealso{
Other file system function provided by ub: 
\code{\link{clean_filename}()},
\code{\link{file_backup}()},
\code{\link{file_can_open_check}()},
\code{\link{file_empty}()},
\code{\link{file_size}()},
\code{\link{file_sort}()},
\code{\link{map_drives}},
\code{\link{random_filename}()},
\code{\link{separate_path}()}
}
\concept{file system function provided by ub}
