% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesystem.R
\name{clean_filename}
\alias{clean_filename}
\title{Drop dots from file names}
\usage{
clean_filename(x, chr = "-", keep_last_dot = TRUE)
}
\description{
\code{clean_filename()} replaces dots in file names with the value in
\code{chr}. Useful for instance for changing file names of plots that are
to be included in a latex file where the dots cause trouble.
}
\seealso{
Other file system function provided by ub: 
\code{\link{check_file_locked}()},
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
