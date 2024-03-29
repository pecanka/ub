% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matching.R
\name{sub_at}
\alias{sub_at}
\alias{gsub_at}
\alias{substitute_at}
\title{Substitution of \"@\"-substrings}
\usage{
gsub_at(x, ..., at = "@")

sub_at(x, ..., at = "@")

substitute_at(x, sub_list, at = "@", workhorse = subm)
}
\description{
\code{sub_at()} and \code{gsub_at()} perform the replacement of
\"@-substrings\" inside a string \code{x}. \"@-substrings\" are strings
that start with the character '@' (or the value supplied in \code{at},
more generally). The replacement of these substrings is governed by
what follows the '@'. The string is then replaced (together with the
'@') with the values in the supplied arguments. The matching of the
\"@-substrings\" with the arguments can be controlled in three ways:

named vectors or named lists (e.g. \code{gsub_at(x, c(name='John',  city='Prague'))}.
\enumerate{
\item as the actual arguments when supplied as named arguments (e.g.
\code{gsub_at(x, name='John', city='Prague')}).
\item from the symbols supplied in the function call (e.g.
\verb{name='John'; city='Prague'; gsub_at(x, name, city)}).
}

In principal, these methods can be combined (see examples below).
Furthermore, one can leverage a combination of naming arguments and
supplying named vectors/lists, where the names of the arguments and
the names in the named vectors/lists are concatenated. Keep in mind
that whenever the argument is a named vector/list, the names are
concatenated with an added dot ('.'), while for unnamed vectors/lists
the concatenation happens without adding the dot.

\code{substitute_at()} is the general function which takes named
vectors/lists only and allows the specification of the workhorse
function (e.g. \code{sub}, \code{gsub}).
}
\examples{
x = '@name lived in @city1 and @city2. @city2 became @whose home.'
name = 'Jeff'; whose = 'his'; city1 = 'New York'; city2 = 'Prague'
gsub_at(x, name, city1, city2, whose)

# specify by naming arguments
gsub_at(x, name='Donna', city1='Prague', city2='Bonn', whose='her') 

# a combination of the two
gsub_at(x, name='Donna', whose='her', city1, city2) 

# or via a named vector
person = c(name='Donna', whose='her', city1='Prague', city2='Bonn')
gsub_at(x, person) 

# even a combination works
gsub_at(x, person[1:2], city2='Berlin', city1='Amsterdam') 

# relying on concatenation
gsub_at(x, person[1:2], city=c('Berlin','Amsterdam'))

}
\seealso{
Other string-manipulation functions provided by ub: 
\code{\link{ascii}()},
\code{\link{collapse}},
\code{\link{cumpaste0}()},
\code{\link{multi_sub}()},
\code{\link{padding}},
\code{\link{patternize}()},
\code{\link{str_cut}()},
\code{\link{str_diff}()},
\code{\link{str_grab}()},
\code{\link{str_insert}()},
\code{\link{str_is_empty}()},
\code{\link{str_paste_grid}()},
\code{\link{str_pos}()},
\code{\link{str_rev.character}()},
\code{\link{str_trim_space}()},
\code{\link{toupperfirst}()}
}
\concept{string-manipulation functions provided by ub}
