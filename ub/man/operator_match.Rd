% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{operator_match}
\alias{operator_match}
\alias{\%m\%}
\alias{\%nm\%}
\alias{\%mi\%}
\alias{\%nmi\%}
\alias{\%like\%}
\alias{\%notlike\%}
\alias{\%likei\%}
\alias{\%notlikei\%}
\alias{\%likef\%}
\alias{\%notlikef\%}
\alias{\%likefi\%}
\alias{\%notlikefi\%}
\alias{\%likeany\%}
\alias{\%notlikeany\%}
\alias{\%likeanyi\%}
\alias{\%notlikeanyi\%}
\alias{\%likeanyf\%}
\alias{\%notlikeanyf\%}
\alias{\%likeanyfi\%}
\alias{\%notlikeanyfi\%}
\title{Regular expression match operator}
\usage{
pattern \%m\% x

lhs \%nm\% rhs

lhs \%mi\% x

lhs \%nmi\% x

x \%like\% rhs

x \%notlike\% rhs

x \%likei\% rhs

x \%notlikei\% rhs

x \%likef\% rhs

x \%notlikef\% rhs

x \%likefi\% rhs

x \%notlikefi\% rhs

x \%likeany\% rhs

x \%notlikeany\% rhs

x \%likeanyi\% rhs

x \%notlikeanyi\% rhs

x \%likeanyf\% rhs

x \%notlikeanyf\% rhs

x \%likeanyfi\% rhs

x \%notlikeanyfi\% rhs
}
\description{
\verb{\\\%m\\\%} is an operator version of \code{base::grepl}. It uses the left-hand
side argument (\code{lhs}) as the pattern and the right-hand side argument
(\code{rhs}) as the string that is compared with the pattern using a vectorized
version of \code{base::grepl}. The vectorization means that the operator can
take non-scalar arguments (on either side). It is required that the two
arguments have the same length or that at least one of them has length 1.
The return value has length of the longer argument.

\code{\%like\%} does the same except with the roles of the two
arguments are reversed, i.e., \code{lhs} is the string and \code{rhs} is the
pattern (analogous to the keyword 'LIKE' in SQL).

\verb{\\\%nm\%} and \verb{\\\%notlike\%} are the negations of the two operators.
\verb{\\\%mi\\\%}, \verb{\\\%likei\\\%}, \verb{\\\%nmi\\\%}, \verb{\\\%notlikei\\\%} are case-insensitive
versions of the four operators above.

\verb{\\\%likeany\\\%} is an \"any pattern\" version of \verb{\\\%like\\\%}. It checks
whether the elements of \code{lhs} match any of the elements in \code{rhs}, which
are the patterns. The two arguments can have any length. The return value
has the same length as \code{lhs}.

\verb{\\\%likeanyi\%} is a case-insensitive version of \verb{\\\%likeany\%}.
}
\examples{
# pattern on the left-hand side:
'ay' \%m\% 'daylight'              # TRUE
'ai' \%m\% 'daylight'              # FALSE
'ai' \%m\% 'daylight'              # FALSE
'ay' \%nm\% 'daylight'             # FALSE
'ai' \%nm\% 'daylight'             # TRUE

# pattern on the left-hand side (vectors):
c('ay','x') \%m\% 'daylight'       # TRUE, FALSE
'lig' \%m\% c('day','daylight')    # FALSE, TRUE
rep('lig',3) \%m\% c('day','ops')  # error

# pattern on the right-hand side:
'daylight' \%like\% 'ay'           # TRUE
'daylight' \%like\% 'ai'           # FALSE
'daylight' \%notlike\% 'ay'        # FALSE
'daylight' \%notlike\% 'ai'        # TRUE

# pattern on the right-hand side:
'daylight' \%like\% c('ay','x')    # TRUE, FALSE

# case insensitive versions:
'ay' \%mi\% 'DAYlight'             # TRUE
'aY' \%mi\% 'DAYlight'             # TRUE
'DAYlight' \%likei\% 'ay'          # TRUE
'DAYlight' \%likei\% 'aY'          # TRUE

# any-pattern matching:
c('Monday','January') \%likeany\% c('on','day','j')   # TRUE, FALSE, FALSE
c('Monday','January') \%likeanyi\% c('on','day','j')  # TRUE, FALSE, TRUE

}
\seealso{
Other operators provided by ub: 
\code{\link{\%anyin\%}()},
\code{\link{\%modify\%}()},
\code{\link{not_in}},
\code{\link{operator_NULL}},
\code{\link{operator_append}},
\code{\link{operator_c}},
\code{\link{operator_try}},
\code{\link{renaming_operators}}
}
\concept{operators provided by ub}
