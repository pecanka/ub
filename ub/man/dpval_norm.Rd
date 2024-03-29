% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvalues.R
\name{dpval_norm}
\alias{dpval_norm}
\title{Density of the normal distribution p-values}
\usage{
dpval_norm(p, m = 0, s = 1)
}
\description{
\code{dpval_norm()} returns the density of the distribution of the p-value associated
with the test statistic T distributed according to N(a,b^2), i.e., the normal
distribution with the expectation "a" and the standard deviation "b".
}
\examples{
# Draw values of the statistic
m = 0.75
s = 1.4
X = rnorm(1e5, m, s)
# Calculate the p-values and the density
P = 1 - pnorm(X)
d = dpval_norm(p <- ppoints(1000), m, s)
# Plot
hist(P, freq = FALSE)
lines(d~p, col='red', type='l')

}
