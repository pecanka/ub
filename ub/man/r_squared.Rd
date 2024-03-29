% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{r_squared}
\alias{r_squared}
\title{Coefficient of determination}
\usage{
r_squared(y, yh, na.rm = TRUE, adjusted = FALSE, k = NULL)
}
\description{
\code{r_squared()} calculates the coefficient of determination (the so-called
"R squared" for linear regression). With \code{adjusted=TRUE} the adjusted R
squared is calculated (adjusted for \code{k} parameters). Supply the observed
values of the response in \code{y} and the fitted values in \code{yh}, where the latter
can also be a model obtained via \code{stats::lm} or \code{stats::glm}.

\code{rmse()} calculates the square-root of the mean sum of squared errors from
the supplied observed (\code{y}) and fitted (\code{yh}) values, or more generally
the square-root of the mean sum of differences between the two vectors.
}
\examples{
x = rnorm(100)
y = 3*x + rnorm(100)
model = lm(y~x)
summary(model)[c('r.squared','adj.r.squared')]
r_squared(y, model$fitted)
r_squared(y, model$fitted, adjusted=TRUE, k=1)
r_squared(y, model)
r_squared(y, model, adjusted=TRUE)

}
