% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttmake.R
\name{ttmake}
\alias{ttmake}
\title{Create a topTable for limma with "msd" based on confidence interval}
\usage{
ttmake(fit, coef = 1, sort = TRUE, ...)
}
\arguments{
\item{fit}{lmFit object}

\item{coef}{coefficient for which the output should be generated}

\item{sort}{whether the output should be sorted}

\item{...}{any other parameter passed to \code{topTable}}
}
\description{
Create a topTable for limma with "msd" based on confidence interval
}
\details{
Create a topTable for limma with the additional column "msd" based on confidence interval.

msd is defined as the lower boundary of the confidence interval for the
logFC, multiplied by the sum of the logFC. This allows to sort the list of
genes by both the confidence we have in the predicted logFC and its magnitude, rather than by
p-value alone (which can correspond to an small effect size) and logFC
(which can be very variable despite a large estimate).
}
