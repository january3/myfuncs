% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvals.R
\name{pval2str}
\alias{pval2str}
\alias{pval2star}
\title{Convert a p-value to a character corresponding to p-value levels}
\usage{
pval2str(pv, numonly = FALSE, ...)

pval2star(pv, ns = "", thresholds = c(0.05, 0.001, 1e-04))
}
\arguments{
\item{pv}{a p-value}

\item{numonly}{do not append stars to the output of \code{pval2str}}

\item{ns}{string to be used for values below the lowest threshold}

\item{thresholds}{numeric vector defining three thresholds for one, two
or three stars}
}
\value{
Both functions return character vector with p-values converted to respective strings.
}
\description{
Convert a p-value to a character corresponding to p-value levels
}
\details{
Convert a p-value to a character corresponding to p-value levels.
\code{pval2str} returns a formatted number with, optionally, stars denoting
the significance of the p-value. \code{pval2star} returns just the stars.
}
\examples{
# will return c( "**" )
pval2star( 0.0005 )
}
