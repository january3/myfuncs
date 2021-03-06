% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getsubsets.R
\name{normtosel}
\alias{normtosel}
\title{Standarization using a subset of the data}
\usage{
normtosel(data, sel, min.var = 1e-16, param = FALSE)
}
\arguments{
\item{data}{either a matrix, data frame or an EList object}

\item{sel}{a logical vector specifying which samples to use for standarization}

\item{min.var}{if not NULL, all variables with variance / IQR below this threshold will be removed}

\item{param}{if TRUE, mean and sd will be used; if FALSE (default), median and IQR will be used for standarization}
}
\description{
Standarization using a subset of the data
}
\details{
Standarization using a subset of the data.
}
