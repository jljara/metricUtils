% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.COR.R
\name{get.COR}
\alias{get.COR}
\alias{get.COR.default}
\title{Gets a correlation value for two vectors.}
\usage{
get.COR(sim, obs, ...)

\method{get.COR}{default}(
  sim,
  obs,
  use = "everything",
  method = c("pearson", "kendall", "spearman"),
  ...
)
}
\arguments{
\item{sim}{A vector with the predicted values}

\item{obs}{A vector with the observed values}

\item{...}{Ignored}

\item{use, }{method Pass to an internal call to the
\code{stats::cor()} function}
}
\value{
A logical vector with \code{TRUE} if the corresponding
        value in \code{x} is divisible by the corresponding value
        in \code{y} considering the specified tolerance level;
        \code{FALSE} otherwise
}
\description{
Calculates a correlation between two vectors.
}
\details{
This is the default implementation that works with
two vectors of classes "integer", "numeric" or "ts".
}
\section{Methods (by class)}{
\itemize{
\item \code{get.COR(default)}: Default implementation

}}
