% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/uptimerobot.fields.R
\name{uptimerobot.fields}
\alias{uptimerobot.fields}
\title{Get a list of the available fields for various endpoints}
\usage{
uptimerobot.fields(type)
}
\arguments{
\item{type}{string with the type of fields to be reported. Only \code{monitor} and \code{contact} are currently supported.}
}
\value{
The function returns a list of 3 elements which in turn contains a vector of available fields for a given \code{set} each.
The returned elements are:
\enumerate{
  \item \code{typical} returns a typical set of fields, used in most situations;
  \item \code{full} returns the full set of available fields, including passwords and other potentially confidential data;
  \item \code{compact} return a minimal set of fields.
}
}
\description{
\code{uptimerobots.fields} returns a list of vectors of available fields for commodity uses.
Use it to avoid manually typing long list of fields in vectors or comma-delimited strings when used in various endpoints.
}
\details{
Use the \code{type} parameter to choose which set of fields to return in a list of vectors. These endpoints are currently supported:
\code{monitor} and \code{contact}.
}
\author{
Gabriele Baldassarre
}
\seealso{
\code{\link{uptimerobot.monitors}}, \code{\link{uptimerobot.contacts}}
}

