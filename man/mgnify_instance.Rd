% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mgnify_instance.R
\name{mgnify_instance}
\alias{mgnify_instance}
\title{Search for a specific entry in MGnify}
\usage{
mgnify_instance(query, from)
}
\arguments{
\item{query}{character; the indentifier}

\item{from}{character; the api which contains this identifier. See
\code{mgnify_endpoints()} for a list of possible apis.}
}
\value{
a list
}
\description{
This function can be used for searching MGnify using an identifier.
}
\examples{
\dontrun{
# look up an assembly
mgnify_instance("ERZ477576", from = "assemblies")
}
}
