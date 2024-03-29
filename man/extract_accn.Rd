% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_accn.R
\name{extract_accn}
\alias{extract_accn}
\title{Extract Accession Numbers from a parsed assembly report}
\usage{
extract_accn(report)
}
\arguments{
\item{report}{list; a parsed assembly report. use \code{parse_report()} to
parse assembly reports.}
}
\value{
a data frame
}
\description{
All assembly reports contain GenBank and/or RefSeq identifiers that uniquely
identify a contig. This function can be used to extract both GenBank and
RefSeq accessions a parsed assembly report.
}
\note{
This is the fifth step within the pipeline for downloading GenBank
files.
}
\examples{
\dontrun{
phages <- get_genomeid("Autographiviridae", db = "assembly")
report_url <- get_report_url(phages$ids[1])
download_report(report_url)
filename <- dir(paste0(tempdir(), "/assembly_reports"))
filepath <- paste0(tempdir(), "/assembly_reports/", filename)
rpt <- parse_report(filepath)
extract_accn(rpt)
}
}
\seealso{
\code{get_genomeid},
\code{get_report_url()},
\code{download_report()},
\code{parse_report()},
\code{download_gb()}
}
