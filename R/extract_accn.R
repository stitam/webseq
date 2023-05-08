#' Extract Accession Numbers from a parsed assembly report
#'
#' All assembly reports contain GenBank and/or RefSeq identifiers that uniquely
#' identify a contig. This function can be used to extract both GenBank and
#' RefSeq accessions a parsed assembly report.
#' @param report list; a parsed assembly report. use \code{parse_report()} to
#' parse assembly reports.
#' @return a data frame
#' @note This is the fifth step within the pipeline for downloading GenBank
#' files.
#' @seealso
#' \code{get_genomeid},
#' \code{get_report_url()},
#' \code{download_report()},
#' \code{parse_report()},
#' \code{download_gb()}
#' @examples
#' \dontrun{
#' phages <- get_genomeid("Autographiviridae", db = "assembly")
#' report_url <- get_report_url(phages$ids[1])
#' download_report(report_url)
#' filename <- dir(paste0(tempdir(), "/assembly_reports"))
#' filepath <- paste0(tempdir(), "/assembly_reports/", filename)
#' rpt <- parse_report(filepath)
#' extract_accn(rpt)
#' }
#' @export
extract_accn <- function(report) {
  if ("arpt" %in% class(report) == FALSE) {
    stop("Input must be a parsed assembly report.")
  }
  foo <- function(x) {
    accn_genbank <- data.frame(
      assembly_name = x$`Assembly name`,
      accession = x$`Assembly-Units`$`GenBank Unit Accession`,
      contig = x$`Assembly definition`$`GenBank-Accn`
    )
    accn_refseq <- data.frame(
      assembly_name = x$`Assembly name`,
      accession = x$`Assembly-Units`$`RefSeq Unit Accession`,
      contig = x$`Assembly definition`$`RefSeq-Accn`
    )
    accn <- dplyr::bind_rows(accn_genbank, accn_refseq)
    accn <- accn[stats::complete.cases(accn),]
    return(accn)
  }
  out <- try(foo(report), silent = TRUE)
  if (inherits(out, "try-error")){
    stop(paste0("Extraction failed: ", report$`Assembly name`))
  } else {
    return(out)
  }
}
