#' Download assembly reports
#'
#' This function takes a vector of assembly report urls, checks for their
#' presence in the assembly report directory and downloads them if they are
#' either missing from the directory or might be outdated.
#' @param urls character; a vector of assembly report urls.
#' @param report_dir character; download directory for the assembly reports.
#' @param update_threshold numeric; reports accessed earlier than this value
#' (days) will be downloaded again and the existing versions will be
#' overwritten. Set below \code{0} to force download all reports.
#' @param verbose logical; print messages to screen?
#' @note This is the third step within the pipeline for downloading GenBank
#' files.
#' @seealso
#' \code{get_genomeid},
#' \code{get_report_url()},
#' \code{parse_report()},
#' \code{extract_accn()},
#' \code{download_gb()}
#' @examples
#' \dontrun{
#' phages <- get_genomeid("Autographiviridae", db = "assembly")
#' report_url <- get_report_url(phages$ids[1], cache = FALSE, verbose = TRUE)
#' download_report(report_url, verbose = TRUE)
#' }
#' @export
download_report <- function(urls,
                            report_dir = NULL,
                            update_threshold = 30,
                            verbose = getOption("verbose")){
  # todo: verbose messages - do not say 'Downloadinf' when it is not.
  if (is.null(report_dir)) report_dir = paste0(tempdir(), "/assembly_reports")
  if (!dir.exists(report_dir)){
    if (verbose) {
      print("Download directory not found. Creating empty download directory.")
    }
    dir.create(report_dir, recursive = TRUE)
  }
  foo <- function(x) {
    destfile <- paste0(
      report_dir, "/",
      stringi::stri_extract(x, regex = "GC[A|F]_[0-9]+\\.[0-9]+_[A-Za-z0-9]+"),
      ".txt")
    if(!file.exists(destfile)) {
      if (verbose) print(paste0("Downloading ", basename(destfile)))
      download.file(url = x, destfile = destfile)
    }
    if((file.info(destfile)$mtime -Sys.time()) > update_threshold) {
      if (verbose) print(paste0("Downloading ", basename(destfile)))
      download.file(url = x, destfile = destfile)
    }
  }
  print("Downloading assembly reports.")
  out <- lapply(urls, foo)
}
