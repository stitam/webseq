#' Download GenBank files
#'
#' Download GenBank files from NCBI.
#' @param accns data.frame; a data frame that contains genome ids
#' (e.g. GCA_009859665.2) and unit (e.g. MN692672.2) accessions.
#' @param assembly_dir character; the path to the download directory.
#' @param update_threshold numeric; reports accessed earlier than this value
#' (days) will be downloaded again and the existing versions will be
#' overwritten. Set below \code{0} to force download all reports.
#' @param verbose logical; print messages to screen?
#' @return The function will download a GenBank file for each line of the
#' \code{accns} data frame.
#' @note This is the sixth and last step within the pipeline for downloading GenBank
#' files.
#' @note IMPORTANT! This function uses efetch in the background and requires
#' NCBI Entrez utilities to be available through the command line. On
#' Debian-like systems this requires the installation of the
#' \code{ncbi-entrez-direct} package. The package is available through the
#' package manager.
#' @seealso
#' \code{get_genomeid},
#' \code{get_report_url()},
#' \code{download_report()},
#' \code{parse_report()},
#' \code{extract_accn()},
#' \code{download_gb()}
#' @seealso
#' @examples
#' \dontrun{
#' phages <- get_uid("Autographiviridae", db = "assembly")
#' report_url <- get_report_url(phages$ids[1:3], cache = FALSE, verbose = TRUE)
#' download_report(report_url, verbose = TRUE)
#' filenames <- dir(paste0(tempdir(), "/assembly_reports"))
#' filepaths <- paste0(tempdir(), "/assembly_reports/", filenames)
#' rpts <- lapply(filepaths, parse_report())
#' accns <- lapply(rpts, extract_accn())
#' accns <- dplyr::bind_rows(accns)
#' download_gb(accns, verbose = TRUE)
#' }
#' @export
download_gb <- function(accns,
                        assembly_dir = NULL,
                        update_threshold = 30,
                        verbose = getOption("verbose")) {
  if (is.null(assembly_dir)) assembly_dir <-  paste0(tempdir(), "/genbank_files")
  if(!dir.exists(assembly_dir)) {
    if (verbose) {
      print("Download directory not found. Creating empty download directory.")
    }
    dir.create(assembly_dir, recursive = TRUE)
  }
  dlgb <- function(accession, contig, dir, verbose) {
    destfile <- paste0(assembly_dir, "/", accession, "__", contig, ".gb")
    cmd <- paste0(
      "efetch -db nuccore -id ",
      contig,
      " -format gb > ",
      destfile)
    if (!file.exists(destfile)) {
      if (verbose) print(paste0("Downloading ", accession, "__", contig, ".gb."))
      system(cmd)
    }
    if((file.info(destfile)$mtime - Sys.time()) > update_threshold) {
      if (verbose) print(paste0("Downloading ", accession, "__", contig, ".gb."))
      system(cmd)
    }
  }
  if (verbose) print("Downloading assemblies.")
  out <- mapply(function(x, y) {
    dlgb(accession = x, contig = y, dir = assembly_dir, verbose = verbose)
  }, accns$accession, accns$contig)
}
