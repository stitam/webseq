#' Download sequencing reads from the European Nucleotide Archive (ENA)
#' 
#' @param accession character; a character vector of ENA Accession IDs.
#' @param type character; A character string specifying the type of file to
#' download, either \code{"run"}, \code{"fastq"} or \cpde{"err"}. See Details
#' for more  information.
#' @param dirpath character; the path to the directory where the file should be
#' downloaded. If \code{NULL}, download file to the working directory.
#' @param mirror logical; should the download directory mirror the structure of 
#' the FTP directory?
#' @param verbose logical; should verbose messages be printed to console?
#' @details 
#' If \code{type == "run"} the function will download the read files that were 
#' originally submitted to ENA. If a run was originally submitted to another 
#' INSDC database (NCBI SRA, DDBJ) then a file of this category will not be 
#' available. If \code{type == "fastq"} the function will download one or more 
#' FASTQ files for each run. If \code{type = "err"} the function will download
#' files that can be used with NCBI's SRA Toolkit. 
#' @examples
#' \dontrun{
#' ena_download_reads("ERR164407", type = "fastq", verbose = TRUE)
#' }
#' 
#' @references 
#' https://ena-docs.readthedocs.io/en/latest/retrieval/file-download/sra-ftp-structure.html
#' @export
ena_download_reads <- function(
    accession, 
    type = "fastq",
    dirpath = NULL,
    mirror = TRUE,
    verbose = getOption("verbose")
    ) {
    file_type <- match.arg(file_type, choices = c("run", "fastq", "err"))
    foo <- function(x, type, verbose) {
      if (verbose) message(x, ". ", appendLF = FALSE)
      accession_prefix <- substr(accession, 1, 6)    
      ftpdir <- paste0(
        "ftp://ftp.sra.ebi.ac.uk/vol1/", 
        type, "/",
        accession_prefix, "/",
        accession, "/"
      )
      h <- curl::new_handle(dirlistonly=TRUE)
      con <- try(curl::curl(ftpdir, "r", h), silent = TRUE)
      if (inherits(con, "try-error")) {
        if (verbose) message("Failed. FTP path not found.")
        return(NA)
      }
      tbl <- read.table(con, stringsAsFactors=FALSE, fill=TRUE)
      close(con)
      ftpfiles <- paste0(ftpdir, tbl$V1)
      if (is.null(dirpath)) dirpath <- paste0(getwd(), "/")
      if (mirror) {
        dirpath <- gsub(
          "ftp://ftp.sra.ebi.ac.uk/vol1/",
          dirpath,
          ftpdir
        )
      }
      if (!dir.exists(dirpath)) dir.create(dirpath, recursive = TRUE)
      for (i in ftpfiles) {
        localfile <- paste0(dirpath, basename(i))
        if (file.exists(localfile)) {
          if (verbose) message("Done. Already downloaded.")
          return(NA)
        }
        webseq_sleep(type = "FTP")
        out <- try(utils::download.file(
          i, destfile = localfile, quiet = TRUE), silent = TRUE)
        if (inherits(out, "try-error")) {
          if (verbose) message("Failed. Webservice temporarily down.")
          file.remove(localfile)
          return(NA)
        }
        message("Done.")
      }
    }
    out <- lapply(accession, function(x) foo(x, type = type, verbose = verbose))
}
