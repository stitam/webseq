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
    type <- match.arg(type, choices = c("run", "fastq", "err"))
    if (is.null(dirpath)) dirpath <- paste0(getwd(), "/")
    if (!grepl("/$", dirpath)) dirpath <- paste0(dirpath, "/")
    if (!dir.exists(dirpath)) dir.create(dirpath, recursive = TRUE)
    foo <- function(x, type, verbose) {
      if (verbose) message(x, ". ", appendLF = FALSE)
      accession_prefix <- substr(x, 1, 6)    
      ftpdir <- paste0(
        "ftp://ftp.sra.ebi.ac.uk/vol1/", 
        type, "/",
        accession_prefix, "/",
        x, "/"
      )
      h <- curl::new_handle(dirlistonly=TRUE)
      con <- try(curl::curl(ftpdir, "r", h), silent = TRUE)
      if (inherits(con, "try-error")) {
        if (verbose) message("Failed. FTP path not found.")
        return(NA)
      }
      tbl <- read.table(con)
      close(con)
      ftpfiles <- paste0(ftpdir, tbl$V1)
      if (mirror) {
        dirpath <- gsub(
          "ftp://ftp.sra.ebi.ac.uk/vol1/",
          dirpath,
          ftpdir
        )
      }
      if (!dir.exists(dirpath)) dir.create(dirpath, recursive = TRUE)
      for (i in seq_along(ftpfiles)) {
        filename <- basename(ftpfiles[i])
        if (verbose) message(filename, ". ", appendLF = FALSE)
        localfile <- paste0(dirpath, filename)
        if (file.exists(localfile)) {
          if (verbose) {
            if (i < length(ftpfiles)) {
              message("Done. Already downloaded. ", appendLF = FALSE)
            } else {
              message("Done. Already downloaded.")
            }
          }
          next()
        }
        webseq_sleep(type = "FTP")
        out <- try(utils::download.file(
          ftpfiles[i], destfile = localfile, quiet = TRUE), silent = TRUE)
        if (inherits(out, "try-error")) {
          if (verbose) {
            if (i < length(ftpfiles)) {
              message("Failed. Webservice temporarily down. ", appendLF = FALSE)
            } else {
              message("Failed. Webservice temporarily down.")
            }
          }
          try(file.remove(localfile), silent = TRUE)
          next()
        }
        if (verbose) {
          if (i < length(ftpfiles)) {
            message("Done. ", appendLF = FALSE)
          } else {
            message("Done.")
          }
        }
      }
    }
    out <- lapply(accession, function(x) foo(x, type = type, verbose = verbose))
}
