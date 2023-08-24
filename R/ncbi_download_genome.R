#' Download Genomes from NCBI
#'
#' This function directly downloads genome data through the NCBI FTP server.
#' @param accession character; a character vector of assembly accessions.
#' @param type character; the file extension to download. Valid options are
#' \code{"assembly_report"}, \code{"assembly_stats"}, \code{"cds"},
#' \code{"feature_count"}, \code{"feature_table"}, \code{"genomic.fna"},
#' \code{"genomic.gbff"}, \code{"genomic.gff"}, \code{"genomic.gtf"},
#' \code{"protein.faa"}, \code{"protein.gpff"}, \code{"translated_cds"}.
#' @param dirpath character; the path to the directory where the file should be
#' downloaded. If \code{NULL}, download file to the working directory.
#' @param verbose logical; should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' # Download genbank file for GCF_003007635.1.
#' # The function will access files within this directory:
#' # ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/003/007/635/
#' ncbi_download_genome("GCF_003007635.1", type = "genomic.gbff", verbose = TRUE)
#' 
#' # Download multiple files
#' accessions <- c("GCF_000248195.1", "GCF_000695855.3")
#' ncbi_download_genome(accessions, type = "genomic.gbff", verbose = TRUE)
#' }
#' @export
ncbi_download_genome <- function(accession,
                                 type = "genomic.gbff",
                                 dirpath = NULL,
                                 verbose = getOption("verbose")) {
  type <- match.arg(type, c(
    "assembly_report", "assembly_stats", "cds", "feature_count",
    "feature_table", "genomic.fna", "genomic.gbff", "genomic.gff",
    "genomic.gtf", "protein.faa", "protein.gpff", "translated_cds"))
  foo <- function(x, type, verbose) {
    if (grepl("^GC[A|F]_[0-9]+\\.[0-9]+", x) == FALSE) {
      if (verbose) {
        message(x, ". Failed. Not an assembly accession.")
      }
      return(NA)
    }
    if (verbose) message(x, ". ", appendLF = FALSE)
    Sys.sleep(stats::runif(1,0.2,0.5))
    assembly_uid <- try(get_uid(x, db = "assembly"), silent = TRUE)
    if (inherits(assembly_uid, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      return(NA)
    }
    if (length(assembly_uid) == 1 && is.na(assembly_uid)) {
      if (verbose) message("Failed. Assembly accession not found.")
      return(NA)
    }
    if (length(assembly_uid$uid) > 1) {
      if (verbose) {
        message("Failed. Multiple assembly uid-s found: ",
                paste(assembly_uid, collapse = ", "), ".") 
      }
      return(NA)
    }
    Sys.sleep(stats::runif(1,0.2,0.5))
    assembly_meta <- try(ncbi_meta_assembly(assembly_uid$uid), silent = TRUE)
    if (inherits(assembly_meta, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      return(NA)
    }
    if (nrow(assembly_meta) != 1) {
      if (verbose) message("Failed. Assembly metadata not appropriate.")
      return(NA)
    }
    if (assembly_meta$assembly != x) {
      msg <- paste0(
        "Warning! Query: ", x, ". Found: ", assembly_meta$assembly, ". ")
      if (verbose) message(msg, appendLF = FALSE) else warning(msg)
    }
    ftppath <- assembly_meta$ftppath
    if (is.na(assembly_meta$ftppath)) {
      if (verbose) message("Failed. FTP path not found.")
      return(NA)
    }
    prefix <- strsplit(ftppath, "/")[[1]]
    prefix <- prefix[length(prefix)]
    suffix <- switch(type,
                     assembly_report = ".txt",
                     assembly_stats = ".txt",
                     cds.fna = "_from_genomic.fna.gz",
                     feature_count = ".txt.gz",
                     feature_table = ".txt.gz",
                     genomic.fna = ".gz",
                     genomic.gbff = ".gz",
                     genomic.gff = ".gz",
                     genomic.gtf = ".gz",
                     protein.faa = ".gz",
                     protein.gpff = ".gz",
                     translated_cds = ".faa.gz")
    urlpath <- paste0(ftppath, "/", prefix, "_" ,type, suffix)
    if (is.null(dirpath)) dirpath = getwd()
    if (!dir.exists(dirpath)) dir.create(dirpath, recursive = TRUE)
    filepath <- paste0(dirpath, "/", basename(urlpath))
    if (file.exists(filepath)) {
      if (verbose) message("Done. Already downloaded.")
      return(NA)
    }
    Sys.sleep(stats::runif(1,0.2,0.5))
    out <- try(utils::download.file(urlpath,
                                    destfile = filepath,
                                    quiet = TRUE), silent = TRUE)
    if (inherits(out, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      file.remove(filepath)
      return(NA)
    }
    message("Done.")
  }
  out <- lapply(accession, function(x) foo(x, typ = type, verbose = verbose))
}
