#' Download Genomes from NCBI
#'
#' This function directly accesses a genome through the NCBI FTP server.
#' @param assembly_uid numeric; a unique identifier. Use \code{get_uid} to
#' acquire a unique UID.
#' @param type character; the file extension to download. Valid options are
#' \code{"assembly_report"}, \code{"assembly_stats"}, \code{"cds"},
#' \code{"feature_count"}, \code{"feature_table"}, \code{"genomic.fna"},
#' \code{"genomic.gbff"}, \code{"genomic.gff"}, \code{"genomic.gtf"},
#' \code{"protein.faa"}, \code{"protein.gpff"}, \code{"translated_cds"}.
#' @param dirpath character; the path to the directory where the file should be
#' downloaded.
#' @param from character; either \code{"refseq"} or \code{"genbank"}.
#' If \code{from} is set to \code{"refseq"} then the function will attempt to
#' download the file from the refseq repository and use \code{"genbank"} as a
#' fallback option.
#' @param verbose boolean; should verbose messages be printed to console?
#' @note ToDo: vectorise and add input validation to ensure function fails early
#' if there are input errors. Add tests.
#' @examples
#' \dontrun {
#' # Aim: download genbank file for "GCF_003007635.1"
#'
#' # The function will access files within this directory:
#' # ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/003/007/635/
#'
#' # Step 1 - Get NCBI Assembly UID
#' uid <- get_uid("GCF_003007635.1", db = "assembly")
#'
#' # Step 2 - Download file
#' download_genome(uid, type = "genomic.gbff")
#' }

download_genome <- function(assembly_uid,
                            type = "genomic.gbff",
                            dirpath = NULL,
                            from = "refseq",
                            verbose = getOption("verbose")) {
  assembly_uid <- as.numeric(assembly_uid)
  if (is.na(assembly_uid)) {
    stop("Query is not an NCBI Assembly UID.")
  }
  type <- match.arg(type, c(
    "assembly_report", "assembly_stats", "cds", "feature_count",
    "feature_table", "genomic.fna", "genomic.gbff", "genomic.gff",
    "genomic.gtf", "protein.faa", "protein.gpff", "translated_cds"))
  from <- match.arg(from, c("refseq", "genbank"))
  data <- rentrez::entrez_fetch(
    "assembly", id = assembly_uid, rettype = "docsum", retmode = "json")
  data <- jsonlite::fromJSON(data)
  if (from == "refseq") {
    ftppath <- paste0(data$result[[2]]$ftppath_refseq)
    if (is.null(ftppath) | ftppath == "") {
      if (verbose) message("Not found in RefSeq. Trying GenBank.")
      ftppath <- paste0(data$result[[2]]$ftppath_genbank)
    }
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
  if (is.null(dirpath)) {
    dirpath = tempdir()
  }
  if (!dir.exists(dirpath)) dir.create(dirpath, recursive = TRUE)
  filepath <- paste0(dirpath, "/", basename(urlpath))
  if (file.exists(filepath)) {
    message("Already downloaded: ", basename(urlpath))
  } else {
    out <- try(download.file(urlpath, destfile = filepath), silent = TRUE)
    if (inherits(out, "try-error")) {
      message("Download failed: ", urlpath)
      file.remove(filepath)
    }
  }
}
