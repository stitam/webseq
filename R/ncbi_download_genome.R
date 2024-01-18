#' Download Genomes from NCBI Assembly Database
#'
#' This function directly downloads genome data through the NCBI FTP server.
#' @param query either an object of class \code{ncbi_uid} or an integer vector 
#' of NCBI Assembly UIDs. See Details for more information.
#' @param type character; the file extension to download. Valid options are
#' \code{"assembly_report"}, \code{"assembly_stats"}, \code{"cds"},
#' \code{"feature_count"}, \code{"feature_table"}, \code{"genomic.fna"},
#' \code{"genomic.gbff"}, \code{"genomic.gff"}, \code{"genomic.gtf"},
#' \code{"protein.faa"}, \code{"protein.gpff"}, \code{"translated_cds"}.
#' @param dirpath character; the path to the directory where the file should be
#' downloaded. If \code{NULL}, download file to the working directory.
#' @param verbose logical; should verbose messages be printed to console?
#' @details Some functions in webseq, e.g. \code{ncbi_get_uid()} or
#' \code{ncbi_link_uid()} return objects of class \code{"ncbi_uid"}. These
#' objects may be used directly as query input for
#' \code{ncbi_download_genome()}. It is recommended to use this approach because
#' then the function will check whether the query really contains UIDs from the
#' NCBI Assembly database and fail if not. Alternatively, you can also use a
#' character vector of UIDs as query input but in this case there will be no
#' consistency checks and the function will just attempt to interpret them as
#' NCBI Assembly UIDs.
#' @examples
#' \dontrun{
#' # Download genbank file for GCF_003007635.1.
#' # The function will access files within this directory:
#' # ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/003/007/635/
#' 
#' uid <- ncbi_get_uid("GCF_003007635.1", db = "assembly")
#' ncbi_download_genome(uid, type = "genomic.gbff", verbose = TRUE)
#' 
#' # Download multiple files
#' data(examples) 
#' uids <- ncbi_get_uid(examples$assembly, db = "assembly")
#' ncbi_download_genome(uids, type = "genomic.gff", verbose = TRUE)
#' }
#' @export
ncbi_download_genome <- function(query,
                                 type = "genomic.gbff",
                                 dirpath = NULL,
                                 verbose = getOption("verbose")) {
  type <- match.arg(type, c(
    "assembly_report", "assembly_stats", "cds", "feature_count",
    "feature_table", "genomic.fna", "genomic.gbff", "genomic.gff",
    "genomic.gtf", "protein.faa", "protein.gpff", "translated_cds"))
  if ("ncbi_uid" %in% class(query)) {
    if (query$db == "assembly") {
      assembly_uid <- query$uid
    } else {
      stop("Query must contain NCBI Assembly UIDs.")
    }
  } else {
    assembly_uid <- query
  }
  assembly_uid <- unlist(get_idlist(
    assembly_uid, 
    batch_size = length(assembly_uid),
    verbose = verbose
  ))
  foo <- function(x, type, verbose) {
    if (verbose) message(x, ". ", appendLF = FALSE)
    # TODO update ncbi_parse_assembly_xml and then replace ncbi_meta_assembly()
    # plus response handling with ncbi_get_meta() 
    assembly_meta <- try(ncbi_meta_assembly(x), silent = TRUE)
    if (inherits(assembly_meta, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      return(NA)
    }
    if (nrow(assembly_meta) != 1) {
      if (verbose) message("Failed. Assembly metadata not appropriate.")
      return(NA)
    }
    if (assembly_meta$assembly_uid != x) {
      msg <- paste0(
        "Warning! Query: ", x,
        ". Found: ", assembly_meta$assembly,
        ". Skipping."
      )
      warning(msg)
      return()
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
    webseq_sleep(type = "FTP")
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
  out <- lapply(assembly_uid, function(x) foo(x, type = type, verbose = verbose))
}
