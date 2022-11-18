#'Retrieve sequences from ENA
#'
#'@param accessions character; Accessions to query.
#'@param mode character; Can be either \code{"embl"}, \code{"fasta"}, or
#' \code{"xml"}.
#'@param expanded logical; Get expanded records for CON sequences.
#'@param annotation_only logical; Only retrieve annotation, no sequence.
#'@param line_limit integer; Limit the number of text lines returned.
#'@param download logical; Download the result as a file.
#'@param destfile_by character; Number of files to download. 
#'\code{"batch"}: one file for each batch, \code{"all"}: one file altogether.
#'@param gzip logical; Download the result as a gzip file.
#'@param set logical; ???
#'@param range character; ???
#'@param complement logical; ???
#'@param batch_size integer; Number of accessions to query in a single request.
#'Using this value, accessions will be broken down into one or more batches.
#'If set to 0, all accessions will be queried in a single request. 
#'@param verbose logical; Should verbose messages be printed to the console?
#'@examples
#'\dontrun {
#'ena_query("LC136852")
#'ena_query(c("LC136852", "LC136853"))
#'}
#'@importFrom curl new_handle handle_setopt curl_download
#'@export
ena_query <- function(
    accessions,
    mode = "fasta",
    expanded = FALSE,
    annotation_only = FALSE,
    line_limit = 0,
    download = FALSE,
    destfile_by = "all",
    gzip = FALSE,
    set = FALSE,
    range = NULL,
    complement = FALSE,
    batch_size = 0,
    verbose = getOption("verbose")
) {
  stopifnot(length(expanded) == 1 && is.logical(expanded))
  stopifnot(length(annotation_only) == 1 && is.logical(annotation_only))
  #stopifnot(length(line_limit) == 1 && is.integer(line_limit) && line_limit >= 0)
  stopifnot(length(download) == 1 && is.logical(download))
  stopifnot(length(gzip) == 1 && is.logical(gzip))
  stopifnot(length(set) == 1 && is.logical(set))
  #stopifnot(length(range) == 1 && is.character(range))
  stopifnot(length(complement) == 1 && is.logical(complement))
  stopifnot(length(verbose) == 1 && is.logical(verbose))
  expanded <- tolower(as.character(expanded))
  annotation_only <- tolower(as.character(annotation_only))
  download <- tolower(as.character(download))
  gzip <- tolower(as.character(gzip))
  set <- tolower(as.character(set))
  complement <- tolower(as.character(complement))
  stopifnot(length(batch_size) == 1 && is.numeric(batch_size))
  if (length(accessions) > 10000) {
    if (batch_size == 0 | batch_size > 10000) {
      msg <- paste0(
        "Maximum 10000 accessions per request. ",
        "Adjust 'batch_size' to run your query in multiple batches."
      )
      stop(msg)
    }
  }
  foo <- function(x, destfile_by, i) {
    if (length(x) > 1) {
      x <- paste(x, collapse = ",")
      if (destfile_by == "batch") {
        destfile <- paste0(
          "ena_query_", i, ".", mode, ifelse(gzip == "true", ".gz", ""))
      } else {
        destfile <- paste0(
          "ena_query.", mode, ifelse(gzip == "true", ".gz", ""))
      }
    } else {
      destfile <- paste0(x, ".", mode, ifelse(gzip == "true", ".gz", ""))
    }
    url <- paste0(
      "https://www.ebi.ac.uk/ena/browser/api/", mode, "?",
      "accessions=", x,
      "&expanded=", expanded,
      "&annotationOnly=", annotation_only,
      "&download=", download,
      "&gzip=", gzip,
      "&set=", set,
      "&complement=", complement
    )
    if (download == "true") {
      # TODO: finalise dev code
      # variations based on number of accessions, batch_size, download_by, gzip
      mode <- ifelse(destfile_by == "all" & gzip == FALSE, "a", "wb")
      handle <- curl::new_handle()
      curl::handle_setopt(handle, customrequest = "POST")
      curl::curl_download(
        url = url,
        destfile = destfile,
        handle = handle,
        mode = mode
      )
    } else {
      res <- try_url("POST", url)
      seqs <- ena_parse(res, mode = mode)
      return(seqs)
    }
  }
  if (batch_size == 0) {
    seqs <- foo(accessions, destfile_by, i = 0)
  } else {
    seqs <- vector()
    nbatch <- ceiling(length(accessions)/batch_size)
    for (i in 1:nbatch) {
      if(i < nbatch) {
        accns <- accessions[(batch_size*(i-1)+1):(batch_size*i)]
      } else {
        accns <- accessions[(batch_size*(i-1)+1):length(accessions)]
      }
      seqs <- c(seqs, foo(accns, destfile_by, i))
    }
  }
  return(seqs)
}

#' Parse API responses from ENA
#' @param mode character; \code{embl}, \code{fasta}, or \code{xml}
#' @importFrom httr content
#' @importFrom stringr str_locate
#' @noRd
ena_parse <- function(response, mode) {
  if (mode == "embl"){
    out <- as.list(response)
  }
  if (mode == "fasta") {
    cont <- httr::content(response, type = "text", encoding = "UTF-8")
    cont <- strsplit(cont, ">")[[1]][-1]
    breaks <- unname(stringr::str_locate(cont, "\n")[,1])
    out <- trimws(substr(cont, start = breaks, stop = nchar(cont)))
    out <- gsub("\\\n", "", out)
    out <- as.list(out)
    names(out) <- trimws(substr(cont, start = 1, stop = breaks))
  }
  if (mode == "xml") {
    cont <- httr::content(response, type = "text/xml")
    out <- as.list(cont)
  }
  return(out)
}
