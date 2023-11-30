#' Collect UID-s from NCBI databases
#'
#' This function replicates the NCBI website's search utility. One or more
#' search terms are matched against the chosen database and the function returns
#' a tibble of internal NCBI UID-s that can be used e.g. to link NCBI entries
#' with entries in other NCBI databases.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}
#' @param cache_file character; the name of the cache file without the file
#' extension. If \code{NULL}, results are not cached.
#' @param verbose logical; should verbos messages be printed to the console?
#' @return A tibble.
#' @examples
#' \dontrun{
#' get_uid("GCA_003012895.2")
#' get_uid("Autographiviridae OR Podoviridae", db = "assembly")
#' get_uid(c("WP_093980916.1", "WP_181249115.1"), db = "protein")
#' }
#' @export
get_uid <- function(term,
                    db = "assembly",
                    cache_file = NULL,
                    verbose = getOption("verbose")) {
  db <- match.arg(db, rentrez::entrez_dbs())
  foo <- function(x) {
    if (is.na(x)) {
      if (verbose) webseq_message("na")
      return(tibble::tibble(term = x, db = db, uid = NA_integer_))
    }
    if (verbose) webseq_message("query", x, appendLF = FALSE)
    r <- NULL
    attempt <- 1
    while(is.null(r) && attempt <= 5) {
      hit <- try(rentrez::entrez_search(db, term = x), silent = TRUE)
      if (inherits(hit, "try-error")) {
        attempt <- attempt + 1
      } else r <- 1
    }
    if (inherits(hit, "try-error")) {
      if (verbose) webseq_message("service_down")
      return(tibble::tibble(term = x, db = db, uid = NA_integer_))
    }
    if (hit$count > hit$retmax) {
      r <- NULL
      attempt <- 1
      while(is.null(r) && attempt <= 5) {
        hit <- try(rentrez::entrez_search(db, term = x, retmax = hit$count),
                   silent = TRUE)
        if (inherits(hit, "try-error")) {
          attempt <- attempt + 1
        } else r <- 1
      }
    }
    if (inherits(hit, "try-error")) {
      if (verbose) webseq_message("service_down")
      return(tibble::tibble(term = x, db = db, uid = NA_integer_))
    }
    if (length(hit$ids) > 0) {
      if (verbose) message("OK.")
      return(tibble::tibble(term = x, db = db, uid = as.integer(hit$ids)))
    } else {
      if (verbose) message("Not found. Returning NA.")
      return(tibble::tibble(term = x, db = db, uid = NA_integer_))
    }
  }
  if (is.null(cache_file)) {
    res <- lapply(term, foo)
  } else {
    if (!dir.exists("cache")) dir.create("cache")
    cfpath <- paste0("cache/", cache_file, ".rds")
    if (file.exists(cfpath)) {
      query_results <- readRDS(file = cfpath)
    } else {
      query_results <- list()
    }
    res <- lapply(term, function(x) {
      if (x %in% names(query_results)) {
        if (verbose) webseq_message("query", x, appendLF = FALSE)
        if (verbose) message("Already retrieved.")
        return(query_results[[x]])
      } else {
        new <- foo(x)
        if (!is.na(x)) {
          query_results[[x]] <<- new
          saveRDS(query_results, file = cfpath)
        }
        return(new)
      }
    })
  }
  res <- dplyr::bind_rows(res)
  return(res)
}
