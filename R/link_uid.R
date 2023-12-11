#' Link NCBI UID-s
#' 
#' Each entry in an NCBI database has its unique internal id. Entries in
#' different databases may be linked. For example, an assembly in the NCBI
#' Assembly database may be linked with metadata in the NCBI BioSample database.
#' This function links uids from one database with uids from another.
#' @param query character; a vector of uids.
#' @param from character; the database the queried uids come from.
#' \code{ncbi_dbs()} lists all available options.
#' @param to character; the database in which the function should look for links.
#' @param cache_file character; the name of the cache file without the file
#' extension. If \code{NULL}, results are not cached.
#' @param verbose logical; should verbos messages be printed to the console?
#' \code{ncbi_dbs()} lists all available options.
#' @return A tibble
#' @examples
#' \dontrun{
#' link_uid("4253631", "assembly", "biosample")
#' link_uid(c("1226742659", "1883410844"), "protein", "nuccore")
#' }
#' @export
link_uid <- function(query,
                    from,
                    to,
                    cache_file = NULL,
                    verbose = getOption("verbose")) {
  if (any(is.na(as.numeric(query)))) {
    stop("Query must be a valid UID. Valid UIDs can be converted to 'numeric'.")
  }
  from <- match.arg(from, ncbi_dbs())
  to <- match.arg(to, ncbi_dbs())
  foo <- function(x) {
    if (verbose) webseq_message("query", x, appendLF = FALSE)
    res <- NULL
    attempt <- 1
    while(is.null(res) && attempt <= 5) {
      res <- try(
        rentrez::entrez_link(
          id = x, dbfrom = from, db = to), silent = TRUE)
      if (inherits(res, "try-error")) {
        res <- NULL
        attempt <- attempt + 1
      }
    }
    if (length(res$links) == 0) {
      if (verbose) message("Not found. Returning NA.")
      tbl <- tibble::tibble(
        query = x,
        query_db = from,
        result = NA,
        result_db = to)
    } else {
      if (verbose) message("OK.")
      tbl <- tibble::tibble(
        query = x,
        query_db = from,
        result = res$links[[paste(from, to, sep = "_")]],
        result_db = to)
    }
    return(tbl)
  }
  if (is.null(cache_file)) {
    out <- lapply(query, foo)
  } else {
    if (!dir.exists("cache")) dir.create("cache")
    cfpath <- paste0("cache/", cache_file, ".rds")
    if (file.exists(cfpath)) {
      query_results <- readRDS(file = cfpath)
    } else {
      query_results <- list()
    }
    out <- lapply(query, function(x) {
      if (x %in% names(query_results)) {
        if (verbose) webseq_message("query", x, appendLF = FALSE)
        if (verbose) message("Already retrieved.")
        return(query_results[[as.character(x)]])
      } else {
        new <- foo(x)
        if (!is.na(x)) {
          query_results[[as.character(x)]] <<- new
          saveRDS(query_results, file = cfpath)
        }
        return(new)
      }
    })
  }
  out <- dplyr::bind_rows(out)
  return(out)
}
