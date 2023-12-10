#' Get UID-s from NCBI databases
#'
#' This function replicates the NCBI website's search utility. It searches one
#' or more search terms in the chosen database and returns internal NCBI UID-s 
#' for individual hits. These can be used e.g. to link NCBI entries with entries
#' in other NCBI databases or to retrieve the data itself.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{ncbi_supported_dbs()}
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param use_history logical; should the function use web history for faster
#' API queries? 
#' @param verbose logical; should verbose messages be printed to the console?
#' @return The function returns a list with two elements:
#' \itemize{
#'  \item \code{uids}: a tibble with the search terms and their corresponding
#'  UIDs.
#'  \item \code{history}: if \code{web_history = TRUE}, a character vector with
#'  the web history IDs of the individual batches, otherwise \code{NULL}.
#'  }
#' @details The default value for \code{batch_size} should work in most cases.
#' However, if the search terms are very long, the function may fail with an
#' error message. In this case, try reducing the \code{batch_size} value.
#' @examples
#' \dontrun{
#' ncbi_get_uid("GCA_003012895.2", db = "assembly")
#' ncbi_get_uid("Autographiviridae OR Podoviridae", db = "biosample")
#' ncbi_get_uid(c("WP_093980916.1", "WP_181249115.1"), db = "protein")
#' }
ncbi_get_uid <- function(
    term,
    db,
    batch_size = 100,
    use_history = TRUE,
    verbose = getOption("verbose")
    ) {
  db <- match.arg(db, choices = ncbi_supported_dbs())
  if (all(is.na(term))) {
    stop("No valid search terms.")
  } else if (any(is.na(term))){
    if (verbose) message("Removing NA-s from search terms.")
    term <- term[which(!is.na(term))]
  }
  termlist <- list()
  if (length(term) > batch_size) {
    nbatch <- ceiling(length(term)/batch_size)
    if (verbose) message("Splitting search terms into ", nbatch, " batches.")
    for (i in 1:nbatch) {
      termlist[[i]] <- term[((i-1)*batch_size + 1):min(i*batch_size, length(term))]
    }
  } else {
    termlist[[1]] <- term
  }
  termlist <- lapply(termlist, function(x) {
    newterm <- paste(x, collapse = " OR ") |> trimws()
    return(newterm)
  })
  foo <- function(x) {
    if (verbose) message("Querying UIDs for batch ", x, ". ", appendLF = FALSE)
    r <- NULL
    attempt <- 1
    while(is.null(r) && attempt <= 5) {
      webseq_sleep(type = "API")
      if (use_history) {
        hit <- try(
          rentrez::entrez_search(db, term = termlist[[x]], use_history = TRUE),
          silent = TRUE
        )
      } else {
        hit <- try(
          rentrez::entrez_search(db, term = termlist[[x]], use_history = FALSE),
          silent = TRUE
        )
      }
      if (inherits(hit, "try-error")) {
        attempt <- attempt + 1
      } else r <- 1
    }
    if (inherits(hit, "try-error")) {
      if (verbose) webseq_message("service_down")
      return(list(
        uids = tibble::tibble(term = termlist[[x]], db = db, uid = NA_integer_),
        web_history = NULL
      ))
    }
    if (hit$count > hit$retmax) {
      r <- NULL
      attempt <- 1
      while(is.null(r) && attempt <= 5) {
        webseq_sleep(type = "API")
        if (use_history) {
          hit <- try(
            rentrez::entrez_search(
              db, term = termlist[[x]], use_history = TRUE, retmax = hit$count),
            silent = TRUE
          )
        } else {
          hit <- try(
            rentrez::entrez_search(
              db, term = termlist[[x]], use_history = FALSE, retmax = hit$count),
            silent = TRUE
          )
        }
        if (inherits(hit, "try-error")) {
          attempt <- attempt + 1
        } else r <- 1
      }
    }
    if (inherits(hit, "try-error")) {
      if (verbose) webseq_message("service_down")
      return(list(
        uids = tibble::tibble(term = termlist[[x]], db = db, uid = NA_integer_),
        web_history = NULL
      ))
    }
    if (length(hit$ids) > 0) {
      if (verbose) message("OK.")
      return(list(
        uids = tibble::tibble(term = termlist[[x]], db = db, uid = as.integer(hit$ids)),
        web_history = hit$web_history
      ))
    } else {
      if (verbose) message("Term not found. Returning NA.")
      return(list(
        uids = tibble::tibble(term = termlist[[x]], db = db, uid = NA_integer_),
        web_history = NULL
      ))
    }
  }
  res <- lapply(seq_along(termlist), foo)
  uids <- lapply(res, function(x) x$uids) |> dplyr::bind_rows()
  webenvs <- unlist(lapply(res, function(x) {
    if ("web_history" %in% names(x)) {
      return(x$web_history$WebEnv)
    } else {
      return(NULL)
    }
  }))
  out <- list(
    uids = uids,
    web_history = webenvs
  )
  return(out)
}
