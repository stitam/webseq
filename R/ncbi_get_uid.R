#' Get UID-s from NCBI databases
#'
#' This function replicates the NCBI website's search utility. It searches one
#' or more search terms in the chosen database and returns internal NCBI UID-s 
#' for the hits. These can be used e.g. to link NCBI entries with entries in 
#' other NCBI databases or to retrieve the data itself.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{ncbi_dbs()}.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param use_history logical; should the function use web history for faster
#' API queries? 
#' @param na_strings character; a vector of strings which should be interpreted
#' as `NA`.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return An object of class \code{"ncbi_uid"} which is a list with three
#' elements:
#' \itemize{
#'  \item \code{uid}: a vector of UIDs.
#'  \item \code{db}: the database used for the query.
#'  \item \code{web_history}: a tibble of web histories.
#'  }
#' @details The default value for \code{batch_size} should work in most cases.
#' However, if the search terms are very long, the function may fail with an
#' error message. In this case, try reducing the \code{batch_size} value.
#' @examples
#' ncbi_get_uid("GCA_003012895.2", db = "assembly")
#' ncbi_get_uid("Autographiviridae OR Podoviridae", db = "biosample")
#' ncbi_get_uid(c("WP_093980916.1", "WP_181249115.1"), db = "protein")
#' @export
ncbi_get_uid <- function(
    term,
    db,
    batch_size = 100,
    use_history = TRUE,
    na_strings = "NA",
    verbose = getOption("verbose")
    ) {
  db <- match.arg(db, choices = ncbi_dbs())
  index <- which(term %in% na_strings)
  if (length(index) > 0) {
    if (verbose) {
      terms_collapsed <- paste(term[index], collapse = ", ")
      msg <- paste0(
        "The following terms will be replaced with NAs: ",
        terms_collapsed,
        ". "
      )
      message(msg)
    }
    term[index] <- NA
  }
  if (any(is.na(term))){
    if (verbose) message("Removing NA-s from search terms. ", appendLF = FALSE)
    term <- term[which(!is.na(term))]
    if (verbose) message(paste0(length(term), " terms remain."))
  }
  if (length(term) == 0) {
    stop("No valid search terms.")
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
    hit <- wrap(
      "entrez_search",
      package = "rentrez",
      verbose = verbose,
      db = db,
      term = termlist[[x]],
      use_history = use_history
    )
    if (hit$count > hit$retmax) {
      hit <- wrap(
        "entrez_search",
        package = "rentrez",
        verbose = verbose,
        db = db,
        term = termlist[[x]],
        use_history = use_history,
        retmax = hit$count
      )
    }
    if (length(hit) == 1 && is.na(hit)) {
      return(list(
        uid = NA_real_,
        db = db,
        web_history = tibble::tibble()
      ))
    } else if (length(hit$ids) == 0) {
      if (verbose) message("Term not found. Returning NA.")
      return(list(
        uid = NA_real_,
        db = db,
        web_history = tibble::tibble()
      ))
    } else if (length(hit$ids) > 0) {
      return(list(
        uid = as.numeric(hit$ids),
        db = db,
        web_history = hit$web_history
      ))
    } else {
      if (verbose) message("Unknown exception.")
      return(list(
        uid = NA_real_,
        db = db,
        web_history = tibble::tibble()
      ))
    }
  }
  res <- lapply(seq_along(termlist), foo)
  uid <- lapply(res, function(x) x$uid)
  web_histories <- lapply(res, function(x) x$web_history)
  web_histories <- dplyr::bind_rows(web_histories)
  out <- list(
    uid = unlist(uid),
    db = db,
    web_history = web_histories
  )
  class(out) <- c("ncbi_uid", class(out))
  validate_webseq_class(out)
  return(out)
}
