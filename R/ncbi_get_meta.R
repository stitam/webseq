#' Get sequence metadata from NCBI
#' 
#' This function retrieves metadata from a given NCBI sequence database.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{ncbi_dbs()}. Not all databases are supported.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param use_history logical; should the function use web history for faster
#' API queries?
#' @param parse logical; Should the function attempt to parse the output into a
#' tibble? If unsuccessful, the function will return the unparsed output.
#' @param verbose logical; Should verbose messages be printed to console?
#' @return The function returns a list with two elements:
#' \itemize{
#' \item \code{meta}: if \code{parse = TRUE} then either a tibble with the 
#' metadata or if parsing is unsuccessful, the unparsed metadata. If
#' \code{parse = FALSE} the unparsed metadata.
#' \item \code{history}: a tibble of web histories.
#' }
#' @examples
#' \dontrun{
#' data(examples)
#' meta <- ncbi_get_meta(examples$biosample, db = "biosample")
#' }
#' @export
ncbi_get_meta <- function(
    term,
    db,
    batch_size = 100,
    use_history = TRUE,
    parse = TRUE,
    verbose = getOption("verbose")
  ) {
  db <- match.arg(db, choices = ncbi_dbs())
  uid <- ncbi_get_uid(
    term = term,
    db = db,
    batch_size = batch_size,
    use_history = use_history,
    verbose = verbose
  )
  rettype <- switch(
    db,
    assembly = "docsum",
    bioproject = "xml",
    biosample = "full",
    gene = "null",
    nuccore = "native",
    protein = "native",
    sra = "full"
  )
  retmode <- "xml"
  if (use_history) {
    res <- lapply(1:nrow(uid$web_history), function(x) {
      WH <- list(
      "WebEnv" = uid$web_history$WebEnv[x],
      "QueryKey" = uid$web_history$QueryKey[x]
      )
      wrap(
        "entrez_fetch",
        package = "rentrez",
        db = db,
        web_history = WH,
        rettype = rettype,
        retmode = retmode,
        verbose = verbose
      ) 
    })
  } else {
    idlist <- get_idlist(uid$uid, batch_size, verbose)
    res <- lapply(idlist, function(x) {
      wrap(
        "entrez_fetch",
        package = "rentrez",
        db = db,
        id = x,
        rettype = rettype,
        retmode = retmode,
        verbose = verbose
      )
    })
  }
  if (parse) {
    if (verbose) {
      message("Attempting to parse retrieved metadata.")
    }
    res_parsed <- lapply(res, function(x) {
      ncbi_parse(meta = x, db = db, verbose = verbose)
    })
    if ("data.frame" %in% class(res_parsed[[1]])) {
      res_parsed <- dplyr::bind_rows(res_parsed)
    } else {
      res_parsed <- res
    }
  } else {
    res_parsed <- res
  }
  if (use_history) {
    out <- list(
      meta = res_parsed,
      db = db,
      web_history = uid$web_history
    )
  } else {
    out <- list(
      meta = res_parsed,
      db = db,
      web_history = NULL
    )
  }
  class(out) <- c("ncbi_meta", class(out))
  validate_webseq_class(out)
  return(out)
}
