#' Get sequence metadata from NCBI
#' 
#' This function retrieves sequence metadata from a given NCBI sequence
#' database.
#' @param query either an object of class \code{ncbi_uid} or an integer vector 
#' of UIDs. See Details for more information.
#' @param db character; the database to search in. For options see
#' \code{ncbi_dbs()}. Note that not all of these databases are supported.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param use_history logical; should the function use web history for faster
#' API queries?
#' @param parse logical; should the function attempt to parse the output into a
#' tibble?
#' @param mc_cores integer; number of cores to use for parallel processing. Only
#' used if \code{parse = TRUE}.
#' @param verbose logical; Should verbose messages be printed to console?
#' @return If \code{parse = FALSE} the function will return an object of class
#' \code{ncbi_meta}, which is a character vector with some extra information
#' about the database. This output can be used directly with \code{ncbi_parse}.
#' If \code{parse = TRUE} the function will attempt to parse the data using 
#' \code{ncbi_parse}. If parsing is successful, the function will return a
#' tibble, otherwise it will return the unparsed \code{ncbi_meta} object.
#' @details You can give UIDs to \code{ncbi_get_meta()} in two ways: 1. You can
#' use functions like \code{ncbi_get_uid()} or \code{ncbi_link_uid} to get UIDs,
#' and then use the returned \code{ncbi_uid} objects directly with
#' \code{ncbi_get_meta}. If you follow this approach then you do not have to
#' specify the \code{db} argument since the function can extract it from the
#' \code{ncbi_uid} object. However, if you do provide it, then it must be
#' identical to the \code{db} attribute of the \code{"ncbi_uid"} object. 2. 
#' Alternatively, you can just provide a vector of UIDs, but then you must 
#' specify the \code{db} argument as well.
#' @examples
#' \dontrun{
#' data(examples)
#' uids <- ncbi_get_uid(examples$biosample, db = "biosample")
#' meta <- ncbi_get_meta(uids)
#' }
#' @export
ncbi_get_meta <- function(
    query,
    db = NULL,
    batch_size = 100,
    use_history = TRUE,
    parse = TRUE,
    mc_cores = NULL,
    verbose = getOption("verbose")
  ) {
  if ("ncbi_uid" %in% class(query)) {
    if (is.null(db)) {
      db <- query$db
    } else {
      if (db != query$db) {
        msg <- paste0(
          "Database for queried UIDs does not match 'from' argument.\n",
          "Provide identical values or use from = NULL (default)."
        )
        stop(msg)
      }
    }
  } else {
    if (is.null(db)) {
      msg <- paste0(
        "Specify a 'db' argument ",
        "or use an object of class 'ncbi_uid' as query."
      )
      stop(msg)
    }
  }
  db <- match.arg(db, choices = ncbi_dbs())
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
  foo_from_histories <- function(query, x, db, verbose) {
    WH <- list(
      "WebEnv" = query$web_history$WebEnv[x],
      "QueryKey" = query$web_history$QueryKey[x]
    )
    class(WH) <- c("web_history", "list")
    id_count <- length(query$uid)
    retstart <- c(0, cumsum(rep(9999, times = floor(id_count/9999))))
    if (verbose) message(
      "Attempting to retrieve metadata for ", id_count, " sample(s). ",
      "Will use ", length(retstart), " batch(es)."
    )
    res <- list()
    for (i in seq_along(retstart)) {
      index_from = retstart[i] + 1
      index_to = min(c(retstart[i] + 9999, id_count))
      if (verbose) message(
        "Batch ", i, ". Retrieving sample(s) ", index_from, ":", index_to, ". ",
        appendLF = FALSE
      )
      res[[i]] <- wrap(
        "entrez_fetch",
        package = "rentrez",
        db = db,
        web_history = WH,
        rettype = rettype,
        retmode = retmode,
        verbose = verbose,
        retstart = retstart[i]
      )
    }
    res <- unlist(res)
    return(res)
  }
  foo_from_ids <- function(x, db) {
    if(length(x) == 1 && is.na(x)){
      if (verbose) message("No valid UIDs.")
      return(NA_character_)
    }
    res <- wrap(
      "entrez_fetch",
      package = "rentrez",
      db = db,
      id = x,
      rettype = rettype,
      retmode = retmode,
      verbose = verbose
    )
    return(res)
  }
  if ("ncbi_uid" %in% class(query) & use_history) {
    if (nrow(query$web_history) > 0) {
      if (verbose) message("Using web history.")
      res <- list()
      for (i in 1:nrow(query$web_history)) {
        if (verbose) message(paste0("Querying web history ", i, "."))
        res[[i]] <- foo_from_histories(query, i, db = db, verbose = verbose)
      }
      res <- unlist(res)
    } else {
      if (verbose) message("No web history found.")
      idlist <- get_idlist(query$uid, batch_size, verbose)
      res <- sapply(idlist, function(x) {
        foo_from_ids(x, db)
      })
    }
  } else {
    if ("ncbi_uid" %in% class(query)) {
      query <- query$uid
    }
    query <- as_numeric(query)
    idlist <- get_idlist(query, batch_size, verbose)
    res <- sapply(idlist, function(x) {
      foo_from_ids(x, db = db)
    })
  }
  attr(res, "db") = db
  class(res) <- c("ncbi_meta", class(res))
  validate_webseq_class(res)
  if (parse) {
    if (verbose) {
      message("Attempting to parse retrieved metadata.")
    }
    out <- ncbi_parse(meta = res, mc_cores = mc_cores, verbose = verbose)
  } else {
    out <- res
  }
  return(out)
}
