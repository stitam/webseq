#' Link UID-s from one NCBI database to another
#' 
#' Each entry in an NCBI database has its unique internal id. Entries in
#' different databases may be linked. For example, entries in the NCBI Assembly
#' database may be linked with entries in the NCBI BioSample database. This
#' function attempts to link uids from one database to another.
#' @param query either an object of class \code{ncbi_uid} or an integer vector 
#' of UIDs. See Details for more information.
#' @param from character; the database the queried UIDs come from.
#' \code{ncbi_dbs()} lists all available options.
#' @param to character; the database in which the function should look for links.
#' \code{ncbi_dbs()} lists all available options. See Details for more
#' information.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately. Not used when using web
#' history.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A tibble with two columns. The first column contains UIDs in the 
#' `from` database, the second column contains linked UIDs in the `to` database.
#' @details The function `ncbi_get_uid()` returns an object of class `ncbi_uid`. 
#' This object may be used directly as query for `ncbi_link_uid()`. If query is 
#' an `ncbi_uid` object, the `from` argument is optional. If `from` is not 
#' specified, the function will retrieve it from the query object. However, if 
#' it is specified, it must be identical to the `db` attribute of the query.
#' @examples
#' ncbi_link_uid(5197591, "assembly", "biosample")
#' ncbi_link_uid(c(1226742659, 1883410844), "protein", "nuccore")
#' @export
ncbi_link_uid <- function(
    query,
    from = NULL,
    to,
    batch_size = 100,
    verbose = getOption("verbose")
    ) {
  if ("ncbi_uid" %in% class(query)) {
    if (is.null(from)) {
      from <- query$db
    } else {
      if (from != query$db) {
        msg <- paste0(
          "Database for queried UIDs does not match 'from' argument.\n",
          "Provide identical values or use from = NULL (default)."
        )
        stop(msg)
      }
    }
  } else {
    if (is.null(from)) {
      msg <- paste0(
        "Specify a 'from' argument ",
        "or use an object of class 'ncbi_uid' as query."
      )
      stop(msg)
    }
  }
  from <- match.arg(from, ncbi_dbs())
  to <- match.arg(to, ncbi_db_links(from))
  foo_from_ids <- function(x, from, to) {
    if (length(x) == 1 && is.na(x)) {
      if (verbose) message("No valid UIDs.")
      return(tibble::tibble(query = x, uid = NA_real_))
    }
    id_hit <- suppressWarnings(wrap(
        "entrez_link",
        package = "rentrez",
        verbose = verbose,
        dbfrom = from,
        id = x,
        db = to,
        cmd = "neighbor",
        by_id = TRUE
    ))
    if ("elink" %in% class(id_hit)) {
      id_hit <- list(id_hit)
    }
    get_link <- function(hit, from, to) {
      out <- as.numeric(hit$links[[paste(from, to, sep = "_")]])
      if (is.null(out)) {
        return(NA)
      } else {
        return(out)
      }
    }
    out <- lapply(seq_along(x), function(i) {
      tibble::tibble(
        query = x[i],
        uid = get_link(id_hit[[i]], from = from, to = to)
      )
    })
    out <- dplyr::bind_rows(out)
    return(out)
  }
  if ("ncbi_uid" %in% class(query)) {
    query <- query$uid
  }
  if (!is.numeric(query)) {
    stop("Query must be an ncbi_uid object or a numeric vector or UIDs.")
  }
  idlist <- get_idlist(query, batch_size, verbose)
  res <- lapply(idlist, function(x) {
    foo_from_ids(
      x,
      from = from,
      to = to
    )
  })
  out <- dplyr::bind_rows(res)
  if ("ncbi_uid" %in% class(query)) {
    out <- dplyr::left_join(tibble::tibble(from = query$uid), out, by = "query")
  } else {
    out <- dplyr::left_join(tibble::tibble(query = query), out, by = "query")
  }
  names(out) <- c(from, to)
  return(out)
}
