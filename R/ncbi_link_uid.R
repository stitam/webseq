#' Link UID-s from one NCBI database to another
#' 
#' Each entry in an NCBI database has its unique internal id. Entries in
#' different databases may be linked. For example, entries in the NCBI Assembly
#' database may be linked with entries in the NCBI BioSample database. This
#' function attempts to link uids from one database to another.
#' @param query either an object of class `ncbi_uid` or `ncbi_uid_link`, or an
#' integer vector of UIDs. See Details for more information.
#' @param from character; the database the queried UIDs come from.
#' \code{ncbi_dbs()} lists all available options. See Details for more
#' information.
#' @param to character; the database in which the function should look for links.
#' \code{ncbi_dbs()} lists all available options. 
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A tibble with two or more columns. When `ncbi_link_uid()` is called
#' on a `ncbi_uid` object or a vector of UIDs, the function returns a tibble
#' with exactly two columns: the first column contains UIDs in the `from`
#' database, and the second column contains linked UIDs in the `to` database.
#' However, `ncbi_link_uid()` can be called multiple times in succession. Each
#' call after the first call will add a new column to the returned tibble. 
#' See Details for more information.
#' @details The function can take three query classes: It can take `ncbi_uid`
#' objects, these are returned by `ncbi_get_uid()`. In this case, the `from`
#' argument will be retrieved from the query object, by default. It can also
#' take `ncbi_uid_link` objects, which means `ncbi_link_uid()` can be called
#' several times in a sequence to perform a number of successive conversions.
#' When the query is an `ncbi_uid_link` object, the function will always convert
#' the UIDs in the last column of the query object, and will retrieve the `from`
#' argument from the name of the last column. This means links should always be
#' interpreted "left-to-right". Note, when tibbles are joined during subsequent
#' `ncbi_link_uid` calls they are joined using "many-to-many" relationships; see
#' `?dplyr::left_join()` for more information. Lastly, the function can also
#' take a vector of integer UIDs.
#' @examples
#' # Simple call with integer UIDs
#' ncbi_link_uid(5197591, "assembly", "biosample")
#' ncbi_link_uid(c(1226742659, 1883410844), "protein", "nuccore")
#' 
#' # Complex call with ncbi_get_uid() and several ncbi_link_uid() calls
#' "GCF_000299415.1" |> 
#'   ncbi_get_uid(db = "assembly") |> 
#'   ncbi_link_uid(to = "biosample") |>
#'   ncbi_link_uid(to = "bioproject") |>
#'   ncbi_link_uid(to = "pubmed")
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
  } else if ("ncbi_uid_link" %in% class(query)) {
    fromdb <- names(query)[length(names(query))]
    if (is.null(from)) {
      from <- fromdb
    } else {
      if (from != fromdb) {
        msg <- paste0(
          "Database for queried UIDs does not match 'from' argument.\n",
          "Provide identical values (last column name) or use from = NULL (default)."
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
      return(tibble::tibble(query = x, uid = NA_integer_))
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
    query_vector <- query$uid
  } else if ("ncbi_uid_link" %in% class(query)) {
    query_vector <- query[[from]] |> unique()
  } else {
    query_vector <- query
  }
  if (!is.numeric(query_vector)) {
    stop("Query must be an ncbi_uid object or a numeric vector or UIDs.")
  }
  idlist <- get_idlist(query_vector, batch_size, verbose)
  res <- lapply(idlist, function(x) {
    foo_from_ids(
      x,
      from = from,
      to = to
    )
  })
  out <- dplyr::bind_rows(res)
  if ("ncbi_uid" %in% class(query)) {
    out <- dplyr::left_join(
      tibble::tibble(query = query_vector),
      out,
      by = "query"
    )
    names(out) <- c(from, to)
  } else if ("ncbi_uid_link" %in% class(query)) {
    names(out) <- c(from, to)
    out <- dplyr::left_join(
      query,
      out,
      by = from,
      relationship = "many-to-many"
    )
  } else {
    out <- dplyr::left_join(
      tibble::tibble(query = query_vector),
      out,
      by = "query"
    )
    names(out) <- c(from, to)
  }
  if (!"ncbi_uid_link" %in% class(out)) {
    class(out) <- c("ncbi_uid_link", class(out))
  }
  return(out)
}
