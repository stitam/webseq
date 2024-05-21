#' Link UID-s from one NCBI database to another
#' 
#' Each entry in an NCBI database has its unique internal id. Entries in
#' different databases may be linked. For example, entries in the NCBI Assembly
#' database may be linked with entries in the NCBI BioSample database. This
#' function attempts to link uids from one database to another.
#' @param query either an object of class \code{ncbi_uid} or an integer vector 
#' of UIDs. See Details for more information.
#' @param to character; the database in which the function should look for links.
#' \code{ncbi_dbs()} lists all available options. See Details for more
#' information.
#' @param from character; the database the queried UIDs come from.
#' \code{ncbi_dbs()} lists all available options.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately. Not used when using web
#' history.
#' @param use_history logical; should the function use web history for faster
#' API queries?
#' @param verbose logical; should verbose messages be printed to the console?
#' \code{ncbi_dbs()} lists all available options.
#' @return An object of class \code{"ncbi_uid"} which is a list with three
#' elements:
#' \itemize{
#'  \item \code{uid}: a vector of UIDs.
#'  \item \code{db}: the database used for the query.
#'  \item \code{web_history}: a tibble of web histories.
#'  }
#' @details Some functions in webseq, e.g. \code{ncbi_get_uid()} or
#' \code{ncbi_link_uid()} return objects of class \code{"ncbi_uid"}. These
#' objects may be used directly as query input for \code{ncbi_link_uid()}. This
#' approach is recommended because the internal structure of these objects make
#' \code{ncbi_link_uid()} queries more robust. Alternatively, you can also
#' use a character vector of UIDs as query input.
#' @details If query is a \code{"ncbi_uid"} object, the \code{from} argument is
#' optional. If \code{from} is not specified, the function will use the
#' \code{db} attribute of the \code{"ncbi_uid"} object as \code{from} argument.
#' However, if it is specified, it must be identical to the \code{db} attribute
#' of the \code{"ncbi_uid"} object. If query is a character vector, the
#' \code{from} argument is required.
  #' @note \code{ncbi_link_uid()} can work with or without web histories, but the
#' behaviour of the function with web histories is unreliable. The option is
#' there but it is recommended NOT to use web histories with this function.
#' @examples
#' ncbi_link_uid("4253631", "assembly", "biosample")
#' ncbi_link_uid(c("1226742659", "1883410844"), "protein", "nuccore")
#' @export
ncbi_link_uid <- function(
    query,
    from = NULL,
    to,
    batch_size = 100,
    use_history = FALSE,
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
  foo_from_histories <- function(x, from, to) {
    WH <- list(
      "WebEnv" = query$web_history$WebEnv[x],
      "QueryKey" = query$web_history$QueryKey[x]
    )
    class(WH) <- c("web_history", "list")
    id_hit <- wrap(
        "entrez_link",
        package = "rentrez",
        verbose = verbose,
        dbfrom = from,
        web_history = WH,
        db = to,
        cmd = "neighbor"
    )
    wh_hit <- wrap(
        "entrez_link",
        package = "rentrez",
        verbose = verbose,
        dbfrom = from,
        web_history = WH,
        db = to,
        cmd = "neighbor_history"
    )
    if (
      (length(id_hit) == 1 && is.na(id_hit)) | 
      (length(wh_hit) == 1 && is.na(wh_hit))
      ) {
      return(list(
        uid = NA_real_,
        db = to,
        web_history = tibble::tibble()
      ))
    } else {
      return(list(
        uid = as.numeric(id_hit$links[[paste(from, to, sep = "_")]]),
        db = to,
        web_history = wh_hit$web_histories[[paste(from, to, sep = "_")]]
      ))
    }
  }
  foo_from_ids <- function(x, from, to, use_history) {
    if (length(x) == 1 && is.na(x)) {
      if (verbose) message("No valid UIDs.")
      return(list(
        uid = NA_real_,
        db = to,
        web_history = tibble::tibble()
      ))
    }
    id_hit <- wrap(
        "entrez_link",
        package = "rentrez",
        verbose = verbose,
        dbfrom = from,
        id = x,
        db = to,
        cmd = "neighbor"
    )
    if (use_history) {
      wh_hit <- wrap(
          "entrez_link",
          package = "rentrez",
          verbose = verbose,
          dbfrom = from,
          id = x,
          db = to,
          cmd = "neighbor_history"
      )
    } else {
      wh_hit <- NULL
    }
    if (
      (length(id_hit) == 1 && is.na(id_hit)) | 
      (length(wh_hit) == 1 && is.na(wh_hit))
      ) {
      return(list(
        uid = NA_real_,
        db = to,
        web_history = tibble::tibble()
      ))
    } else {
      if (is.null(wh_hit)) {
        web_history <- tibble::tibble()
      } else {
        web_history <- wh_hit$web_histories[[paste(from, to, sep = "_")]]
      }
      return(list(
        uid = as.numeric(id_hit$links[[paste(from, to, sep = "_")]]),
        db = to,
        web_history = web_history
      ))
    }
  }

  if ("ncbi_uid" %in% class(query) & use_history) {
    if (nrow(query$web_history) > 0) {
      if (verbose) message("Using web history.")
      res <- lapply(1:nrow(query$web_history), function(x) {
        foo_from_histories(
          x,
          from = from,
          to = to
        )
      })
    } else {
      if (verbose) message("No web history found.")
      idlist <- get_idlist(query$uid, batch_size, verbose)
      res <- lapply(idlist, function(x) {
        foo_from_ids(
          x,
          from = from,
          to = to,
          use_history = use_history
        )
      })
    }
  } else {
    if (verbose) message("Not using web history.")
    if ("ncbi_uid" %in% class(query)) {
      query <- query$uid
    }
    idlist <- get_idlist(query, batch_size, verbose)
    res <- lapply(idlist, function(x) {
      foo_from_ids(
        x,
        from = from,
        to = to,
        use_history = use_history
      )
    })
  }
  uid <- lapply(res, function(x) x$uid)
  web_histories <- lapply(res, function(x) x$web_history)
  web_histories <- dplyr::bind_rows(web_histories)
  out <- list(
    uid = unlist(uid),
    db = to,
    web_history = web_histories
  )
  class(out) <- c("ncbi_uid", class(out))
  validate_webseq_class(out)
  return(out)
}
