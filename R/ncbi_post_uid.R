#' Post a vector of NCBI UIDs
#'
#' This function posts a vector of NCBI UIDs to the NCBI webservice. The purpose
#' of this function is to generate an ncbi_uid object with web history which can
#' then be used in downstream functions.
#' @param uid numeric; a vector of NCBI UIDs.
#' @param db character; the database the UIDs belong to. For options see
#' `ncbi_dbs()`.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return An object of class \code{"ncbi_uid"} which is a list with three
#' elements:
#' \itemize{
#'  \item \code{uid}: a vector of UIDs.
#'  \item \code{db}: the database the UIDs belong to.
#'  \item \code{web_history}: a tibble of web histories.
#' }
#' @details The default value for \code{batch_size} should work in most cases.
#' However, if the vector of UIDs is very long, the function may fail with an
#' error message. In this case, try reducing the \code{batch_size} value.
#' @note In webseq NCBI UIDs are acquired using `ncbi_get_uid()` or 
#' `ncbi_link_uid()` and are used for acquiring linked UIDs in other NCBI 
#' databases or for retrieving the data itself. In some cases data retrieval is
#' faster when using web history. However, you may have UIDs without a web
#' history or your web history may expire. In this case you can use 
#' `ncbi_post_uid()` to generate a new ncbi_uid object with web history.
#' @examples
#' ncbi_post_uid(4505768, db = "biosample")
#' @export
ncbi_post_uid <- function(
    uid, 
    db,
    batch_size = 100,
    verbose = getOption("verbose")
  ) {
  uid <- validate_ncbi_uid(uid)
  db <- match.arg(db, choices = ncbi_dbs())
  uidlist <- get_idlist(uid, batch_size = batch_size)
  res <- lapply(uidlist, function(x) {
    res <- rentrez::entrez_post(db = db, id = x)
    out <- list(
      uid = x,
      web_history = tibble::tibble(
        WebEnv = res$WebEnv,
        QueryKey = res$QueryKey
      )
    )
    return(out)
  })
  uid <- lapply(res, function(x) x$uid)
  web_histories <- lapply(res, function(x) x$web_history) |>
    dplyr::bind_rows()
  out <- list(
    uid = unlist(uid),
    db = db,
    web_history = web_histories
  )
  class(out) <- c("ncbi_uid", class(out))
  validate_webseq_class(out)
  return(out)
}
