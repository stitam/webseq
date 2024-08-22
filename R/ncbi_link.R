#' Link ID-s from one NCBI database to another
#' 
#' Each entry in an NCBI database has its unique ID. Entries in different 
#' databases may be linked. For example, entries in the NCBI Assembly database
#' may be linked with entries in the NCBI BioSample database. This function 
#' attempts to link ID-s from one database to another.
#' @param query character; a vector of IDs
#' @param from character; the database the queried ID-s come from.
#' \code{ncbi_dbs()} lists all available options.
#' @param to character; the database in which the function should look for links.
#' \code{ncbi_dbs()} lists all available options.
#' @param multiple character; handling of rows in x with multiple matches in y.
#' For more information see `?dplyr::left_join()`.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A tibble with two columns. The first column contains IDs in the 
#' `from` database, the second column contains linked IDs in the `to` database.
#' @examples
#' \dontrun{
#' ncbi_link("GCF_000002435.2", from = "assembly", to = "biosample")
#' ncbi_link("SAMN02714232", from = "biosample", to = "assembly")
#' }
#' @export
ncbi_link <- function(
    query, 
    from,
    to,  
    multiple = "all",
    batch_size = 100,
    verbose = getOption("verbose")
    ){
  if (from == "assembly") {
    if (to == "biosample") {
      linkfun <- "ncbi_link_assembly_biosample"
    } else {
    stop("Link not supported.")
    }
  } else {
    linkfun <- "ncbi_link_generic"
  }
  f <- get(linkfun)
  f(
    query,
    from = from,
    to = to,
    multiple = multiple,
    batch_size = batch_size,
    verbose = verbose
  )
}

#' Convert NCBI Assembly IDs to NCBI BioSample IDs
#' 
#' This function converts one or more NCBI Assembly IDs to NCBI BioSample IDs.
#' @param assembly character; a vector of NCBI Assembly IDs.
#' @param batch_size integer; the number of search terms to query at once. 
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A data frame of NCBI Assembly IDs and matching BioSample IDs.
#' @examples
#' \dontrun{
#' ncbi_convert_assembly_biosample("GCF_000002435.2")
#' }
#' @importFrom dplyr left_join
#' @importFrom tibble as_tibble
#' @noRd
ncbi_link_assembly_biosample <- function(
    assembly, 
    from = "assembly",
    to = "biosample",
    multiple = "all",
    batch_size, 
    verbose = getOption("verbose")
  ) {
  from <- match.arg(from, "assembly")
  to <- match.arg(to, "biosample")
  from_uid <- ncbi_get_uid(
    assembly,
    db = "assembly",
    batch_size = batch_size,
    verbose = verbose
  )
  res <- ncbi_get_summary(query = from_uid, verbose = verbose)
  ids <- data.frame(
    assembly_gbk = unname(sapply(res, function(x) x$synonym$genbank)),
    assembly_rsq = unname(sapply(res, function(x) x$synonym$refseq)),
    biosample = unname(sapply(res, function(x) x$biosampleaccn))
  )
  ids$assembly_rsq <- ifelse(ids$assembly_rsq == "", NA, ids$assembly_rsq)
  index_gbk <- which(ids$assembly_gbk %in% assembly[which(!is.na(assembly))])
  index_rsq <- which(ids$assembly_rsq %in% assembly[which(!is.na(assembly))])
  out <- data.frame(
    assembly = assembly
  )
  out_gbk <- dplyr::right_join(
    out, 
    ids[index_gbk, c("assembly_gbk", "biosample")], 
    by = c("assembly" = "assembly_gbk")
  )
  out_rsq <- dplyr::right_join(
    out,
    ids[index_rsq, c("assembly_rsq", "biosample")],
    by = c("assembly" = "assembly_rsq")
  )
  out_both <- dplyr::bind_rows(out_gbk, out_rsq)
  out <- dplyr::left_join(out, out_both, by = "assembly")
  out <- tibble::as_tibble(out)
  return(out)
}

#' Convert NCBI IDs between databases
#' 
#' This function converts one or more NCBI IDs between databases. This generic 
#' function retrieves UIDs from one database, links them to UIDs from another
#' database and then recovers the IDs from the UIDs. The function should work
#' with most links
#' @param query character; a vector of IDs
#' @param from character; the database the queried ID-s come from.
#' \code{ncbi_dbs()} lists all available options.
#' @param to character; the database in which the function should look for links.
#' \code{ncbi_dbs()} lists all available options.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A tibble
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#' @noRd
ncbi_link_generic <- function(
    query,
    from,
    to,
    multiple = "all",
    batch_size, 
    verbose = getOption("verbose")
  ) {
  uid <- ncbi_get_uid(
    query,
    db = from,
    batch_size = batch_size,
    use_history = FALSE,
    verbose = verbose
  )
  linked_uid <- ncbi_link_uid(
    uid,
    to = to,
    batch_size = batch_size,
    verbose = verbose
  )
  linked_id <- tibble::tibble(
    from = ncbi_recover_id(linked_uid[[1]], db = from),
    to = ncbi_recover_id(linked_uid[[2]], db = to)
  )
  out <- dplyr::left_join(
    tibble::tibble(from = query),
    linked_id,
    multiple = multiple,
    relationship = "many-to-many"
  ) |> suppressMessages()
  names(out) <- c(from, to)
  class(out) <- c("ncbi_link", class(out))
  return(out)
}
