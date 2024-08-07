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
    batch_size = 100,
    verbose = getOption("verbose")
    ){
  f <- try(get(paste("ncbi_link", from, to , sep = "_")), silent = TRUE)
  if (inherits(f, "try-error")) {
    stop("Link not supported.")
  }
  f(query, batch_size = batch_size, verbose = verbose)
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
    batch_size, 
    verbose = getOption("verbose")
  ) {
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

#' Convert NCBI BioSample IDs to NCBI Assembly IDs
#' 
#' This function converts one or more NCBI BioSample IDs to NCBI Assembly IDs.
#' @param biosample character; a vector of NCBI BioSample IDs.
#' @param batch_size integer; the number of search terms to query at once. 
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A data frame of NCBI Biosample IDs and matching Assembly IDs.
#' @examples
#' \dontrun{
#' ncbi_convert_assembly_biosample("GCF_000002435.2")
#' }
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#' @noRd
ncbi_link_biosample_assembly <- function(
    biosample,
    batch_size, 
    verbose = getOption("verbose")
  ) {
  uid <- ncbi_get_uid(
    biosample, 
    db = "biosample",
    batch_size = batch_size,
    use_history = FALSE,
    verbose = verbose
  )
  linked_uid <- ncbi_link_uid(
    uid,
    to = "assembly",
    batch_size = batch_size,
    verbose = verbose
  )
  linked_id <- tibble::tibble(
    biosample = ncbi_recover_id(linked_uid$biosample, db = "biosample"),
    assembly = ncbi_recover_id(linked_uid$assembly, db = "assembly")
  )
  out <- dplyr::left_join(
    tibble::tibble(biosample = biosample),
    linked_id,
    by = "biosample"
  )
  return(out)
}
