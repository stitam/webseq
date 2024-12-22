#' Convert accessions from NCBI to ENA
#'
#' Take a vector of NCBI accessions and convert them to ENA accessions.
#' @param accessions character; a vector or ENA accessions.
#' @param type character; type of NCBI accessions. Supported types:
#' `biosample`, `bioproject`.
#' @return A tibble with two columns, `ncbi` and `ena`.
#' @examples
#' ncbi2ena("SAMEA111452506", type = "biosample")
#' @importFrom magrittr %>%
#' @export
ncbi2ena <- function(accessions, type) {
  convert_repo_accession(
    accessions = accessions,
    from = "ncbi",
    to = "ena",
    type = type
  )
}
