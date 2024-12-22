#' Convert accessions from ENA to NCBI
#'
#' Take a vector of ENA accessions and convert them to NCBI accessions.
#' @param accessions character; a vector or ENA accessions.
#' @param type character; type of ENA accessions. Supported types: `sample`, 
#' `study`.
#' @return A tibble with two columns, `ena` and `ncbi`.
#' @examples
#' ena2ncbi("ERS3202441", type = "sample")
#' ena2ncbi(c("ERS3202441", "ERS3202442"), type = "sample")
#' ena2ncbi("ERP161024", type = "study")
#' @importFrom magrittr %>%
#' @export
ena2ncbi <- function(accessions, type) {
  convert_repo_accession(
    accessions = accessions,
    from = "ena",
    to = "ncbi",
    type = type
  )
}

# todo: function automatically sorts results, remove this
# todo: test if invalid and valid entries are mixed
