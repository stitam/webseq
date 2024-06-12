#' Convert accessions from NCBI to ENA
#'
#' Take a vector of NCBI accessions and convert them to ENA accessions.
#' @param accessions character; a vector or ENA accessions.
#' @param type character; type of accessions. Currently only sample accessions
#' are supported.
#' @return A tibble with two columns, `ncbi` and `ena`.
#' @examples
#' ncbi2ena("SAMEA111452506")
#' @importFrom magrittr %>%
#' @export
ncbi2ena <- function(accessions, type = "sample") {
  type <- match.arg(type, choices = "sample")
  url <- "https://www.ebi.ac.uk/ena/portal/api/search"
  query <- paste0('sample_accession="', accessions, '"')
  query_collapsed <- paste(query, collapse = " OR ")
  body <- list(
    result = 'sample',
    query = query_collapsed,
    fields = 'sample_accession,secondary_sample_accession',
    format = 'tsv'
  )
  res <- httr::POST(url, body = body, encode = "form")
  content <- httr::content(res, encoding = "utf-8")
  df <- utils::read.table(text = content, header = TRUE, sep = "\t") %>%
    dplyr::rename(
      "ncbi" = "sample_accession",
      "ena" = "secondary_sample_accession"
    ) %>%
    dplyr::relocate("ncbi")
  df$ncbi <- as.character(df$ncbi)
  df$ena <- as.character(df$ena)
  index <- which(accessions %in% df$ncbi)
  if (length(index) < length(accessions)) {
    missing <- accessions[which(!accessions %in% df$ncbi)]
    missing_collapsed <- paste(missing, collapse = ", ")
    msg <- paste0("The following accessions were not found: ", missing_collapsed)
    warning(msg)
  }
  df <- df[order(accessions[index]), ]
  tbl <- tibble::as_tibble(df)
  return(tbl)
}
