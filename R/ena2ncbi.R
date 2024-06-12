#' Convert accessions from ENA to NCBI
#'
#' Take a vector of ENA accessions and convert them to NCBI accessions.
#' @param accessions character; a vector or ENA accessions.
#' @param type character; type of accessions. Currently only sample accessions
#' are supported.
#' @return A tibble with two columns, `ena` and `ncbi`.
#' @examples
#' ena2ncbi("ERS3202441")
#' ena2ncbi(c("ERS3202441", "ERS3202442"))
#' @importFrom magrittr %>%
#' @export
ena2ncbi <- function(accessions, type = "sample") {
  type <- match.arg(type, choices = "sample")
  url <- "https://www.ebi.ac.uk/ena/portal/api/search"
  query <- paste0('secondary_sample_accession="', accessions, '"')
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
    dplyr::relocate("ena")
  df$ena <- as.character(df$ena)
  df$ncbi <- as.character(df$ncbi)
  index <- which(accessions %in% df$ena)
  if (length(index) < length(accessions)) {
    missing <- accessions[which(!accessions %in% df$ena)]
    missing_collapsed <- paste(missing, collapse = ", ")
    msg <- paste0("The following accessions were not found: ", missing_collapsed)
    warning(msg)
  }
  df <- df[order(accessions[index]), ]
  tbl <- tibble::as_tibble(df)
  return(tbl)
}

# todo: function automatically sorts results, remove this
# todo: test if invalid and valid entries are mixed
# todo: add tests for ncbi2ena
