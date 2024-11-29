#' Convert accession between ENA and NCBI
#' 
#' Take a vector of accessions from either ENA or NCBI and convert them to 
#' accessions in the other repository.
#' @param accessions character; a vector or ENA or NCBI accessions.
#' @param from character; the repository to convert from.
#' @param to character; the repository to convert to.
#' @param type character; the name of the database in the from repository.
#' @return a tibble.
#' @noRd
convert_repo_accession <- function(accessions, from, to, type) {
  from <- match.arg(from, choices = c("ena", "ncbi"))
  to <- match.arg(to, choices = c("ena", "ncbi"))
  dict <- data.frame(
    ena = c("sample", "study"),
    ncbi = c("biosample", "bioproject")
  )
  if (from == "ena") type <- match.arg(type, choices = dict$ena)
  if (from == "ncbi") {
    type <- match.arg(type, choices = dict$ncbi)
    type <- dict$ena[which(dict$ncbi == type)]
  }
  url <- "https://www.ebi.ac.uk/ena/portal/api/search"
  query <- paste0(type, '_accession="', accessions, '"')
  if (from == "ena") query <- paste0("secondary_", query)
  query_collapsed <- paste(query, collapse = " OR ")
  body <- list(
    result = type,
    query = query_collapsed,
    fields = paste0(type, '_accession,secondary_', type, '_accession'),
    format = 'tsv'
  )
  res <- httr::POST(url, body = body, encode = "form")
  content <- httr::content(res, encoding = "utf-8")
  df <- utils::read.table(text = content, header = TRUE, sep = "\t") %>%
    dplyr::rename(
      "ncbi" = paste0(type, "_accession"),
      "ena" = paste0("secondary_", type, "_accession")
    )
  if (from == "ena") {
    df <- df %>% dplyr::relocate("ena")
  }
  df$ena <- as.character(df$ena)
  df$ncbi <- as.character(df$ncbi)
  if (from == "ena") {
    index <- which(accessions %in% df$ena)
  } else if (from == "ncbi") {
    index <- which(accessions %in% df$ncbi)
  }
  if (length(index) < length(accessions)) {
    if (from == "ena") {
      missing <- accessions[which(!accessions %in% df$ena)]
    } else if (from == "ncbi") {
      missing <- accessions[which(!accessions %in% df$ncbi)]
    }
    missing_collapsed <- paste(missing, collapse = ", ")
    msg <- paste0("The following accessions were not found: ", missing_collapsed)
    warning(msg)
  }
  df <- df[order(accessions[index]), ]
  tbl <- tibble::as_tibble(df)
  return(tbl)
}
