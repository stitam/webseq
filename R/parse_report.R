#' Parse assembly reports
#'
#' This function can be used to parse a downloaded assembly report.
#' @param file character; the file path to the assembly report.
#' @return The function returns an object of classes \code{arpt} and
#' \code{list}. The unique class is required for compatibility with subsequent
#' functions in the pipeline. Otherwise data from the returned object can be
#' extracted through general list operations.
#' @note This is the fourth step within the pipeline for downloading GenBank
#' files.
#' @seealso
#' \code{get_genomeid},
#' \code{get_report_url()},
#' \code{download_report()},
#' \code{extract_accn()},
#' \code{download_gb()}
#' @examples
#' \dontrun{
#' phages <- get_genomeid("Autographiviridae", db = "assembly")
#' report_url <- get_report_url(phages$ids[1])
#' download_report(report_url)
#' filename <- dir(paste0(tempdir(), "/assembly_reports"))
#' filepath <- paste0(tempdir(), "/assembly_reports/", filename)
#' parse_report(filepath)
#' }
#' @export
parse_report <- function(file){
  foo <- function(x) {
    data <- readLines(x)
    data <- gsub("# *", "", data)
    spacer <- which(data == "")
    block_1 <- data[1:(spacer[1]-1)]
    block_1_names <- sapply(block_1, function(x) strsplit(x, ": +")[[1]][1])
    block_1_values <- sapply(block_1, function(x) strsplit(x, ": +")[[1]][2])
    block_1_values <- ifelse(block_1_values == "na",
                             NA_character_,
                             block_1_values)
    block_1 <- as.list(t(block_1_values))
    names(block_1) <- block_1_names
    block_2 <- data[(spacer[1]+1):(spacer[2]-1)]
    block_2_name <- block_2[1]
    block_2_name <- gsub(":", "", block_2_name)
    block_2_values <- lapply(block_2[3:length(block_2)], function(x) {
      values <- strsplit(x, "\t")[[1]]
      values <- ifelse(values %in% c("", "na"), NA_character_, values)
      data.frame(t(values))
    })
    block_2_values <- dplyr::bind_rows(block_2_values)
    block_2_values <- tibble::as_tibble(block_2_values)
    names(block_2_values) <-strsplit(block_2[2], "\t")[[1]]
    block_2 <- list(block_2_values)
    names(block_2) <- block_2_name
    block_3 <- data[(spacer[2]+1):(spacer[3]-1)]
    block_3 <- list(Notes = paste(block_3, collapse = " "))
    block_4 <- data[(spacer[3]+1):length(data)]
    block_4_values <- lapply(block_4[2:length(block_4)], function(x) {
      values <- strsplit(x, "\t")[[1]]
      values <- ifelse(values %in% c("","na"), NA_character_, values)
      data.frame(t(values))
    })
    block_4_values <- dplyr::bind_rows(block_4_values)
    block_4_values <- tibble::as_tibble(block_4_values)
    names(block_4_values) <- strsplit(block_4[1], "\t")[[1]]
    block_4 <- list(`Assembly definition` = block_4_values)
    return(c(block_1,block_2,block_3,block_4))
  }
  rpt <- try(foo(file), silent = TRUE)
  if (inherits(rpt, "try-error")) stop()
  if (is.null(rpt$`Assembly name`)) stop()
  if (is.null(rpt$`Assembly-Units`$`GenBank Unit Accession`)) stop()
  if (is.null(rpt$`Assembly definition`$`GenBank-Accn`)) stop()
  if (is.null(rpt$`Assembly-Units`$`RefSeq Unit Accession`)) stop()
  if (is.null(rpt$`Assembly definition`$`RefSeq-Accn`)) stop()
  rpt$Taxid <- as.numeric(rpt$Taxid)
  rpt$Date <- as.Date(rpt$Date)
  class(rpt) <- c("arpt", "list")
  return(rpt)
}
