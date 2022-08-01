tidy_biosample <- function(biosample_df) {
  na_terms <- c(
    "missing", "na", "n/a", "none", "not applicable", "not collected", 
    "not_determined", "unknown", "NA")
  out <- tibble::as_tibble(data.frame(lapply(biosample_df, function(x) {
    ifelse(tolower(x) %in% na_terms == FALSE, x, NA)
  })))
  return(out)
}