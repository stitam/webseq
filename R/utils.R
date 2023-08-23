mgnify_endpoints <- function() {
  apis <- c(
    "biomes", "studies", "super-studies", "samples", "runs", "assemblies",
    "analyses", "experiment-types", "pipelines", "pipeline-tools",
    "publications", "genomes", "release", "genomeset", "cogs", "kegg-modules",
    "kegg-classes", "antismash-geneclusters", "annotations/go-terms",
    "annotations/interpro-identifiers", "annotations/kegg-modules",
    "annotations/pfam-entries", "annotations/kegg-orthologs", 
    "annotations/genome-properties", "annotations/antismash-gene-clusters",
    "annotations/organisms", "mydata")
  others <- c(
    "downloads"
  )
  endpoints <- c(apis, others)
  return(endpoints)
}

#' Try an URL with default parameters
#' 
#' This is a convenience wrapper for trying URLs
#' @param verb character; Name of the verb to use.
#' @param qurl character; Query URL.
#' @importFrom httr RETRY
#' @noRd
try_url <- function(verb, qurl, ...){
  try(httr::RETRY(verb,
                  qurl,
                  terminate_on = 404,
                  quiet = TRUE,
                  ...), silent = TRUE)
}

webseq_message <- function(action = c("na",
                                     "query",
                                     "query_all",
                                     "not_found",
                                     "not_available",
                                     "service_down"),
                            appendLF = TRUE,
                            ...) {
  action <- match.arg(action)
  string <- switch(
    action,
    na = "Query is NA. Returning NA.",
    query = paste0("Querying ", ..., ". "),
    query_all = "Querying. ",
    not_found = "Not found. Returning NA.",
    not_available = "Not available. Returning NA.",
    service_down = "Service not available. Returning NA."
  )
  message(string, appendLF = FALSE)
  if (appendLF) message("")
}
