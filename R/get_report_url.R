#' Get the URL of an assembly report
#'
#' This function can be used to fetch urls that point to assembly reports.
#' @param uid numeric; a vector of internal IDs.
#' @param batch_size the number of IDs to query in one batch. The function will
#' stop with an error if \code{batch_size} is too high. Decrease if necessary.
#' @param cache logical; should results be exported to hard drive?
#' @param cache_file character; file path for the exported file.
#' @param update_threshold numeric; entries accessed earlier than this value
#' (days) will be updated in the cached database. Set below \code{0} to force
#' the update.
#' @param verbose logical; print messages to screen?
#' @note This is the second step within the pipeline for downloading GenBank
#' files.
#' @note Currently the function only works with ID's that refer to entries
#' within the NCBI Assembly database.
#' @seealso
#' \code{get_genomeid},
#' \code{download_report()},
#' \code{parse_report()},
#' \code{extract_accn()},
#' \code{download_gb()}
#' @examples
#' \dontrun{
#' phages <- get_genomeid("Autographiviridae", db = "assembly")
#' report_url <- get_report_url(phages$ids[1], cache = FALSE, verbose = TRUE)
#' }
#' @export
get_report_url <- function(uid,
                           batch_size = 10,
                           cache = FALSE,
                           cache_file = NULL,
                           update_threshold = 30,
                           verbose = getOption("verbose")){
  # todo: remove for loop
  # todo: move paths to a config file
  # todo: add input validation
  if (is.null(cache_file)) cache_file <- paste0(tempfile(), ".rda")
  if (grepl(".rda$", cache_file) == FALSE) {
    stop("Invalid input: cache_file must have .rda extension.")
  }
  if (file.exists(cache_file)) load(cache_file) else {
    urls <- data.frame(uid = vector(), url = vector(), accessed = vector())
  }
  if (verbose) print("Retrieving assembly report urls.")
  is_queried <- urls$uid %in% uid
  is_old <- (Sys.Date()-as.Date(urls$accessed)) > update_threshold
  olduid <- urls$uid[which(is_queried & is_old)]
  if (length(olduid > 0)) {
    if (cache & verbose) print("  Updating outdated entries.")
    nbatch <- ceiling(length(olduid)/batch_size)
    for (i in 1:nbatch) {
      if(i < nbatch) {
        query <- olduid[(batch_size*(i-1)+1):(batch_size*i)]
      } else {
        query <- olduid[(batch_size*(i-1)+1):length(olduid)]
      }
      data <- rentrez::entrez_fetch("assembly",
                                    id = query,
                                    rettype = "docsum",
                                    retmode = "json")
      data <- jsonlite::fromJSON(data)
      uids <- data$result$uids
      newurls <-unname(sapply(uids, function(x) {
        data$result[[x]]$ftppath_assembly_rpt
      }))
      updated_urls <- data.frame(uid = uids, url = newurls)
      index <- sapply(updated_urls$uid, function(x) which(urls$uid == x))
      urls$url[index] <-  updated_urls$index
      urls$accessed <- Sys.Date()
    }
  }
  newuid <- unique(uid[which(uid %in% urls$uid == FALSE)])
  if (length(newuid) > 0) {
    if (cache & verbose) print("  Retrieving new entries.")
    nbatch <- ceiling(length(newuid)/batch_size)
    for (i in 1:nbatch) {
      if(i < nbatch) {
        query <- newuid[(batch_size*(i-1)+1):(batch_size*i)]
      } else {
        query <- newuid[(batch_size*(i-1)+1):length(newuid)]
      }
      data <- rentrez::entrez_fetch("assembly",
                                    id = query,
                                    rettype = "docsum",
                                    retmode = "json")
      data <- jsonlite::fromJSON(data)
      uids <- data$result$uids
      newurls <-unname(sapply(uids, function(x) {
        data$result[[x]]$ftppath_assembly_rpt
      }))
      urls <- rbind(urls,
                    data.frame(uid = uids, url = newurls, accessed = Sys.Date()))
    }
  }
  if (cache) {
    if (!dir.exists(dirname(cache_file))) {
      dir.create(dirname(cache_file), recursive = TRUE)
    }
    save(urls, file = cache_file)
  }
  return(urls$url[which(urls$uid %in% uid)])
}
