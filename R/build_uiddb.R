#' Build UID Database
#'
#' This function connects GCA/GCF type assembly accessions with internal ID-s
#' within a number of NCBI databases.
#' @importFrom dplyr anti_join distinct
#' @param assemblies character; vector of assembly accessions.
#' @param dbpath character; path to an exported uiddb database. \code{NULL} by
#' default in which case the database is not exported but remains in memory.
#' @param update_after numeric; if and entry in the database is older than this
#' value (in days) and the entry is queried again, then it will be updated in
#' the database.
#' @param verbose logical; print verbose messages to console
#' @return A data frame of unique ID-s. If \code{dbpath} is not \code{NULL}
#' then the data frame is also exported as a .rda file and is read within
#' subsequent function calls.
#' @examples
#' \dontrun{
#' ids <- c("GCA_003012895.2", "GCF_000695855.3")
#' build_uiddb(ids)
#' }
#' @export

build_uiddb <- function(assemblies,
                        dbpath = NULL,
                        update_after = 30,
                        verbose = getOption("verbose")) {
  foo <- function(query) {
    Sys.sleep(runif(1,0.2,0.5))
    assembly_uids <- sapply(query, function(x) ncbi_get_uid(x, db = "assembly"))
    Sys.sleep(runif(1,0.2,0.5))
    taxonomy_uids <- link_uids(
      assembly_uids, from = "assembly", to = "taxonomy")
    Sys.sleep(runif(1,0.2,0.5))
    biosample_uids <- link_uids(
      assembly_uids, from = "assembly", to = "biosample")
    newdb <- data.frame(
      assembly = query,
      assembly_uid = assembly_uids,
      biosample_uid = biosample_uids,
      taxid = taxonomy_uids,
      last_updated = Sys.Date()
    )
    rownames(newdb) <- NULL
    return(newdb)
  }
  if (!is.null(dbpath) && file.exists(dbpath)) {
    load(dbpath)
  } else {
    uiddb <- data.frame()
  }
  sysdate <- Sys.Date()
  for (i in seq_along(assemblies)) {
    if (verbose) message(i, "/", length(assemblies), ": ", appendLF = FALSE)
    if (assemblies[i] %in% uiddb$assembly) {
      index <- which(uiddb$assembly == assemblies[i])
      if (sysdate > uiddb$last_updated[index] + update_after) {
        if (verbose) message("Updating ", assemblies[i], ". ", appendLF = FALSE)
        newline <- foo(assemblies[i])
        index <- which(uiddb$assembly == assemblies[i])
        uiddb[index] <- newline
        if (!is.null(dbpath)) save(uiddb, file = dbpath)
        if (verbose) message("Done.")
      } else {
        if (verbose) message(assemblies[i], " up to date. Next.")
        next() 
      }
    } else {
      if (verbose) message("Adding ", assemblies[i], ". ", appendLF = FALSE)
      newline <- foo(assemblies[i])
      uiddb <- rbind(uiddb, newline)
      uiddb <- uiddb[order(uiddb$assembly),]
      if (!is.null(dbpath)) save(uiddb, file = dbpath)
      if (verbose) message("Done.")
    }
  }
  return(uiddb)
}
