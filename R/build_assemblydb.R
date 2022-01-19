#' Build UID Database
#'
#' This function connects GCA/GCF type assembly accessions with internal ID-s
#' within a number of NCBI databases.
#' @importFrom dplyr anti_join distinct
#' @param assemblies character; vector of assembly accessions.
#' @param dbpath character; path to an exported assemblydb database. \code{NULL} by
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
#' build_assemblydb(ids, verbose = TRUE)
#' }
#' @export

build_assemblydb <- function(assemblies,
                             dbpath = NULL,
                             update_after = 30,
                             verbose = getOption("verbose")) {
  foo <- function(x, verbose) {
    if (grepl("^GC[A|F]_[0-9]+\\.[0-9]+", x) == FALSE) {
      if (verbose) {
        message("Failed. Not an assembly accession.")
      }
      return(NA)
    }
    assembly_uid <- try(get_uid(x, db = "assembly"), silent = TRUE)
    if (inherits(assembly_uid, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      return(NA)
    }
    if (length(assembly_uid) == 1 && is.na(assembly_uid)) {
      if (verbose) message("Failed. Assembly accession not found.")
      return(NA)
    }
    if (length(assembly_uid) > 1) {
      if (verbose) {
        message("Failed. Multiple assembly uid-s found: ",
                paste(assembly_uid, collapse = ", "), ".") 
      }
      return(NA)
    }
    Sys.sleep(runif(1,0.2,0.5))
    assembly_meta <- try(ncbi_meta_assembly(assembly_uid), silent = TRUE)
    if (inherits(assembly_meta, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      return(NA)
    }
    if (nrow(assembly_meta) != 1) {
      if (verbose) message("Failed. Assembly metadata not appropriate.")
      return(NA)
    }
    if (assembly_meta$assembly != x) {
      if (verbose) message("Failed. Assembly metadata not appropriate.")
      return(NA)
    }
    Sys.sleep(runif(1,0.2,0.5))
    if (!is.na(assembly_meta$biosample)){
      biosample_uid <- try(
        get_uid(assembly_meta$biosample, db = "biosample"), silent = TRUE)
    }
    if (inherits(biosample_uid, "try-error")) {
      if (verbose) message("Failed. Webservice temporarily down.")
      return(NA)
    }
    if (is.na(biosample_uid)) {
      if (verbose) message("Failed. Biosample not found.")
      return(NA)
    }
    index <- which(names(assembly_meta) == "assembly_uid")
    newdb <- dplyr::bind_cols(assembly_meta[,1:index],
                              tibble::tibble(biosample_uid = biosample_uid),
                              assembly_meta[,(index+1):ncol(assembly_meta)])
    return(newdb)
  }
  if (!is.null(dbpath) && file.exists(dbpath)) {
    load(dbpath)
  } else {
    assemblydb <- data.frame()
  }
  sysdate <- Sys.Date()
  for (i in seq_along(assemblies)) {
    if (verbose) message(i, "/", length(assemblies), ": ", appendLF = FALSE)
    if (assemblies[i] %in% assemblydb$assembly) {
      index <- which(assemblydb$assembly == assemblies[i])
      if (sysdate > assemblydb$accessed[index] + update_after) {
        if (verbose) message("Updating ", assemblies[i], ". ", appendLF = FALSE)
        newline <- foo(assemblies[i], verbose = verbose)
        if (length(newline) == 1 && is.na(newline)) next()
        index <- which(assemblydb$assembly == assemblies[i])
        assemblydb[index] <- newline
        if (!is.null(dbpath)) save(assemblydb, file = dbpath)
        if (verbose) message("Done.")
      } else {
        if (verbose) message(assemblies[i], " up to date. Next.")
        next() 
      }
    } else {
      if (verbose) message("Adding ", assemblies[i], ". ", appendLF = FALSE)
      newline <- foo(assemblies[i], verbose = verbose)
      if (length(newline) == 1 && is.na(newline)) next()
      assemblydb <- rbind(assemblydb, newline)
      assemblydb <- assemblydb[order(assemblydb$assembly),]
      if (!is.null(dbpath)) save(assemblydb, file = dbpath)
      if (verbose) message("Done.")
    }
  }
  return(assemblydb)
}
