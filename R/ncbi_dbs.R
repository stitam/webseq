ncbi_dbs <- function() {
  if (!dir.exists(tempdir())) dir.create(tempdir())
  if (!file.exists(paste0(tempdir(), "/webseq.rds"))) {
    dbs <- rentrez::entrez_dbs()
    saveRDS(dbs, file = paste0(tempdir(), "/webseq.rds"))
  } else {
    dbs <- readRDS(paste0(tempdir(), "/webseq.rds"))
  }
  return(dbs)
}