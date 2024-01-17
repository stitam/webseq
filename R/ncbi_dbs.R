ncbi_dbs <- function() {
  if (!dir.exists(tempdir())) dir.create(tempdir())
  if (!file.exists(paste0(tempdir(), "/webseq_ncbi_dbs.rds"))) {
    dbs <- rentrez::entrez_dbs()
    saveRDS(dbs, file = paste0(tempdir(), "/webseq_ncbi_dbs.rds"))
  } else {
    dbs <- readRDS(paste0(tempdir(), "/webseq_ncbi_dbs.rds"))
  }
  return(dbs)
}