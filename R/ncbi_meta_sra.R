#function currently used to get run id from sra_uid
ncbi_meta_sra <- function(sra) {
  init <- substr(sra, 1, 3)
  sra_uid <- ncbi_get_uid(sra, db = "sra")
  res <- rentrez::entrez_fetch(
    db = "sra", id = sra_uid, rettype = "docsum", retmode = "json")
  cont <- jsonlite::fromJSON(res)$result[[2]]$runs
  run <- gsub('\\"', "", cont)
  if (init == "ERS") {
    run <- stringi::stri_extract(run, regex = "acc=ERR+[0-9]+")
  }
  if (init == "SRS") {
    run <- stringi::stri_extract(run, regex = "acc=SRR+[0-9]+")
  }
  run <- gsub("acc=", "", run)
  return(run)
}
