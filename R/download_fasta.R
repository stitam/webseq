download_fasta <- function(id, fasta_dir = "./cache/fasta") {
  if (!dir.exists(fasta_dir)) dir.create(fasta_dir, recursive = TRUE)
  data <- jsonlite::fromJSON(rentrez::entrez_fetch("assembly",
                                                   id,
                                                   rettype = "docsum",
                                                   retmode = "json"))
  rpt_url <- data$result[[2]]$ftppath_assembly_rpt
  ftp_dir <- stringi::stri_sub(
    rpt_url,
    1,
    stringi::stri_locate_last(rpt_url, regex = "/")[1]-1)
  filename <- paste0(
    stringi::stri_sub(
      ftp_dir,
      stringi::stri_locate_last(ftp_dir, regex = "/")[1]+1, nchar(ftp_dir)),
    "_genomic.fna.gz")
  fasta_url <- paste0(ftp_dir, "/", filename)
  destfile <-  paste0(fasta_dir, "/", filename)
  if (!file.exists(destfile)) {
   down <- try(download.file(fasta_url, destfile = destfile), silent = TRUE)
    if (inherits(down, "try-error")) {
      warning("Download failed for ", filename, ".")
    }
  }
}
