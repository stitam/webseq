#' Retrieve NCBI Assembly metadata
#' 
#' @param assembly_uid numeric
#' @examples
#' \dontrun{
#' ncbi_meta_assembly(419738)
#' }
ncbi_meta_assembly <- function(assembly_uid) {
  foo <- function(x) {
    res <- rentrez::entrez_fetch(
      db = "assembly", id = x, rettype = "docsum", retmode = "xml")
    parsed_xml <- XML::xmlParse(res)
    rootnode <- XML::xmlRoot(parsed_xml)
    
    assembly <- XML::xpathSApply(
      rootnode, "//AssemblyAccession", XML::xmlValue)
    assembly <- coerce_xmlvalue(assembly)
    asm_name <- XML::xpathSApply(
      rootnode, "//AssemblyName", XML::xmlValue)
    asm_name <- coerce_xmlvalue(asm_name)
    bioproject <- XML::xpathSApply(
      rootnode, "//BioprojectAccn", XML::xmlValue)
    bioproject <- coerce_xmlvalue(bioproject)
    biosample <- XML::xpathSApply(
      rootnode, "//BioSampleAccn", XML::xmlValue)
    biosample <- coerce_xmlvalue(biosample)
    organism <- XML::xpathSApply(
      rootnode, "//Organism", XML::xmlValue)
    organism <- coerce_xmlvalue(organism)
    assembly_status <- XML::xpathSApply(
      rootnode, "//AssemblyStatus", XML::xmlValue)
    assembly_status <- coerce_xmlvalue(assembly_status)
    taxid <- XML::xpathSApply(
      rootnode, "//Taxid", XML::xmlValue)
    taxid <- coerce_xmlvalue(taxid)
    ftppath <- ifelse(
      grepl("^GCF", assembly),
      XML::xpathSApply(rootnode, "//FtpPath_RefSeq", XML::xmlValue),
      XML::xpathSApply(rootnode, "//FtpPath_GenBank", XML::xmlValue))
    ftppath <- coerce_xmlvalue(ftppath)
    out <- tibble::tibble(
      assembly = assembly,
      asm_name = asm_name,
      bioproject = bioproject,
      biosample = biosample,
      organism = organism,
      status = assembly_status,
      assembly_uid = x,
      taxid = taxid,
      ftppath = ftppath,
      accessed = Sys.Date())
    return(out)
  }
  out <- lapply(assembly_uid, foo)
  out <- dplyr::bind_rows(out)
  return(out)
}

coerce_xmlvalue <- function(x){
  if (length(x) > 1) return(paste(x, collapse = "|"))
  if (length(x) == 1 && x == "") return(NA)
  if (length(x) == 0) return(NA)
  return(x)
}
