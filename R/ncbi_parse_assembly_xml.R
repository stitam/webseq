#' Parse NCBI assembly metadata 
#' 
#' This function can be used to parse an xml file from the NCBI assembly
#' database into a tibble.
#' @param file character; path to an xml file.
#' @param verbose logical; Should verbose messages be printed to console?
#' @returns a tibble.
#' @examples
#' \dontrun{
#' # search for Acinetobacter baumannii within the NCBI Assembly database
#' # https://www.ncbi.nlm.nih.gov/assembly/?term=acinetobacter%20baumannii
#' # upper right corner -> send to -> file -> format = xml -> create file
#' # parse the downloaded file
#' ncbi_parse_assembly_xml("assembly_summary.xml")
#' }
#' @export
ncbi_parse_assembly_xml <- function(file, verbose = getOption("verbose")) {
  foo <- function(x) {
    rootnode <- XML::xmlRoot(x)
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
    wgs_project <- XML::xpathSApply(
      rootnode, "//WGS", XML::xmlValue)
    wgs_project <- coerce_xmlvalue(wgs_project)
    assembly_status <- XML::xpathSApply(
      rootnode, "//AssemblyStatus", XML::xmlValue)
    assembly_status <- coerce_xmlvalue(assembly_status)
    taxid <- XML::xpathSApply(
      rootnode, "//Taxid", XML::xmlValue)
    taxid <- coerce_xmlvalue(taxid)
    coverage <- XML::xpathSApply(
      rootnode, "//Coverage", XML::xmlValue)
    coverage <- coerce_xmlvalue(coverage)
    out <- tibble::tibble(
      assembly = assembly,
      asm_name = asm_name,
      bioproject = bioproject,
      biosample = biosample,
      wgs_project = wgs_project,
      status = assembly_status,
      taxid = taxid,
      coverage = coverage)
    return(out)
  }
  lines <- readLines(file)
  index <- c(0, which(lines == ''))
  if (length(index) == 1) {
    parsed_xml <- list(XML::xmlParse(lines))
  } else {
    parsed_xml <- vector()
    for (i in 2:length(index)) {
      entry <- paste(lines[(index[i-1]+1):(index[i]-1)], collapse = "\n")
      parsed_xml <- c(parsed_xml, XML::xmlParse(entry))
    }
  }
  assembly_meta <- lapply(parsed_xml, foo)
  assembly_meta <- dplyr::bind_rows(assembly_meta)
  return(assembly_meta)
}
