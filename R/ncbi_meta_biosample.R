#' Collect NCBI BioSample metadata
#' 
#' This function collects metadata for entries in NCBI BioSample database.
#' @importFrom rentrez entrez_fetch
#' @importFrom XML xmlParse xmlRoot xmlToList
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble as_tibble 
#' @param biosample_uids character; a character vector of BioSample UID-s.
#' @param verbose logical; Should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' # Sample metadata from BioSample ID
#' biosample_uid <- ncbi_get_uid("SAMN02714232", db = "biosample")
#' ncbi_meta(biosample_uid)
#' 
#' # Sample metadata from assembly accession 
#' assembly_uid <- ncbi_get_uid("GCF_000695855.3")
#' biosample_uid <- link_uids(assembly_uid, from = "assembly", to = "biosample")
#' ncbi_meta_biosample(biosample_uid)
#' }
#' @export
ncbi_meta_biosample <- function(biosample_uids, verbose = getOption("verbose")) {
 foo <- function(x) {
   res <- rentrez::entrez_fetch(
     db = "biosample", id = x, rettype = "full", retmode = "xml")
   parsed_xml <- XML::xmlParse(res)
   rootnode <- XML::xmlRoot(parsed_xml)
   out <- data.frame(biosample_uid = x)
   attr_node <- rootnode[[1]][["Attributes"]]
   if (!is.null(attr_node)) {
     attr_list <- XML::xmlToList(attr_node)
     for (i in 1:length(attr_list)) {
       attrib <- data.frame(attr_list[[i]][["text"]]) 
       names(attrib) <- attr_list[[i]][[".attrs"]]["display_name"]
       names(attrib) <- tolower(names(attrib))
       names(attrib) <- gsub(" +", "_", names(attrib))
       out <- dplyr::bind_cols(out, attrib)
     }  
   }
   owner_node <- rootnode[[1]][["Owner"]]
   if (!is.null(owner_node)) {
     owner_list <- XML::xmlToList(owner_node)
     if (!is.null(owner_list$Contacts$Contact$.attrs)) {
       if ("email" %in% names(owner_list$Contacts$Contact$.attrs)) {
         out$owner_name <- ifelse(!is.null(owner_list$Name),
                                  owner_list$Name,
                                  NA)
         out$owner_firstname <- ifelse(
           !is.null(owner_list$Contacts$Contact$Name$First),
           owner_list$Contacts$Contact$Name$First,
           NA)
         out$owner_lastname <- ifelse(
           !is.null(owner_list$Contacts$Contact$Name$Last),
           owner_list$Contacts$Contact$Name$Last,
           NA)
         index <- which(names(owner_list$Contacts$Contact$.attrs) == "email")
         out$owner_email <- owner_list$Contacts$Contact$.attrs[index]
       }
     }
   }
   return(out)
 }
 out <- lapply(biosample_uids, foo)
 out <- dplyr::bind_rows(out)
 out <- tibble::as_tibble(out)
 return(out)
}
