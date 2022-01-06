#' Retrieve a list of instances from MGnify
#' @examples 
#' \dontrun{
#' # Query samples collected from biogas plants
#' mgnify_list(query = "samples",
#'             from = "biomes"
#'             from_id = "root:Engineered:Biogas plant",
#'             page = 1)
#' }
#' @export
mgnify_list <- function(query,
                        from,
                        from_id,
                        page = NULL) {
  query <- match.arg(query, mgnify_endpoints()$api)
  from <- match.arg(from, mgnify_endpoints()$api)
  from_id <- URLencode(from_id)
  url <- paste("https://www.ebi.ac.uk/metagenomics/api/v1",
               from,
               from_id,
               query,
               sep = "/")
  foo <- function(url, page) {
    Sys.sleep(0.2)
    pageurl <- paste0(url, "?page=", page)
    res <- httr::GET(pageurl)
    cont <- httr::content(res)
    #todo: add content specific extractor
    return(cont$data)
  }
  if(is.null(page)) {
    init <- httr::GET(url)
    init_content <- httr::content(init)
    lastpage <- strsplit(init_content$links$last, "?page=")[[1]][2]
    page <- 1:lastpage
  }
  out <- unlist(lapply(page, function(x) foo(url, x)), recursive = FALSE)
  return(out)
}

