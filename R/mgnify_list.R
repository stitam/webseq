#' Retrieve a list of instances from MGnify
#' 
#' This function retrieves a list of identifiers to look up with other
#' functions.
#' @param query character; what to look for.
#' @param from character; API. See \code{"mgnify_endpoints()"}.
#' @param from_id character; more precise filtering for the API.
#' @param page numeric; the API's response is paginated this tells the API which
#' page to return. If \code{NULL}, the function will return all pages.
#' @param sleep character; number of seconds to sleep before requesting the next
#' page.
#' @param verbose logical; should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' # Query samples collected from biogas plants
#' mgnify_list(query = "samples",
#'             from = "biomes",
#'             from_id = "root:Engineered:Biogas plant",
#'             page = 1)
#' }
#' @export
mgnify_list <- function(query,
                        from,
                        from_id,
                        page = NULL,
                        sleep = 0.2,
                        verbose = getOption("verbose")) {
  endpoints <- mgnify_endpoints(verbose = verbose)
  query <- match.arg(query, endpoints$api)
  from <- match.arg(from, endpoints$api)
  from_id <- utils::URLencode(from_id)
  url <- paste(
    "https://www.ebi.ac.uk/metagenomics/api/v1", from, from_id, query, sep = "/"
    )
  if(is.null(page)) {
    init <- httr::GET(url)
    init_content <- httr::content(init)
    lastpage <- strsplit(init_content$links$last, "?page=")[[1]][2]
    if (verbose) message("number of pages: ", lastpage)
    page <- 1:lastpage
  }
  foo <- function(url, page) {
    if (verbose) message("page: ", page)
    sleep <- eval(sleep)
    if (length(sleep) == 1 && sleep > 0) {
      Sys.sleep(eval(sleep))
    } else (
      stop(paste0("Argument 'sleep' is incorrectly defined. ",
                  "See documentation for more details."))
    )
    pageurl <- paste0(url, "?page=", page)
    res <- httr::GET(pageurl)
    cont <- httr::content(res)
    return(cont$data)
  }
  out <- unlist(lapply(page, function(x) foo(url, x)), recursive = FALSE)
  return(out)
}
