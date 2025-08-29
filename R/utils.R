#' Get package URL
#'
#' Look up the DESCRIPTION file of the package that called this function and
#' return the package URL (the first element of the URL field).
#'
#' @return Either a URL
package_url <- function() {
  # Get package name
  ns <- parent.frame()
  pkg <- utils::packageName(ns)
  if (is.null(pkg)) stop("Could not extract package name.")
  # Extract URL from DESCRIPTION file
  desc <- utils::packageDescription(pkg)
  if (!"URL" %in% names(desc)) stop("Could not extract URL, missing field.")
  urls <- unlist(strsplit(desc[["URL"]], "[[:space:],]+"))
  urls <- urls[nzchar(urls)]
  if (length(urls) == 0) stop("Could not extract URL, empty field.")
  return(urls[1])
}

#' Retry a function call
#' 
#' @param expr expression; Expression to evaluate.
#' @param times integer; Number of times to retry.
#' @noRd
retry <- function(expr, times = 5) {
  if (!is.numeric(times) || times < 1) stop("times must be a positive integer.")
  attempt <- 1
  while (attempt <= times) {
    result <- try(expr, silent = TRUE)
    if (inherits(result, "try-error")) {
      Sys.sleep(attempt ^ 2)
      attempt <- attempt + 1
    } else {
      return(result)
    }
  }
  stop()
}

#' Try an URL with default parameters
#' 
#' This is a convenience wrapper for trying URLs
#' @param verb character; Name of the verb to use.
#' @param qurl character; Query URL.
#' @importFrom httr RETRY
#' @noRd
try_url <- function(verb, qurl, ...){
  try(httr::RETRY(verb,
                  qurl,
                  terminate_on = 404,
                  quiet = TRUE,
                  ...), silent = TRUE)
}

webseq_message <- function(action = c("na",
                                     "query",
                                     "query_all",
                                     "not_found",
                                     "not_available",
                                     "service_down"),
                            appendLF = TRUE,
                            ...) {
  action <- match.arg(action)
  string <- switch(
    action,
    na = "Query is NA. Returning NA.",
    query = paste0("Querying ", ..., ". "),
    query_all = "Querying. ",
    not_found = "Not found. Returning NA.",
    not_available = "Not available. Returning NA.",
    service_down = "Service not available. Returning NA."
  )
  message(string, appendLF = FALSE)
  if (appendLF) message("")
}

#' Function to wait between every web-service query
#'
#' @param time numeric; Wait time in seconds.
#' @param type character; Will be an API queried or a website scraped?
#' @noRd
#'
webseq_sleep <- function(time = NULL, type = c("API", "FTP", "scrape")) {
  type <- match.arg(type)
  if (is.null(time)) {
    if (type == 'API') {
      time <- 0.2
    }
    if (type == "FTP") {
      time <- 0.1
    }
    if (type == 'scrape') {
      time <- 0.3
    }
  } else {
    if (!is.numeric(time)) {
      stop('Set time is not numeric.')
    }
  }
  Sys.sleep(time)
}

#' Wrap web-service queries
#' 
#' @param function_name character; Name of the function to wrap.
#' @param package character; Name of the package in which the function is.
#' @param verbose logical; Should verbose messages be printed to console?
#' @param ... arguments to pass to the function.
#' @noRd
wrap <- function(
  function_name, 
  package, 
  verbose = getOption("verbose"), 
  ...
  ) {
  f <- get(function_name, envir = asNamespace(package))
  r <- NULL
  attempt <- 1
  while(is.null(r) && attempt <= 5) {
    webseq_sleep(type = "API")
    hit <- try(
      f(...),
      silent = TRUE
    )
    if (inherits(hit, "try-error")) {
      attempt <- attempt + 1
    } else r <- 1
  }
  if (inherits(hit, "try-error")) {
    if (verbose) webseq_message("service_down")
    return(NA)
  } else {
    if (verbose) {
      message("Query successful.")
    }
    return(hit)
  }
}

#' Split a vector of IDs into batches
#' 
#' @param ids character; Vector of IDs.
#' @param batch_size integer; Number of IDs per batch.
#' @param verbose logical; Should verbose messages be printed to console?
#' @noRd
get_idlist <- function(ids, batch_size, verbose = getOption("verbose")) {
  ids <- suppressWarnings(as.numeric(ids))
  if (all(is.na(ids))) {
    if (verbose) message("No valid UIDs.")
    ids <- NA
  } else if (any(is.na(ids))){
    if (verbose) message("Removing NA-s from UIDs.")
    ids <- ids[which(!is.na(ids))]
  }
  idlist <- list()
  if (length(ids) > batch_size) {
    nbatch <- ceiling(length(ids)/batch_size)
    if (verbose) message("Splitting ids into ", nbatch, " batches.")
    for (i in 1:nbatch) {
      index_min <- (i-1)*batch_size + 1
      index_max <- min(i*batch_size, length(ids))
      idlist[[i]] <- ids[index_min:index_max]
    }
  } else {
    idlist[[1]] <- ids
  }
  return(idlist)
}

#' Validate the structure of a webseq object
#' 
#' @param x a webseq object
#' @noRd
validate_webseq_class <- function(x) {
  if ("ncbi_uid" %in% class(x)) {
    testthat::expect_equal(length(names(x)), 3)
    testthat::expect_true(names(x)[1] == "uid")
    testthat::expect_true(names(x)[2] == "db")
    testthat::expect_true(names(x)[3] == "web_history")
    testthat::expect_true(class(x$uid) == "numeric")
    testthat::expect_true(class(x$db) == "character")
    testthat::expect_s3_class(x$web_history, c("tbl_df", "tbl", "data.frame"))
  }
  if ("ncbi_meta" %in% class(x)) {
    testthat::expect_true(attr(x, "db") %in% ncbi_dbs())
  }
}

#' Get number of processor cores
#' 
#' @param mc_cores integer; Number of cores to use for parallel processing.
#' @param verbose logical; Should verbose messages be printed to console?
#' @noRd
get_mc_cores <- function(mc_cores, verbose = getOption("verbose")) {
  n_cores <- parallel::detectCores()
  if (is.na(n_cores)) stop("Could not detect number of cores.")
  if (is.null(mc_cores)) {
    if (verbose) message(
      "Number of cores not specified. Looking at 'Ncpu' option."
    )
    mc_cores <- getOption("Ncpu")
    if (!is.null(mc_cores)) {
      if (verbose) message("Found.")
    } else {
      if (verbose) message(
        "Not found. Attempting to use all but one cores (at least 1)."
      )
      mc_cores <- max(n_cores - 1, 1)
    }
  }
  mc_cores <- suppressWarnings(as.integer(mc_cores))
  if (is.na(mc_cores)) {
    stop("Number of cores must be an integer, or coercible to an integer.")
  }
  if (mc_cores < 1) {
    stop("Number of cores must be at least 1.")
  }
  if (mc_cores > n_cores) {
    if (verbose) message(
      "Number of cores specified is greater than the number of available ",
      "cores. Using all available cores."
    )
    mc_cores <- n_cores
  }
  return(mc_cores)
}

#' Convert a vector to numeric
#' 
#' @param x vector; Vector to convert.
#' @noRd
as_numeric <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(x))
  if (any(is.na(numeric_x) & !is.na(x))) {
    stop("Query must be either an ncbi_uid object or a vector of UIDs.")
  }
  return(numeric_x)
}

#' Validate NCBI UIDs
#' 
#' NCBI UIDs must be numeric vectors with no NAs.
#' @param x a vector
#' @noRd
validate_ncbi_uid <- function(x) {
    if (!inherits(x, c("numeric", "integer"))) {
    stop("'uid' must be a numeric vector of NCBI UIDs.")
  }
  uid_integer <- suppressWarnings(as.integer(x))
  index <- which(is.na(uid_integer) & !is.na(x))
  if (length(index) > 0) {
    msg <- paste0(
      "The following elements are not valid UIDs: ",
      x[index],
      collapse = ", "
    )
    stop(msg)
  }
  index <- which(is.na(x))
  if (length(index) > 0) {
    if (verbose) message("Removing NA-s. ", appendLF = FALSE)
    x <- x[which(!is.na(x))]
    if (verbose) message(paste0(length(x), " elements remain."))
  }
  if (length(x) == 0) stop("No valid UIDs.")
  return(x)
}
