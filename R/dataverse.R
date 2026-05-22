#' Connect to a Dataset on a Dataverse server
#'
#' @param dataset character, "persistentId" of the Dataset.
#' @param version character, version of the Dataset.
#' @param server character, URL of the Dataverse server.
#' @param apikey character, your API key for the Dataverse server.
#' @param ... Additional arguments to pass to `dataverse::get_dataset()`.
#' @return A connection object with Dataset metadata.
#' @seealso [dv_list_files()], [dv_download()]
#' @details The persistentId and the server address can be obtained from the URL 
#' of the Dataset. The URL starts with the server address, and the persistentId 
#' is the value of the "persistentId" query parameter. For example, in the URL
#' "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/NCKZD7", the 
#' server address is "https://dataverse.no" and the persistentId is 
#' "doi:10.18710/NCKZD7". 
#' @details If the Dataset is private, you will need to provide your API key to 
#' access it. You can obtain an API key by creating an account on the Dataverse 
#' server and generating a new API token in your account settings.
#' @note The function returns an attribute called "conn" which contains the 
#' connection information you provided when you called the function. If you are 
#' accessing a private Dataset, this attribute will contain your API key!
#' @examples
#' # connect to a public dataset on dataverse.no
#' conn <- dv_connect(
#'   dataset = "doi:10.18710/NCKZD7",
#'   server = "https://dataverse.no"
#' )
#' # connect to a private dataset on dataverse.no
#' conn <- dv_connect(
#'  dataset = "private_persistentId_here",
#'  server = "https://dataverse.no",
#'  apikey = "your_api_key_here"
#' )
#' @export
dv_connect <- function(
  dataset,
  version = ":latest",
  server,
  apikey = ""
) {
  conn <- list(
    persistentId = dataset,
    version      = version,
    server       = server,
    apikey       = apikey
  )
  meta <- dataverse::get_dataset(
    dataset = conn$persistentId,
    version = conn$version,
    server  = conn$server,
    key     = conn$apikey
  )
  structure(meta, conn = conn, class = c("dv_connection", class(meta)))
}

#' List files in a Dataset on a Dataverse server
#'
#' @param conn a `dv_connection` object from [dv_connect()].
#' @return A tibble with information about the files in the Dataset. 
#' @seealso [dv_connect()], [dv_download()]
#' @examples
#' conn <- dv_connect(
#'   dataset = "doi:10.18710/NCKZD7",
#'   server  = "https://dataverse.no"
#' )
#' conn |> dv_list_files()
#' @export
dv_list_files <- function(conn) {
  assert_dv_connection(conn)
  conn$files |> tibble::as_tibble()
}

#' Download files from a Dataset on a Dataverse server
#'
#' @param files a tibble of files from a `dv_connection` object, typically 
#' obtained from [dv_list_files()] plus optional filtering. Must contain columns
#' called `filename`, `id` and `md5`.
#' @param conn a `dv_connection` object from [dv_connect()].
#' @param dirpath character, path to the directory where files will be saved.
#' Defaults to the current working directory.
#' @param verbose logical, should verbose messages be printed to the console?
#' @return A character vector of file paths for successfully downloaded files,
#' or `NA` for files that failed to download. Returned invisibly.
#' @seealso [dv_connect()], [dv_list_files()]
#' @examples
#' conn <- dv_connect(
#'   dataset = "doi:10.18710/NCKZD7",
#'   server  = "https://dataverse.no"
#' )
#' 
#' conn |> 
#'   dv_list_files() |> 
#'   dplyr::filter(grepl("\\.fasta$", filename)) |>
#'   dv_download(conn = conn)
#' @export
dv_download <- function(
  files,
  conn,
  dirpath = NULL,
  verbose = getOption("verbose")
) {
  # validate input
  if (any(!c("filename", "id", "md5") %in% names(files))) {
    stop("'files' must contain columns called 'filename', 'id' and 'md5'.")
  }
  if (any(is.na(files$filename))) {
    stop("'files' contains NA values in the 'filename' column.")
  }
  if (any(is.na(files$id))) {
    stop("'files' contains NA values in the 'id' column.")
  }
  if (any(is.na(files$md5))) {
    warning(
      "'files' contains NA values in the 'md5' column. ",
      "MD5 checksum validation will be skipped for these files."
    )
  }
  conn <- attr(conn, "conn")
  if (is.null(dirpath)) dirpath = getwd()
  if (!dir.exists(dirpath)) {
    if (verbose) message("Creating directory: ", dirpath)
    dir.create(dirpath, recursive = TRUE)
  }
  download_file <- function(i) {
    filepath <- file.path(dirpath, basename(files$filename[i]))
    if (verbose) {
      message("Downloading ", files$filename[i], ". ", appendLF = FALSE)
    }
    if (file.exists(filepath)) {
      if (verbose) message("Already downloaded. ", appendLF = FALSE)
      valid <- validate_checksum(
        md5 = tools::md5sum(filepath), 
        md5ref = files$md5[i], 
        verbose = verbose
      )
      if (valid) {
        return(filepath)
      } else {
        if (verbose) message("Removing and re-downloading. ", appendLF = FALSE)
        file.remove(filepath)
      }
    }
    file_url <- dataverse::get_file_by_id(
      file    = files$id[i],
      dataset = conn$persistentId,
      server  = conn$server,
      key     = conn$apikey,
      return_url = TRUE
    )
    out <- try(
      httr2::request(file_url) |>
      httr2::req_headers("X-Dataverse-key" = conn$apikey) |>
      httr2::req_perform(path = filepath),
      silent = TRUE
    )
    if (!inherits(out, "try-error") && out$status_code == 200) {
      # Validate checksum
      valid <- validate_checksum(
        md5 = tools::md5sum(filepath), 
        md5ref = files$md5[i], 
        verbose = verbose
      )
      if (valid) {
        return(filepath)
      } else {
        if (verbose) message("Removing file.")
        file.remove(filepath)
        return(NA_character_)
      }
    } else if (!inherits(out, "try-error")) {
      if (verbose) message("Failed. ", httr::message_for_status(out))
      return(NA_character_)
    } else {
      if (verbose) message("Failed.")
      return(NA_character_)
    }
  }
  out <- unname(sapply(seq_along(1:nrow(files)), download_file))
  invisible(out)
}

#' Assert valid dv_connection object
#'
#' @param conn Object to validate.
#' @return Invisibly returns `conn` if valid, otherwise stops with an error.
#' @noRd
assert_dv_connection <- function(conn) {
  if (!inherits(conn, "dv_connection")) {
    stop("`conn` must be a `dv_connection` object from `dv_connect()`.", call. = FALSE)
  }
}

validate_checksum <- function(
  md5, 
  md5ref,
  verbose = getOption("verbose")
) {
  if (is.na(md5ref)) {
    if (verbose) {
      message(
        "MD5 checksum reference not available. Skipping validation."
      )
    }
    return(TRUE)
  }
  if (md5 == md5ref) {
    if (verbose) message("Checksum match. ", appendLF = TRUE)
    return(TRUE)
  } else {
    if (verbose) message("Checksum mismatch. ", appendLF = FALSE)
    return(FALSE)
  }
}
