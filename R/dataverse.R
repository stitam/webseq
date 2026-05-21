#' Connect to a Dataset on a Dataverse server
#'
#' @param dataset character, "persistentId" of the Dataset to connect to.
#' @param version character, version of the Dataset to connect to.
#' @param server character, URL of the Dataverse server.
#' @param apikey character, your API key for the Dataverse server.
#' @param ... Additional arguments to pass to `dataverse::get_dataset()`.
#' @return A dataset object object of class `dataverse_dataset`.
#' @details
#' The persistentId and the server address can be obtained from the URL of the 
#' Dataset. The URL starts with the server address, and the persistentId is the 
#' value of the "persistentId" query parameter. For example, in the URL
#' "https://dataverse.no/dataset.xhtml?persistentId=doi:10.18710/NCKZD7", the 
#' server address is "https://dataverse.no" and the persistentId is 
#' "doi:10.18710/NCKZD7". If the Dataset is private, you will need to provide 
#' your API key to access it. You can obtain an API key by creating an account
#' on the Dataverse server and generating a new API token in your account 
#' settings.
#' @examples
#' # connect to a public dataset on dataverse.no
#' conn <- dv_connect_dataset(
#'   dataset = "doi:10.18710/NCKZD7",
#'   server = "https://dataverse.no"
#' )
#' # connect to a private dataset on dataverse.no
#' conn <- dv_connect_dataset(
#'  dataset = "private_persistentId_here",
#'  server = "https://dataverse.no",
#'  apikey = "your_api_key_here"
#' )
#' @export
dv_connect_dataset <- function(
  dataset,
  version = ":latest",
  server,
  apikey = ""
) {
  structure(
    list(
      persistentId = dataset,
      version      = version,
      server       = server,
      apikey       = apikey
    ),
    class = "dv_connection"
  )
}


#' List files in a Dataset of a Dataverse server
#'
#' @param conn a `dv_connection` object from [dv_connect_dataset()].
#' @param extension character, optional file extension to filter by 
#' (`"fasta"`, `"fastq"`, etc.).
#' @return A tibble with information about the files in the Dataset. 
#' @note The returned tibbel is not a simple tibble, it is an object of class 
#' `dv_files` where the connection object is stored as an attribute of the 
#' tibble. This allows piping the output of `dv_list_files()` directly into 
#' `dv_download()`.
#' @seealso [dv_connect_dataset()], [dv_download()]
#' @examples
#' conn <- dv_connect_dataset(
#'   dataset = "doi:10.18710/NCKZD7",
#'   server  = "https://dataverse.no"
#' )
#' conn |> dv_list_files(extension = "fasta")
#' @export
dv_list_files <- function(conn, extension = NULL) {
  assert_dv_connection(conn)
  dataset <- dataverse::get_dataset(
    dataset = conn$persistentId,
    version = conn$version,
    server  = conn$server,
    key     = conn$apikey
  )
  files <- dataset$files |>
    dplyr::select(
      "label",
      "description",
      "filename",
      "filesize",
      "md5"
    )
  if (!is.null(extension)) {
    ext <- ifelse(startsWith(extension, "."), extension, paste0(".", extension))
    index <- tolower(tools::file_ext(files$filename)) == tolower(sub("^\\.", "", ext))
    files <- files[index, ]
  }
  files_tbl <- tibble::as_tibble(files)
  structure(files_tbl, conn = conn, class = c("dv_files", class(files_tbl)))
}


#' Download files from a Dataset of a Dataverse server
#'
#' @param files a `dv_files` object from [dv_list_files()].
#' @param dirpath character, path to the directory where files will be saved.
#' Defaults to the current working directory.
#' @param verbose logical, should verbose messages be printed to the console?
#' @return A character vector of file paths for successfully downloaded files,
#' or `NA` for files that failed to download. Returned invisibly.
#' @seealso [dv_connect_dataset()], [dv_list_files()]
#' @examples
#' conn <- dv_connect_dataset(
#'   dataset = "doi:10.18710/NCKZD7",
#'   server  = "https://dataverse.no"
#' )
#' conn |> dv_list_files(extension = "fasta") |> dv_download()
#' @export
dv_download <- function(
  files,
  dirpath = NULL,
  verbose = getOption("verbose")
) {
  conn <- assert_dv_files(files)
  download_file <- function(filename) {
    file_url <- dataverse::get_file(
      file    = filename,
      dataset = conn$persistentId,
      server  = conn$server,
      key     = conn$apikey,
      return_url = TRUE
    )
    if (is.null(dirpath)) dirpath = getwd()
    if (!dir.exists(dirpath)) {
      dir.create(dirpath, recursive = TRUE)
    }
    filepath <- file.path(dirpath, basename(filename))
    out <- try(
      utils::download.file(file_url, destfile = filepath, quiet = TRUE),
      silent = TRUE
    )
    if (inherits(out, "try-error")) {
      if (verbose) message("Failed to download ", filename, ". Webservice temporarily down.")
      file.remove(filepath)
      return(NA_character_)
    }
    if (verbose) message("Downloaded ", filename)
    return(filepath)
  }
  out <- unname(sapply(files$label, download_file))
  invisible(out)
}


#' Assert valid dv_connection object
#'
#' @param conn Object to validate.
#' @return Invisibly returns `conn` if valid, otherwise stops with an error.
#' @noRd
assert_dv_connection <- function(conn) {
  if (!inherits(conn, "dv_connection")) {
    stop("`conn` must be a `dv_connection` object from `dv_connect_dataset()`.", call. = FALSE)
  }
}

#' Assert valid dv_files object and extract connection
#'
#' @param x Object to validate.
#' @return The connection object stored as an attribute of `x`.
#' @noRd
assert_dv_files <- function(x) {
  if (!inherits(x, "dv_files") || is.null(attr(x, "conn"))) {
    stop(
      "`files` must be a `dv_files` object from `dv_list_files()`.",
      call. = FALSE
    )
  }
  attr(x, "conn")
}