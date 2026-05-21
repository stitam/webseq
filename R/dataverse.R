#' Connect to a dataset on a Dataverse server
#'
#' @param dataset The persistent identifier of the dataset to connect to.
#' @param version The version of the dataset to connect to.
#' @param server The URL of the Dataverse server.
#' @param apikey Your API key for the Dataverse server.
#' @param ... Additional arguments to pass to `dataverse::get_dataset()`.
#' @return A dataset object object of class `dataverse_dataset`.
#' @export
connect_dv_dataset <- function(
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

dv_download <- function(
  files,
  dirpath = NULL,
  verbose = getOption("verbose")
) {
  conn <- assert_dv_files(files)
  
  # Download each file
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
  out <- lapply(files$label, download_file)
  invisible(out)
}

#' @noRd
assert_dv_connection <- function(conn) {
  if (!inherits(conn, "dv_connection")) {
    stop("`conn` must be a `dv_connection` object from `connect_dv_dataset()`.", call. = FALSE)
  }
}

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