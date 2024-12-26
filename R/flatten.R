#' Bind rows in a list of list of data frames
#' 
#' This is a utility function that converts a list of lists, each containing
#' a names list of data frames into a single flat list of data frames.
#' @param x list; a list which contains a named list of data frames
#' @return a flat list of data frames where lists of data frames with the same 
#' name are merged.
flatten <- function(x) {
  is_named_list <- function(x) {
    !is.null(names(x)) && all(names(x) != "")
  }
  #if (!is_named_list(x)) {
  #  stop("'x' must be a named list.")
  #}
  if (any(!sapply(x, is_named_list))) {
    stop("Each element of 'x' must be a named list.")
  }
  df_names <- lapply(x, names) |> unlist() |> unique()
  out <- setNames(lapply(df_names, function(x) tibble::tibble()), df_names)
  for (i in seq_along(out)) {
    df_name <- names(out)[i]
    for (j in seq_along(x)) {
      if (df_name %in% names(x[[j]])) {
        out[[i]] <- dplyr::bind_rows(
          out[[i]],
          x[[j]][[names(out)[i]]]
        )
      } else {
        next()
      }
    }
  }
  return(out)
}