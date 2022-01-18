#This could be a general extractor for mgnify.
#Extract content from mgnify_list() outputs.
#extractor code and options depend on what is available -> could be quite complex.
#this currently works when extracting sample data for certain biomes.
mgnify_meta <- function(mgnify_list) {
  out <- lapply(mgnify_list, function(x) {
    kv <- dplyr::bind_rows(x$attributes$`sample-metadata`)
    out <- tibble::tibble(
      id = x$id,
      key = kv$key,
      value = kv$value
    )
    return(out)
  })
  out <- dplyr::bind_rows(out)
  return(out)
}
