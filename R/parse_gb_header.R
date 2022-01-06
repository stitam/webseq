#' Parse headers from GenBank files
#'
#' @param file character;
#' @param dir character;
#' @param outfile character;
#' @param errorfile character;
#' @param batch_size integer;
#' @param verbose logical;
#' @export
parse_gb_header <- function(file,
                            dir = getwd(),
                            outfile = "./cache/annotation_headers.rda",
                            errorfile = "./cache/annotation_headers_parse_error.rda",
                            batch_size = 10,
                            verbose = getOption("verbose")) {
  # todo: harmonise params with parse_gb_body, parse_report
  df <- data.frame(file = character())
  nbatch <- ceiling(length(file)/batch_size)
  for (i in 1:nbatch) {
    if (i < nbatch) sf <- file[(batch_size * (i - 1) + 1):(batch_size * i)]
    else sf <- file[(batch_size * (i - 1) + 1):length(file)]
    for (j in seq_along(sf)) {
      filepath = paste0(dir, "/", sf[j])
      gbp <- try(suppressMessages(genbankr::parseGenBank(filepath)),
                 silent = TRUE)
      acc <- try(unname(gbp$VERSION[1]), silent = TRUE)
      def <- try(gbp$DEFINITION, silent = TRUE)
      loc <- try(strsplit(gbp$LOCUS, " +")[[1]], silent = TRUE)
      if ("try-error" %in% c(class(acc),
                             class(def),
                             class(loc))) {
        if (verbose) message(paste0("Could not parse file: ", sf[j]))
        newdf <- data.frame(file = sf[j], comment = "parse-error")
        df <- plyr::join(df, newdf, by = "file", type = "full")
        next()
      }
      t <- readLines(filepath)
      host <- t[grepl("host=", t)]
      host <- trimws(host)
      host <- host[grepl("lab_host", host) == FALSE]
      host <- gsub("/host=", "", host)
      host <- gsub('\\"', "", host)
      host <- ifelse(length(host) == 0, NA, host)
      newdf <- try(data.frame(file = sf[j],
                              accession = acc,
                              definition = def,
                              length = as.numeric(loc[3]),
                              nucleic_acid = loc[5],
                              type = loc[6],
                              host = host,
                              comment = NA), silent = TRUE)
      if(inherits(newdf, "try-error")) {
        if (verbose) message(paste0("Could not merge file: ", sf[j]))
        newdf <- data.frame(file = sf[j], comment = "merge-error")
      }
      df <- plyr::join(df, newdf, by = "file", type = "full")
    }
    if(nrow(df)>0) {
      annotation_headers <- df[is.na(df$comment),]
      annotation_headers_parse_error <- df[!is.na(df$comment),]
      save(annotation_headers, file = outfile)
      save(annotation_headers_parse_error, file = errorfile)
    }
  }
  return(annotation_headers)
}
