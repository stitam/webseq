#' Parse NCBI BioSample metadata
#' 
#' BioSample metadata from NCBI can be retrieved in multiple file formats. This
#' function parses metadata retrieved in XML format.
#' @param biosample_xml character; either a character vector containing an xml
#' that was retrieved through \code{rentrez::entrez_fetch()} or a path to an xml
#' file that was downloaded from NCBI BioSample.
#' @param verbose logical; Should verbose messages be printed to console?
ncbi_meta_biosample_xml <- function(
    biosample_xml,
    verbose = getOption("verbose")
    ) {
  parsed_xml <- xml2::as_list(xml2::read_xml(biosample_xml))[[1]]
  names(parsed_xml) <- sapply(parsed_xml, function(x) attributes(x)$accession)
  out <- lapply(parsed_xml, ncbi_meta_biosample_xml_entry)
  out <- dplyr::bind_rows(out)
  out <- out[, c(
    "biosample_uid",
    "biosample",
    names(out)[3:ncol(out)][order(names(out)[3:ncol(out)])]
  )]
  out <- tibble::as_tibble(out)
  return(out)
}

ncbi_meta_biosample_xml_entry <- function(x, verbose = getOption("verbose")) {
  # attributes(x)$names contains all fields! use for validation
  main_attrs <- attributes(x)
  expected_names <- c(
    "names", "last_update", "publication_date",
    "access", "submission_date", "id", "accession"
  )
  if (any(!expected_names %in% names(main_attrs))) {
    msg <- paste0("Could not extract Attributes for BioSample ", biosample, ".")
    stop(msg)
  }
  out <- dplyr::bind_cols(
    # primary key
    data.frame(
      biosample_uid = main_attrs$id
    ),
    # ids
    extract_ids(x, biosample, verbose),
    # title and organism
    data.frame(
      title = extract_title(x, biosample, verbose),
      organism = extract_organism(x, biosample, verbose),
      taxid = extract_taxid(x, biosample, verbose)
    ),
    # package,
    extract_package(x, biosample, verbose),
    # attributes
    extract_attributes(x, biosample, verbose),
    extract_owner(x, biosample, verbose),
    # description
    data.frame(
      description = extract_description(x, biosample, verbose)
    ),
    data.frame(
      bioproject = extract_bioproject(x, biosample, verbose),
      access = main_attrs$access,
      status = extract_status(x, biosample, verbose),
      submission_date = main_attrs$submission_date,
      publication_date = main_attrs$publication_date,
      last_update = main_attrs$last_update,
      status_date = extract_status_date(x, biosample, verbose)
    ),
  )
  return(out)
}

extract_ids <- function(
    x,
    biosample,
    verbose = getOption("verbose")) {
  out <- NA
  res <- x$Ids
  if (!is.null(res)) {
    out <- data.frame(
      db = unname(sapply(res, function(A) {
        # check if this exists?
        attributes(A)$db
      })),
      id = unname(sapply(res, unlist))
    )
    out <- dplyr::distinct(out)
    out <- tidyr::spread(out, db, id)
    names(out) <- gsub(" +", "_", tolower(names(out)))
  }
  if (length(out) == 1 && is.na(out) && verbose) {
    msg <- paste0(
      "Could not extract Ids for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_attributes <- function(x, biosample, verbose = getOption("verbose")) {
  out <- NA
  res <- x$Attributes
  if (!is.null(res)) {
    out <- data.frame(
      attr = unname(sapply(res, function(A) {
        paste0("attr_", attributes(A)$harmonized_name)
      })),
      value = unname(sapply(res, unlist))
    )
    out <- dplyr::distinct(out)
    out <- tidyr::spread(out, attr, value)
  }
  if (length(out) == 1 && is.na(out) && verbose) {
    msg <- paste0(
      "Could not extract Attributes for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_owner <- function(
    x,
    biosample,
    verbose = getOption("verbose")) {
  out <- NA
  res <- x$Owner
  if (!is.null(res)) {
    out <- data.frame(
      owner_name = unlist(res$Name),
      contact_name_first = if("Contacts" %in% names(res)) {
        unlist(res$Contacts$Contact$Name$First)
      } else {
        NA
      },
      contact_name_last = if("Contacts" %in% names(res)) {
        unlist(res$Contacts$Contact$Name$Last)
      } else {
        NA
      },
      contact_email = if (
        "Contacts" %in% names(res) &&
        "email" %in% names(attributes(res$Contacts$Contact))
      ) {
        attributes(res$Contacts$Contact)$email
      } else {
        NA
      }
    )
    out <- dplyr::distinct(out)
  }
  if (length(out) == 1 && is.na(out) && verbose) {
    msg <- paste0(
      "Could not extract Owner info for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_description <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  out <- NA
  if (!is.null(x$Description$Comment$Paragraph)) {
    out <- unlist(x$Description$Comment$Paragraph)
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract Description for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

# test <- get_uid("pathogen cl 1 0[filter]", "biosample")
extract_package <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  out <- NA
  if (!is.null(x$Package)) {
    out <- data.frame(
      package_name = attributes(x$Package)$display_name,
      package_searchterm = unlist(x$Package)
    )
  }
  if (length(out) == 1 && is.na(out) && verbose) {
    msg <- paste0(
      "Could not extract Package for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_organism <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  out <- NA
  res1 <- try(unique(unlist(x$Description$Organism$OrganismName)), silent = TRUE)
  res2 <- try(attributes(x$Description$Organism)$taxonomy_name, silent = TRUE)
  if (!inherits(res1, "try-error") && 
      length(res1) == 1 &&
      inherits(res2, "try-error")) {
    out <- res1
  } else if (!inherits(res2, "try-error") && 
      length(res2) == 1 &&
      inherits(res1, "try-error")) {
    out <- res2
  } else if (!inherits(res1, "try-error") &&
             !inherits(res2, "try-error") &&
             length(res1) == 1 &&
             length(res2) == 1 &&
             res1 == res2) {
    out <- res1
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract Organism for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_bioproject <- function(
    x,
    biosample, 
    verbose = getOption("verbose")
  ) {
  out <- NA
  res <- attributes(x[["Links"]][["Link"]])
  if ("type" %in% names(res) && 
      length(res$type) == 1 && 
      res$type == "entrez") {
    if ("target" %in% names(res) && 
        length(res$target) == 1 && 
        res$target == "bioproject") {
      if ("label" %in% names(res) &&
          length(res$label) == 1) {
        out <- res$label
      }
    }
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract BioProject for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_status <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  out <- NA
  res <- try(attributes(x$Status), silent = TRUE)
  if (!inherits(res, "try-error") && "status" %in% names(attributes(x$Status))) {
    out <- attributes(x$Status)$status
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract status for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_status_date <- function(
    x,
    biosample,
    verbose = getOption("verbose")
) {
  out <- NA
  res <- try(attributes(x$Status), silent = TRUE)
  if (!inherits(res, "try-error") && "status" %in% names(attributes(x$Status))) {
    out <- attributes(x$Status)$when
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract status_date  for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_taxid <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  out <- NA
  res <- try(attributes(x$Description$Organism), silent = TRUE)
  if (!inherits(res, "try-error") && "taxonomy_id" %in% names(res)) {
    out <- res$taxonomy_id
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract taxonomy_id for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}

extract_title <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  out <- NA
  res <- try(unique(unlist(x$Description$Title)), silent = TRUE)
  if (!inherits(res, "try-error")) {
    if (length(res) == 1) {
      out <- res
    }
  }
  if (is.na(out) & verbose) {
    msg <- paste0(
      "Could not extract Title for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}
