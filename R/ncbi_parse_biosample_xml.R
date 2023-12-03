#' Parse NCBI BioSample metadata
#' 
#' BioSample metadata from NCBI can be retrieved in multiple file formats. This
#' function parses metadata retrieved in XML format.
#' @param biosample_xml character; either a character vector containing an xml
#' that was retrieved through \code{rentrez::entrez_fetch()} or a path to an xml
#' file that was downloaded from NCBI BioSample.
#' @param verbose logical; Should verbose messages be printed to console?
ncbi_parse_biosample_xml <- function(
    biosample_xml,
    verbose = getOption("verbose")
    ) {
  parsed_xml <- lapply(biosample_xml, function(x) {
    xml2::as_list(xml2::read_xml(x))[[1]]
  })
  parsed_xml <- unlist(parsed_xml, recursive = FALSE)
  names(parsed_xml) <- sapply(parsed_xml, function(x) attributes(x)$accession)
  out <- lapply(parsed_xml, ncbi_parse_biosample_xml_entry)
  out <- dplyr::bind_rows(out)
  out <- out[, c(
    "biosample_uid",
    "biosample",
    names(out)[3:ncol(out)][order(names(out)[3:ncol(out)])]
  )]
  out <- tibble::as_tibble(out)
  return(out)
}

ncbi_parse_biosample_xml_entry <- function(x, verbose = getOption("verbose")) {
  # attributes(x)$names contains all fields!
  # only extract elements that are listed there.
  main_attrs <- attributes(x)
  expected_names <- c(
    "names", "last_update", "publication_date",
    "access", "submission_date", "id", "accession"
  )
  if (any(!expected_names %in% names(main_attrs))) {
    msg <- paste0(
      "Could not extract main Attributes for BioSample ",
      main_attrs$accession,
      "."
    )
    stop(msg)
  }
  out <- dplyr::bind_cols(
    # primary key
    data.frame(
      biosample_uid = main_attrs$id
    ),
    # ids
    extract_ids(x, main_attrs$accession, verbose),
    # title and organism
    data.frame(
      title = extract_title(x, main_attrs$accession, verbose),
      organism = extract_organism(x, main_attrs$accession, verbose),
      taxid = extract_taxid(x, main_attrs$accession, verbose)
    ),
    # package,
    extract_package(x, main_attrs$accession, verbose),
    # attributes
    extract_attributes(x, main_attrs$accession, verbose),
    extract_owner(x, main_attrs$accession, verbose),
    # description
    data.frame(
      description = extract_description(x, main_attrs$accession, verbose)
    ),
    extract_links(x, main_attrs$accession, verbose),
    data.frame(
      access = main_attrs$access,
      status = extract_status(x, main_attrs$accession, verbose),
      submission_date = main_attrs$submission_date,
      publication_date = main_attrs$publication_date,
      last_update = main_attrs$last_update,
      status_date = extract_status_date(x, main_attrs$accession, verbose)
    )
  )
  if (nrow(out) > 1) {
    msg <- paste0(
      "Multiple parsed rows for BioSample ", main_attrs$accession, "."
    )
    warning(msg)
  }
  return(out)
}

extract_ids <- function(
    x,
    biosample,
    verbose = getOption("verbose")) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- x$Ids
    out <- data.frame(
      db = unname(sapply(res, function(A) {
        attributes(A)$db
      })),
      id = unname(sapply(res, unlist))
    )
    out <- dplyr::distinct(out)
    out <- tidyr::spread(out, db, id)
    names(out) <- gsub(" +", "_", tolower(names(out)))
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract IDs for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_attributes <- function(x, biosample, verbose = getOption("verbose")) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- x$Attributes
    out <- data.frame(
      attr = unname(sapply(res, function(A) {
        attr_instance <- attributes(A)
        if (!is.null(attr_instance$harmonized_name)) {
          attr_name <- attr_instance$harmonized_name
        } else if (
          length(attr_instance) == 1 && 
          names(attr_instance) == "attribute_name") {
          attr_name <- attr_instance$attribute_name
        }
        paste0("attr_", attr_name)
      })),
      value = unname(sapply(res, unlist))
    )
    out <- dplyr::distinct(out)
    out <- tidyr::spread(out, attr, value)
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract Attributes for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_owner <- function(
    x,
    biosample,
    verbose = getOption("verbose")) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- x$Owner
    out <- data.frame(
      owner_name = unlist(res$Name),
      owner_abbreviation = if ("abbreviation" %in% names(attributes(res$Name))) {
        attributes(res$Name)$abbreviation
      } else {
        NA_character_
      },
      owner_url = if ("url" %in% names(attributes(res$Name))) {
        attributes(res$Name)$url
      } else {
        NA_character_
      },
      contact_name_first = if("Contacts" %in% names(res)) {
        unlist(res$Contacts$Contact$Name$First)
      } else {
        NA_character_
      },
      contact_name_last = if("Contacts" %in% names(res)) {
        unlist(res$Contacts$Contact$Name$Last)
      } else {
        NA_character_
      },
      contact_email = if (
        "Contacts" %in% names(res) &&
        "email" %in% names(attributes(res$Contacts$Contact))
      ) {
        attributes(res$Contacts$Contact)$email
      } else {
        NA_character_
      }
    )
    out <- dplyr::distinct(out)
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract Owner info for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_description <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    if ("Comment" %in% names(x$Description)) {
      out <- unlist(x$Description$Comment$Paragraph)
    } else {
      out <- NA
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0(
      "Could not extract Description for BioSample ", biosample, "."
    )
    stop(msg)
  }
  return(out)
}

# test <- ncbi_get_uid("pathogen cl 1 0[filter]", "biosample")
extract_package <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    out <- data.frame(
      package_name = attributes(x$Package)$display_name,
      package_searchterm = unlist(x$Package)
    )
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract Package for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_organism <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res1 <- try(
      unique(unlist(x$Description$Organism$OrganismName)), silent = TRUE)
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
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract Organism for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_links <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- x[["Links"]]
    if (is.null(res)) {
      return(data.frame())
    } else {
      linklist <- lapply(res, function(A) {
        longlink <- data.frame(
          target = attributes(A)$target,
          label = if("label" %in% names(attributes(A))) {
            attributes(A)$label
          } else {
            unlist(A)
          }
        )
        widelink <- tidyr::pivot_wider(
          data = longlink,
          names_from = target,
          values_from = label
        )
        return(widelink)
      })
      out <- dplyr::bind_cols(linklist)
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract BioSample links for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_status <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- try(attributes(x$Status), silent = TRUE)
    if (!inherits(res, "try-error") && "status" %in% names(attributes(x$Status))) {
      out <- attributes(x$Status)$status
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract status for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}

extract_status_date <- function(
    x,
    biosample,
    verbose = getOption("verbose")
) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- try(attributes(x$Status), silent = TRUE)
    if (!inherits(res, "try-error") && "status" %in% names(attributes(x$Status))) {
      out <- attributes(x$Status)$when
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0(
      "Could not extract status_date  for BioSample ", biosample, "."
    )
    stop(msg)
  }
  return(out)
}

extract_taxid <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- try(attributes(x$Description$Organism), silent = TRUE)
    if (!inherits(res, "try-error") && "taxonomy_id" %in% names(res)) {
      out <- res$taxonomy_id
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0(
      "Could not extract taxonomy_id for BioSample ", biosample, "."
    )
    stop(msg)
  }
  return(out)
}

extract_title <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- NULL
    res <- try(unique(unlist(x$Description$Title)), silent = TRUE)
    if (!inherits(res, "try-error")) {
      if (length(res) == 1) {
        out <- res
      }
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    msg <- paste0("Could not extract Title for BioSample ", biosample, ".")
    stop(msg)
  }
  return(out)
}
