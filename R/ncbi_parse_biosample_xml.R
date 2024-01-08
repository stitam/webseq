#' Parse NCBI BioSample metadata
#' 
#' BioSample metadata from NCBI can be retrieved in multiple file formats. This
#' function parses metadata retrieved in XML format.
#' @param biosample_xml character; unparsed XML metadata either returned by
#'  \code{ncbi_get_meta()} or the path to a file that was downloaded from NCBI.
#' @param verbose logical; Should verbose messages be printed to console?
ncbi_parse_biosample_xml <- function(
    biosample_xml,
    verbose = getOption("verbose")
    ) {
  if (length(biosample_xml) == 1 && is.na(biosample_xml)) {
    if (verbose) message("No BioSample metadata to parse.")
    return(NA_character_)
  }
  parsed_xml <- ncbi_xml_to_list(xml = biosample_xml, verbose = verbose)
  if (length(parsed_xml) == 1 && is.na(parsed_xml)) return(NA_character_)
  out <- try(lapply(parsed_xml, function(x) {
    ncbi_parse_biosample_xml_entry(x, verbose = verbose)
  }),silent = TRUE)
  if (inherits(out, "try-error")) {
    return(NA_character_)
  }
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
  # TODO have an extractor for each field and then bind them together
  main_attrs <- attributes(x)
  expected_names <- c(
    "names", "last_update", "publication_date",
    "access", "submission_date", "id", "accession"
  )
  if (any(!expected_names %in% names(main_attrs))) {
    if (verbose) {
      message(paste0(
        "Could not extract main Attributes for BioSample ",
        main_attrs$accession,
        "."
      ))
    } 
    stop()
  }
  out <- tibble::tibble(biosample_uid = main_attrs$id)
  new_elements <- c(
    "ids",
    "title",
    "organism",
    "package",
    "attributes",
    "owner", 
    "description",
    "links"
  )
  for (i in new_elements) {
    f <- get(paste0("extract_", i))
    newdf <- f(x, main_attrs$accession, verbose)
    if (ncol(newdf) > 0) {
      out <- dplyr::bind_cols(out, newdf)
    } else {
      if (verbose) {
        message(paste0("No ", i, " for BioSample ", main_attrs$accession, "."))
      }
    }
  }
  newdf <- data.frame(
    access = main_attrs$access,
    status = extract_status(x, main_attrs$accession, verbose),
    submission_date = main_attrs$submission_date,
    publication_date = main_attrs$publication_date,
    last_update = main_attrs$last_update,
    status_date = extract_status_date(x, main_attrs$accession, verbose)
  )
  if (ncol(newdf) > 0) {
    out <- dplyr::bind_cols(out, newdf)
  } else {
    if (verbose) {
      message(paste0("No status for BioSample ", main_attrs$accession, "."))
    }
  }
  if (nrow(out) > 1) {
    if (verbose) {
      message(paste0(
        "Multiple parsed rows for BioSample ", main_attrs$accession, "."
      ))
    }
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
    # SAMN03863711 lists two BioSample IDs but the contents are duplicates
    out <- dplyr::distinct(out)
    # Remove the redundant BioSample ID
    index <- which(out$db == "BioSample" & out$id != biosample)
    if (length(index) > 0) {
      out <- out[-index,]
    }
    out <- tidyr::spread(out, "db", "id")
    names(out) <- gsub(" +", "_", tolower(names(out)))
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    if (verbose) {
      message(paste0("Could not extract IDs for BioSample ", biosample, "."))
    }
    stop()
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
      value = unname(sapply(res, function(x) {
        if (length(x) == 0) {
          return(NA)
        }
        if (length(x) == 1) {
          return(unlist(x))
        }
        if (length(x) > 1) {
          return(paste(unlist(x), collapse = "|"))
        }
      }))
    )
    out <- dplyr::distinct(out)
    # EXAMPLE FOR NO ATTRIBUTES: "SAMN00678218"
    if (nrow(out) > 0) {
      out <- tidyr::pivot_wider(
        out,
        names_from = "attr",
        values_from = "value",
        # EXAMPLE SAMN36698370
        values_fn = function(x) paste(sort(unique(x)), collapse = "|")
      )
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    if (verbose) {
      message(paste0(
        "Could not extract Attributes for BioSample ", biosample, "."
      ))
    }
    stop()
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
      owner_name = ifelse(!is.null(unlist(res$Name)), unlist(res$Name), NA),
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
      contact_name_first = if("Contact" %in% names(res)) {
        if ("Contact" %in% names(res$Contacts)) {
          if ("Name" %in% names(res$Contacts$Contact)) {
            if ("First" %in% names(res$Contacts$Contact$Name)) {
              unlist(res$Contacts$Contact$Name$First)
            } else {
              NA_character_
            }
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      } else {
        NA_character_
      },
      contact_name_last = if("Contacts" %in% names(res)) {
        if ("Contact" %in% names(res$Contacts)) {
          if ("Name" %in% names(res$Contacts$Contact)) {
            if ("Last" %in% names(res$Contacts$Contact$Name)) {
              unlist(res$Contacts$Contact$Name$Last)
            } else {
              NA_character_
            }
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
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
    if (verbose) {
      message(paste0(
        "Could not extract Owner info for BioSample ", biosample, "."
      ))
    }
    stop()
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
      if ("Paragraph" %in% names(x$Description$Comment)) {
        out <- unlist(x$Description$Comment$Paragraph)
      }
      if ("Table" %in% names(x$Description$Comment)) {
        if (is.null(out)) {
          out <- "WEBSEQ WARNING: DESCRIPTION CONTAINS A TABLE. CHECK MANUALLY."
        } else {
          out <- paste0(
            out, 
            "WEBSEQ WARNING: DESCRIPTION ALSO CONTAINS A TABLE. CHECK MANUALLY."
          )
        }
      }
    } else {
      out <- NA_character_
    }
    if (!is.null(out)) {
      out <- tibble::tibble(description = out)
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    if (verbose) {
      message(paste0(
        "Could not extract Description for BioSample ", biosample, "."
      ))
    }
    stop()
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
    if (verbose) {
      message(paste0(
        "Could not extract Package for BioSample ", biosample, "."
      ))
    }
    stop()
  }
  return(out)
}

extract_organism <- function(
    x,
    biosample,
    verbose = getOption("verbose")
  ) {
  foo <- function (x, biosample, verbose) {
    out <- data.frame(remove = NA)
    if ("Description" %in% names(x)) {
      if ("Organism" %in% names(x$Description)) {
        if ("OrganismName" %in% names(x$Description$Organism)) {
          out <- dplyr::bind_cols(
            out, 
            data.frame(
              organism_name = unlist(x$Description$Organism$OrganismName)
            )
          )
        }
        if (length(attributes(x$Description$Organism)) > 0) {
          if ("names" %in% names(attributes(x$Description$Organism))) {
            if (attributes(x$Description$Organism)$names != "OrganismName") {
              if (verbose) {
                message("Unexpected Organism attribute name.")
              }
              stop()
            } else {
              index <- which(
                names(attributes(x$Description$Organism)) != "names"
              )
            }
          } else {
            index <- seq_along(length(attributes(x$Description$Organism)))
          }
          if (length(index) > 0) {
            out <- dplyr::bind_cols(
              out, 
              as.data.frame(attributes(x$Description$Organism)[index])
            )
          }
        }
        if (ncol(out) == 1) {
          out <- NULL
        } else {
          out <- out |> dplyr::select(-remove)
        }
      } else {
        out <- NULL
      }
    } else {
      out <- NULL
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    if (verbose) {
      message(paste0(
        "Could not extract Organism for BioSample ", biosample, "."
      ))
    }
    stop()
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
        if (!"target" %in% names(attributes(A))) {
          # EXAMPLE: SAMN36356470
          # TODO: Find a solution to take a note of failed parsings
          return(data.frame())
        }
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
          names_from = "target",
          values_from = "label"
        )
        return(widelink)
      })
      out <- dplyr::bind_cols(linklist)
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    if (verbose) {
      message(paste0(
        "Could not extract BioSample links for BioSample ", biosample, "."
      ))
    }
    stop()
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
    if (verbose) {
      message(paste0("Could not extract status for BioSample ", biosample, "."))
    }
    stop()
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
    if (verbose) {
      message(paste0(
        "Could not extract status_date  for BioSample ", biosample, "."
      ))
    }
    stop()
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
        out <- tibble::tibble(title = res)
      }
    }
    return(out)
  }
  out <- try(foo(x, biosample, verbose), silent = TRUE)
  if (inherits(out, "try-error") | is.null(out)) {
    if (verbose) {
      message(paste0("Could not extract Title for BioSample ", biosample, "."))
    }
    stop()
  }
  return(out)
}
