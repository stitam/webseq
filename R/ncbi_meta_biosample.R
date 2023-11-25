#' Collect NCBI BioSample metadata
#' 
#' This function collects metadata for entries in NCBI BioSample database.
#' @importFrom rentrez entrez_fetch
#' @importFrom XML xmlParse xmlRoot xmlToList
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble as_tibble 
#' @param biosample_uids character; a character vector of BioSample UID-s.
#' @param verbose logical; Should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' # Sample metadata from BioSample ID
#' biosample_uid <- get_uid("SAMN02714232", db = "biosample")
#' ncbi_meta(biosample_uid)
#' 
#' # Sample metadata from assembly accession 
#' assembly_uid <- get_uid("GCF_000695855.3")
#' biosample_uid <- ncbi_link_uids(
#'   assembly_uid, from = "assembly", to = "biosample")
#' ncbi_meta_biosample(biosample_uid)
#' }
ncbi_meta_biosample_xml <- function(
    biosample_uids,
    verbose = getOption("verbose")
    ) {
 foo <- function(x) {
   res <- rentrez::entrez_fetch(
     db = "biosample", id = x, rettype = "full", retmode = "xml")
   parsed_xml <- XML::xmlParse(res)
   rootnode <- XML::xmlRoot(parsed_xml)
   out <- data.frame(biosample_uid = x)
   attr_node <- rootnode[[1]][["Attributes"]]
   if (!is.null(attr_node)) {
     attr_list <- XML::xmlToList(attr_node)
     for (i in 1:length(attr_list)) {
       attrib <- data.frame(attr_list[[i]][["text"]]) 
       names(attrib) <- attr_list[[i]][[".attrs"]]["display_name"]
       names(attrib) <- tolower(names(attrib))
       names(attrib) <- gsub(" +", "_", names(attrib))
       out <- dplyr::bind_cols(out, attrib)
     }  
   }
   owner_node <- rootnode[[1]][["Owner"]]
   if (!is.null(owner_node)) {
     owner_list <- XML::xmlToList(owner_node)
     if (!is.null(owner_list$Contacts$Contact$.attrs)) {
       if ("email" %in% names(owner_list$Contacts$Contact$.attrs)) {
         out$owner_name <- ifelse(!is.null(owner_list$Name),
                                  owner_list$Name,
                                  NA)
         out$owner_firstname <- ifelse(
           !is.null(owner_list$Contacts$Contact$Name$First),
           owner_list$Contacts$Contact$Name$First,
           NA)
         out$owner_lastname <- ifelse(
           !is.null(owner_list$Contacts$Contact$Name$Last),
           owner_list$Contacts$Contact$Name$Last,
           NA)
         index <- which(names(owner_list$Contacts$Contact$.attrs) == "email")
         out$owner_email <- owner_list$Contacts$Contact$.attrs[index]
       }
     }
   }
   return(out)
 }
 out <- lapply(biosample_uids, foo)
 out <- dplyr::bind_rows(out)
 out <- tibble::as_tibble(out)
 return(out)
}

# entrez fetch > xml2::as_list(xml2::read_xml()) > [[1]] > lapply
ncbi_meta_biosample_xml_entry <- function(x, verbose = getOption("verbose")) {
  # attributes(x)$names contains all fields! use for validation
  # have a separate function for each field
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
      biosample = main_attrs$accession,
      biosample_uid = main_attrs$id
    ),
    # ids
    get_ids(x, biosample, verbose),
    # description
    data.frame(
      title = title(x, biosample, verbose),
      sample_name = sample_name(x, biosample, verbose),
      organism = organism(x, biosample, verbose),
      taxid = taxid(x, biosample, verbose)
    ),
    # attributes
    get_attributes(x, biosample, verbose),
    data.frame(
      bioproject = bioproject(x, biosample, verbose),
      access = main_attrs$access,
      status = status(x, biosample, verbose),
      submission_date = main_attrs$submission_date,
      publication_date = main_attrs$publication_date,
      last_update = main_attrs$last_update,
      status_date = status_date(x, biosample, verbose)
    )
  )
  return(out)
}

# data(examples)
# b <- get_uid(examples$biosample, "biosample")
# c <- rentrez::entrez_fetch(db = "biosample", id = b$uid, rettype = "full", retmode = "xml")
# d <- xml2::as_list(xml2::read_xml(c))[[1]]
# names(d) <- sapply(d, function(x) attributes(x)$accession)

get_ids <- function(
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

get_attributes <- function(x, biosample, verbose = getOption("verbose")) {
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

# ITT TARTOK
get_owner <- function(
    x,
    biosample,
    verbose = getOption("verbose")) {
  out <- NA
  res <- x$Owner
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
  }
  if (length(out) == 1 && is.na(out) && verbose) {
    msg <- paste0(
      "Could not extract Ids for BioSample ", biosample, "."
    )
    message(msg)
  }
  return(out)
}



get_links <- function() {}
get_status <- function() {}
get_description <- function() {}
get_models <- function() {}
get_package <- function() {}

organism <- function(
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

bioproject <- function(
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

status <- function(
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

status_date <- function(
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

taxid <- function(
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

title <- function(
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