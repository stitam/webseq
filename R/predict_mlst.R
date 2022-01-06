#' Predict Multi-Locus Sequence Type
#' 
#' This function predicts the multi-locus sequence type of a set of sequences
#' @param config.file character; path to a local yaml file that stores the job
#' configurations.
#' @param print boolean; verbose messages to console?
#' @return The function will download a set of json files. The output directory
#' can be defined in the config file.
#' @note This function depends on \code{singularity} and requires a docker image.
#' @seealso pull_docker, parse_mlst
#' @references https://sylabs.io/guides/master/user-guide/index.html
#' @export
predict_mlst <- function(config.file, verbose = getOption("verbose")) {
  if (grepl(".yaml$", config.file) == FALSE) {
    stop("'config.file' must be a yaml file")
  }
  config <- yaml::read_yaml(config.file)
  outdir <- paste0("./output/",config$jobname,"/mlst")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  filepaths <- dir(paste0("./input/",config$jobname), full.names = TRUE)
  foo <- function(filepath, dbpath, outdir, species) {
    if (verbose) {
      message("Predicting MLST for ", basename(filepath), ": ", appendLF = FALSE)
    }
    cmd <- paste0("python3 ~/Programs/mlst/mlst.py -i ", filepath,
                  " -p ", dbpath,
                  " -o ", outdir,
                  " -s ", species)
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    file.rename(paste0(outdir, "/data.json"),
                paste0(outdir, "/",basename(filepath), ".json"))
    data <- jsonlite::fromJSON(paste0(outdir, "/",basename(filepath), ".json"))
    mlst <- data$mlst$results$sequence_type
    if (verbose) message(mlst)
  }
  run <- lapply(filepaths, function(x) foo(x,
                                           dbpath = config$mlst$db,
                                           outdir = outdir,
                                           species = config$mlst$species))
}
