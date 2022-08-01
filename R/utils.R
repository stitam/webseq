mgnify_endpoints <- function() {
  apis <- c(
    "biomes", "studies", "super-studies", "samples", "runs", "assemblies",
    "analyses", "experiment-types", "pipelines", "pipeline-tools",
    "publications", "genomes", "release", "genomeset", "cogs", "kegg-modules",
    "kegg-classes", "antismash-geneclusters", "annotations/go-terms",
    "annotations/interpro-identifiers", "annotations/kegg-modules",
    "annotations/pfam-entries", "annotations/kegg-orthologs", 
    "annotations/genome-properties", "annotations/antismash-gene-clusters",
    "annotations/organisms", "mydata")
  others <- c(
    "downloads"
  )
  endpoints <- c(apis, others)
  return(endpoints)
}