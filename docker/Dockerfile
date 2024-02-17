FROM debian:bookworm-20240211

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

# Install system dependencies
RUN apt-get install -y \
  libcurl4-openssl-dev

# Install R and R packages from apt
RUN apt-get install -y \
  r-base \
  r-cran-curl \
  r-cran-devtools \
  r-cran-dplyr \
  r-cran-httr \
  r-cran-jsonlite \
  r-cran-rentrez \
  r-cran-rmarkdown \
  r-cran-stringi \
  r-cran-stringr \
  r-cran-testthat \
  r-cran-tibble \
  r-cran-tidyr \
  r-cran-xml \
  r-cran-xml2

# Install webseq
RUN R -e 'options(warn = 2); devtools::install_github("stitam/webseq", upgrade = "never")'

# Set R library path to avoid binding from host system
ENV R_LIBS_USER /usr/local/lib/R/site-library