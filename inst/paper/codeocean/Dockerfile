FROM rocker/r-ver:4.3.3

# System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# R packages
RUN R -e "install.packages(c('duckdb', 'arrow', 'dplyr', 'DBI', 'R6', 'bench', 'ggplot2', 'digest'), repos='https://cran.rstudio.com/')"

# Bioconductor
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); BiocManager::install(c('SummarizedExperiment', 'S4Vectors'))"

# Install OmicsLake from source
COPY . /omicslake
WORKDIR /omicslake
RUN R -e "devtools::install('.')"

WORKDIR /code
