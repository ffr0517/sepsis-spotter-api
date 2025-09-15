FROM rocker/r-ver:4.3.2

# System libs (OpenMP for xgboost)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libicu-dev \
    libsodium-dev \
    zlib1g-dev \
    make \
    g++ \
    pkg-config \
    libgomp1 \
 && rm -rf /var/lib/apt/lists/*

# Posit Package Manager (Ubuntu 22.04 Jammy binaries)
ENV R_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/latest

# Install R deps (added Matrix, vctrs)
RUN R -e "options(repos = c(CRAN = Sys.getenv('R_REPO')));" \
      -e "install.packages(c( \
            'plumber','jsonlite','dplyr','workflows','parsnip','hardhat','tibble','rlang', \
            'recipes','xgboost','Matrix','vctrs' \
          ), Ncpus = parallel::detectCores())"

WORKDIR /app
COPY plumber.R /app/plumber.R
COPY models /app/models

EXPOSE 8000

ENV OMP_NUM_THREADS=1 \
    OPENBLAS_NUM_THREADS=1 \
    MKL_NUM_THREADS=1 \
    VECLIB_MAXIMUM_THREADS=1 \
    NUMEXPR_NUM_THREADS=1 \
    XGBOOST_NUM_THREADS=1

CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]

