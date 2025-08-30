FROM rocker/r-ver:4.3.2

# System libs needed by plumber deps (httpuv, sodium, curl, openssl, xml2, etc.)
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
 && rm -rf /var/lib/apt/lists/*

# Use Posit Public Package Manager (binary builds for Ubuntu Jammy)
ENV R_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/latest

# Install required R packages
RUN R -e "options(repos=c(CRAN(Sys.getenv('R_REPO')))); install.packages(c( \
  'plumber','jsonlite','dplyr','workflows','parsnip','hardhat','tibble','rlang' \
))"

WORKDIR /app
COPY plumber.R /app/plumber.R
COPY models /app/models

EXPOSE 8000
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]