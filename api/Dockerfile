FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
 && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','jsonlite','dplyr','workflows','parsnip','hardhat','tibble','rlang'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY plumber.R /app/plumber.R
COPY models /app/models

EXPOSE 8000
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]