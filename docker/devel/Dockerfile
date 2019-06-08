FROM rocker/rstudio:devel

RUN apt-get update \
  && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libproj-dev \
    vim

RUN install2.r -e \
    crul \
    curl \
    devtools \
    dplyr \
    httr \
    lubridate \
    magrittr \
    maptools \
    purrr \
    readr \
    rgdal \
    rgeos \
    rnaturalearthdata \
    rvest \
    sf \
    sp \
    stringr \
    testthat \
    tibble \
    tidyr \
    remotes \
    xml2

RUN installGithub.r \
    klutometis/roxygen@v6.1.1 \
    r-lib/rlang@v0.2.1 \
    r-lib/pkgdown \
    tidyverse/ggplot2@v3.1.0

RUN Rscript -e 'install.packages("rrricanesdata", repos = "https://timtrice.github.io/drat/", type = "source");'

RUN Rscript -e 'devtools::install_github("ropensci/rrricanes@develop");'

RUN cd /home/rstudio/.rstudio/monitored/user-settings/ \
  && mv user-settings user-settings.copy \
  && wget https://gist.githubusercontent.com/timtrice/94a679b51388faf99ef7918c7bdaff8d/raw/9a52ffebd1e2e8587918a31ff8e962110b816936/user-settings \
  && chown -R rstudio:rstudio user-settings

RUN Rscript -e 'sessionInfo();'
