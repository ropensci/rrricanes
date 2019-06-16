FROM rocker/rstudio:devel

RUN apt-get update \
  && apt-get install -y \
    libmagick++-dev \
    libudunits2-dev \
    libgdal-dev \
    libproj-dev \
    qpdf \
    vim

RUN install2.r -e \
    broom \
    covr \
    crul \
    curl \
    devtools \
    dplyr \
    gganimate \
    ggplot2 \
    httr \
    HURDAT \
    lubridate \
    magick \
    magrittr \
    maptools \
    pkgdown \
    purrr \
    readr \
    rgdal \
    rgeos \
    rlang \
    rnaturalearthdata \
    roxygen2 \
    rvest \
    sf \
    sp \
    stringr \
    testthat \
    tibble \
    tidyr \
    remotes \
    xml2

RUN Rscript -e 'install.packages("rrricanesdata", repos = "https://timtrice.github.io/drat/", type = "source");'

RUN cd /home/rstudio/.rstudio/monitored/user-settings/ \
  && mv user-settings user-settings.copy \
  && wget https://gist.githubusercontent.com/timtrice/94a679b51388faf99ef7918c7bdaff8d/raw/9a52ffebd1e2e8587918a31ff8e962110b816936/user-settings \
  && chown -R rstudio:rstudio user-settings

RUN Rscript -e 'sessionInfo();'
