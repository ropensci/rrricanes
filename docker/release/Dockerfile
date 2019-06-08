FROM rocker/rstudio:latest

RUN apt-get update \
  && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libproj-dev \
    vim

RUN install2.r -e \
    broom \
    crul \
    curl \
    devtools \
    dplyr \
    ggplot2 \
    httr \
    lubridate \
    magrittr \
    maptools \
    purrr \
    readr \
    rgdal \
    rgeos \
    rlang \
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
    r-lib/pkgdown

RUN Rscript -e 'install.packages("rrricanesdata", repos = "https://timtrice.github.io/drat/", type = "source");'

RUN cd /home/rstudio/.rstudio/monitored/user-settings/ \
  && mv user-settings user-settings.copy \
  && wget https://gist.githubusercontent.com/timtrice/94a679b51388faf99ef7918c7bdaff8d/raw/9a52ffebd1e2e8587918a31ff8e962110b816936/user-settings \
  && chown -R rstudio:rstudio user-settings

RUN Rscript -e 'sessionInfo();'
