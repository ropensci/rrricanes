FROM rocker/tidyverse:3.4.1

RUN apt-get update \ 
  && apt-get upgrade -y \ 
  && apt-get install -y \ 
    libgdal-dev \ 
    libproj-dev \
    libxml2-dev \
    vim \
  && install2.r -e \ 
    broom \ 
    crul \
    devtools \
    httr \
    knitr \
    lubridate \
    magrittr \
    maptools \
    rgdal \
    rgeos \
    rlang \
    rmarkdown \
    rnaturalearthdata \
    rvest \
    sp \
    testthat \
    xml2 \ 
  && install2.r -e -r https://timtrice.github.io/drat/ \ 
    rrricanesdata \
  && installGithub.r \ 
    r-lib/fs@v1.2.1 \ 
    r-lib/pkgdown \ 
    klutometis/roxygen@v6.0.1
