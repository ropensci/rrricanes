FROM rocker/tidyverse:devel

RUN apt-get update \ 
  && apt-get upgrade -y \ 
  && apt-get install -y \ 
    libgdal-dev \ 
    libproj-dev \
    vim \ 
  && install2.r -e \
    crul \
    rgdal \
    rgeos \
    rnaturalearthdata \ 
  && install2.r -e -r https://timtrice.github.io/drat/ \ 
    rrricanesdata \ 
  && installGithub.r \ 
    r-lib/fs \
    r-lib/pkgdown
