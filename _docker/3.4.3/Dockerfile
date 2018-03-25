FROM rocker/tidyverse:3.4.3

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
    rnaturalearthdata

RUN install2.r -e -r https://timtrice.github.io/drat/ \ 
    rrricanesdata
