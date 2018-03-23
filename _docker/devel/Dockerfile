FROM rocker/tidyverse:devel

RUN apt-get update \ 
  && apt-get upgrade -y \ 
  && apt-get install -y \ 
    libgdal-dev \ 
    libproj-dev \
    vim

RUN install2.r -e crul
RUN install2.r -e rgdal
RUN install2.r -e rgeos
RUN install2.r -e rnaturalearthdata
