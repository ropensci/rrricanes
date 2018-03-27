FROM rocker/tidyverse:3.4.4

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

RUN mkdir -p /home/rstudio/.rstudio/monitored/user-settings
COPY ../user-settings /home/rstudio/.rstudio/monitored/user-settings/
RUN chown -R rstudio:rstudio /home/rstudio/.rstudio
