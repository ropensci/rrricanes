FROM rocker/rstudio:3.4.3

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y \
    vim \
    libgdal-dev \
    libproj-dev \
    libxml2-dev

# Add working directory for proceeding commands
WORKDIR /home/rstudio

# Clone GitHub repo
RUN git clone https://github.com/ropensci/rrricanes.git

# Move to rrricanes directory
WORKDIR /home/rstudio/rrricanes

# Checkout branch develop
RUN git checkout develop

# Install R packages
RUN Rscript _docker/_install.R

# Change ownership of all files to rstudio
RUN chown -R rstudio:rstudio .

