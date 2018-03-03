# set CRAN mirrors
local({
  r <- getOption("repos")
	r["CRAN"] <- "https://mran.microsoft.com/snapshot/2017-07-16"
  options(repos = r)
})

# rrricanes dependencies
install.packages(c("broom", 
                   "crul", 
                   "dplyr", 
                   "ggplot2", 
                   "httr", 
                   "lubridate", 
                   "magrittr", 
                   "maptools", 
                   "purrr", 
                   "readr", 
                   "rgdal", 
                   "rgeos", 
                   "rlang", 
                   "rnaturalearthdata", 
                   "rvest", 
                   "stringr", 
                   "tibble", 
                   "tidyr", 
                   "xml2"))

# Suggests
install.packages(c("devtools", 
                   "knitr", 
                   "rmarkdown", 
                   "rrricanesdata", 
                   "sp", 
                   "testthat"))

# Non-pkg required
install.packages(c("pkgdown"))
