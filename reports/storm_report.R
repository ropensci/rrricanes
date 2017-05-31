## ---- Variables --------------------------------------------------------------
#' Only edit this section
key <- "EP022017"

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(rrricanes)

#' Do not edit beyond here.
basin <- stringr::str_sub(key, 0L, 2L)
year_num <- as.numeric(stringr::str_sub(key, 3L, 4L))
year <- as.numeric(stringr::str_sub(key, 5L, 8L))

# Get storms
df <- get_storms(year = year, basin = basin) %>% slice(year_num)

# Storm vars
storm_name <- df %>% .$Name

if (!(dir.exists(sprintf("%s", year))))
    dir.create("%s", year)

rmarkdown::render("./reports/storm_report.Rmd",
                  output_file = sprintf("%s/%s.pdf", year, key),
                  params = list(
                      key = key,
                      set_title = storm_name))
