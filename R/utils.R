
#' @title get_kmz
#' @description Download a kmz file and convert it to
#' sf format.
#'
#' @param url  The url or path to a file with the kmz extension.
#'
#' @return A sf data frame
#' @export

get_kml <-function(url) {
  if (tools::file_ext(url) == "kmz") {
    file_name <- file.path(tempdir(),
                    paste0(basename(url), ".zip"))
  } else if (tools::file_ext(url) %in% c("kml", "zip")) {
    file_name <- file.path(tempdir(), paste0(basename(url)))
  } else {
    return(warning("File extension must be one
                   of kmz, kml, zip."))
  }
    download.file(url,  destfile = file_name,
                quiet = TRUE, method = "auto")
    if (tools::file_ext(file_name) == "zip"){
      file_name  <- unzip(file_name, overwrite = TRUE, exdir = tempdir())
    }
   sf::st_read(file.path(tempdir(), basename(file_name)))

}

