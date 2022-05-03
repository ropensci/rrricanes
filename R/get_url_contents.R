#' @title get_url_contents
#' @description Get contents from URL
#' @details This function primarily is reserved for extracting the contents of
#' the individual products (thought it can be used in other instances). Often,
#' there are timeout issues. This is an attempt to try to work around that.
#' @param link URL to download
#' @keywords internal
get_url_contents <- function(links) {

  download_text <- function(grouped_links) {

    # Create a new Async object with `grouped_links`
    grouped_links <- crul::Async$new(urls = grouped_links)

    # Get `grouped_links`
    results <- grouped_links$get()

    # Do we have any bad `grouped_links`?
    bad_results_ind <- which(purrr::map(results, ~.$success()) == FALSE)
    if (length(bad_results_ind) > 0) {
      warning(sprintf("URL %s was unsuccesful.\n",
                      purrr::map(results[bad_results_ind], ~.$url)),
              call. = FALSE)
      # Remove bad `grouped_links`
      results <- results[-bad_results_ind]
    }
    purrr::map_chr(results, ~.$parse(encoding = "UTF-8"))
  }

  # Create groups of links divisible by 80. We are to allow no more than 80
  # requests every 10 seconds. If length of `link` is less than 80, then will
  # only have one group and should have no delay.
  groups <- ceiling(seq(links)/80)

  if (is.vector(links)) {
    links <- split(links, groups)
  }

  # Set progress bar
  p <- dplyr::progress_estimated(n = length(links))

  contents <-
    purrr::imap(links, .f = function(x, y) {

      if (as.numeric(y) != length(links)) {
        # Send group of links to `download_txt`
        txt <- download_text(x)

        # We are not in the last group; apply a delay
        p$tick()$print()
        if (getOption("rrricanes.working_msg"))
          message("Waiting 10 seconds to retrieve large numbers of links.")
        p$pause(10)
        txt
      } else {
        # Send group of links to `download_txt`
        p$tick()$print()
        download_text(x)
      }
    })

  purrr::flatten_chr(contents)

}
