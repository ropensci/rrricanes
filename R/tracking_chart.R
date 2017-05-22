#' @title al_tracking_chart
#' @description Build tracking chart centered on Atlantic Basin.
#' @param ... Additional parameters for ggplot2
#' @seealso \code{\link{tracking_chart}}
#' @return ggplot2 object centered on Atlantic basin.
#' @export
al_tracking_chart <- function(...) {
    p <- tracking_chart(...)
    p <- p + ggplot2::coord_equal(xlim = c(-100, 0), ylim = c(0, 60))
    return(p)
}

#' @title ep_tracking_chart
#' @description Build tracking chart centered on northeast Pacific Basin.
#' @param ... Additional parameters for ggplot2
#' @seealso \code{\link{tracking_chart}}
#' @return ggplot2 object centered on northeast Pacific basin.
#' @export
ep_tracking_chart <- function(...) {
    p <- tracking_chart(...)
    p <- p + ggplot2::coord_equal(xlim = c(-180, -80), ylim = c(0, 35))
    return(p)
}

#' @title tracking_chart
#' @description Build base tracking chart using ggplot
#' @param countries Show country borders. Default TRUE.
#' @param states Show state boundaries. Default TRUE. Ignored if `countries` is
#'     FALSE.
#' @param res Resolution of charts; 110 (1:110m), 50 (1:50m), 10 (1:10m).
#'     Default is low. The higher the resolution, the longer the plot takes to
#'     appear.
#' @param ... Additional ggplot2::aes parameters
#' @return Returns ggplot2 object that can be printed directly or have new
#'     layers added.
#' @seealso \code{\link[ggplot2]{aes}}
#' @export
tracking_chart <- function(countries = TRUE, states = TRUE, res = 110, ...) {

    # Convert to numeric just in case
    res <- as.integer(res)

    # Validate res
    if (!(res %in% c(110, 50, 10)))
        stop("Chart resolution must be 110, 50, 10")

    pkg <- "rnaturalearth"
    if (res %in% c(110, 50)) {
        pkg <- paste0(pkg, "data")
    } else {
        pkg <- paste0(pkg, "hires")
    }

    # A base map can be drawn off either coastlines data or countries data. If
    # countries is FALSE, return coastlines data. Otherwise, build countries w/
    # states if states is TRUE.
    if (!countries) {
        x <- get(paste0("coastline", res))
    } else {
        x <- get(paste0("countries", res))
        if (states) {
            if (res >= 50) {
                # Resolution for states is only 10 or 50. If res is > 50, use
                # rnaturalearthdata::states50
                y <- get(paste0("states", 50))
            } else {
                y <- get(paste0("states", res))
            }
        }
    }

    p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = x,
                              ggplot2::aes(long, lat, group = group), ...) +
        ggplot2::coord_equal()

    if (exists("y"))
        p <- p +
        ggplot2::geom_polygon(data = y,
                              ggplot2::aes(long, lat, group = group), ...)

    return(p)
}
