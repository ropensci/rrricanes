#' @title al_tracking_chart
#' @description Build tracking chart centered on Atlantic Basin.
#' @param ... Additional parameters for \link{tracking_chart} and ggplot2
#' @seealso \code{\link{tracking_chart}}
#' @return ggplot2 object centered on Atlantic basin.
#' @examples
#' \dontrun{
#' # Build map with white land areas, thin black borders
#' al_tracking_chart(color = "black", size = 0.1, fill = "white")
#'
#' # 50nm resolution, no states
#' al_tracking_chart(res = 50, states = FALSE, color = "black", size = 0.1,
#'           fill = "white")
#'
#' # 50nm resolution, coastlines only
#' al_tracking_chart(countries = FALSE, res = 50, color = "black", size = 0.1,
#'           fill = "white")
#'
#' # Adding and modifying with ggplot functions
#' al_tracking_chart(color = "black", size = 0.1, fill = "white") +
#'   ggplot2::labs(x = "Lon", y = "Lat",
#'   title = "Base Atlantic Tracking Chart")
#' }
#' @export
al_tracking_chart <- function(...) {
  p <- tracking_chart(...)
  p + ggplot2::coord_equal(xlim = c(-100, 0), ylim = c(0, 60))
}

#' @title ep_tracking_chart
#' @description Build tracking chart centered on northeast Pacific Basin.
#' @param ... Additional parameters for ggplot2
#' @seealso \code{\link{tracking_chart}}
#' @return ggplot2 object centered on northeast Pacific basin.
#' @examples
#' \dontrun{
#' # Build map with white land areas, thin black borders
#' ep_tracking_chart(color = "black", size = 0.1, fill = "white")
#'
#' # 50nm resolution, no states
#' ep_tracking_chart(res = 50, states = FALSE, color = "black", size = 0.1,
#'           fill = "white")
#'
#' # 50nm resolution, coastlines only
#' ep_tracking_chart(countries = FALSE, res = 50, color = "black", size = 0.1,
#'           fill = "white")
#'
#' # Adding and modifying with ggplot functions
#' ep_tracking_chart(color = "black", size = 0.1, fill = "white") +
#'   ggplot2::labs(x = "Lon", y = "Lat",
#'   title = "Base East Pacific Tracking Chart")
#' }
#' @export
ep_tracking_chart <- function(...) {
  p <- tracking_chart(...)
  p + ggplot2::coord_equal(xlim = c(-140, -80), ylim = c(0, 35))
}

#' @title tracking_chart
#' @description Build base tracking chart using ggplot
#' @param countries Show country borders. Default TRUE.
#' @param states Show state boundaries. Default TRUE. Ignored if `countries` is
#'   FALSE.
#' @param res Resolution of charts; 110 (1:110m), 50 (1:50m), 10 (1:10m).
#'   Default is low. The higher the resolution, the longer the plot takes to
#'   appear.
#' @param ... Additional ggplot2::aes parameters
#' @return Returns ggplot2 object that can be printed directly or have new
#'   layers added.
#' @seealso \code{\link[ggplot2]{aes}}
#' @examples
#' \dontrun{
#' # Build map with white land areas, thin black borders
#' tracking_chart(color = "black", size = 0.1, fill = "white")
#'
#' # 50nm resolution, no states
#' tracking_chart(res = 50, states = FALSE, color = "black", size = 0.1,
#'        fill = "white")
#'
#' # 50nm resolution, coastlines only
#' tracking_chart(countries = FALSE, res = 50, color = "black", size = 0.1,
#'        fill = "white")
#'
#' # Adding and modifying with ggplot functions
#' tracking_chart(color = "black", size = 0.1, fill = "white") +
#'   ggplot2::labs(x = "Lon", y = "Lat", title = "Base Tracking Chart")
#' }
#' @export
tracking_chart <- function(countries = TRUE, states = TRUE, res = 110, ...) {

  # Convert to numeric just in case
  res <- as.integer(res)

  # Validate res
  if (!(res %in% c(110, 50, 10)))
    stop("Chart resolution must be 110, 50, 10")

  pkg <- "rnaturalearth"
  if (res %in% c(110, 50)) {
    pkg <- stringr::str_c(pkg, "data")
  } else {
    pkg <- stringr::str_c(pkg, "hires")
  }

  # A base map can be drawn off either coastlines data or countries data. If
  # countries is FALSE, return coastlines data. Otherwise, build countries w/
  # states if states is TRUE.
  if (!countries) {
    dataset <- stringr::str_c("coastline", res)
    base_map_data <- getExportedValue(ns = pkg, name = dataset)
  } else {
    dataset <- stringr::str_c("countries", res)
    base_map_data <- getExportedValue(ns = pkg, name = dataset)
    if (states) {
      if (res >= 50) {
        dataset <- stringr::str_c("states", 50)
        state_map_data <- getExportedValue(ns = pkg, name = dataset)
      } else {
        dataset <- stringr::str_c("states", res)
        state_map_data <- getExportedValue(ns = pkg, name = dataset)
      }
    }
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = base_map_data,
                          ggplot2::aes(long, lat, group = group), ...) +
    ggplot2::coord_equal()

  if (exists("state_map_data"))
    p <- p +
    ggplot2::geom_polygon(data = state_map_data,
                          ggplot2::aes(long, lat, group = group), ...)

   p

}
