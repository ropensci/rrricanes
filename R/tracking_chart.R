#' @title tracking_chart
#' @description Build base tracking chart
#' @param states Show state boundaries. TRUE by default.
#' @param res Resolution of charts; low (1:110m), medium (1:50m), high (1:10m).
#'     Default is low.
tracking_chart <- function(states = TRUE, res = "low") {
    if (!(res %in% c("low", "medium", "high")))
        stop("Chart resolution must be low, medium, or high")
}
