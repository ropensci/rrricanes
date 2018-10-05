#' @title get_storm_list
#' @description Get a list of all known storms.
#' @details
#'   \describe{
#'     \item{STORM_NAME}{Literal storm name, "INVEST", or "GENESISxxx" where xxx
#'       is a number}
#'     \item{RE}{Region (basin) code: WP, IO, SH, CP, EP, AL, LS. (See 4.
#'       \href{https://www.nrlmry.navy.mil/atcf_web/docs/database/new/database.html#dataoverview}{Data Format})}
#'     \item{X}{Subregion code: W, A, B, S, P, C, E, L, Q. In
#'       \href{https://www.nrlmry.navy.mil/atcf_web/docs/database/new/abdeck.txt}{Storm History Record Format},
#'       these are listed as:
#'       \describe{
#'         \item{A}{Arabian Sea}
#'         \item{B}{Bay of Bengal}
#'         \item{C}{Central Pacific}
#'         \item{E}{Eastern Pacific}
#'         \item{L}{Atlantic}
#'         \item{P}{South Pacific (135E - 120W)}
#'         \item{Q}{South Atlantic}
#'         \item{S}{South Indian Ocean (20E - 135E)}
#'         \item{W}{Western Pacific}
#'       }
#'     }
#'     \item{R2}{Region 2 code: WP, IO, SH, CP, or EP. This and R3-R5 are codes
#'       for basins entered subsequent to the original basin where the storm was
#'       generated.}
#'     \item{R3}{Region 3 code: WP, IO, SH, CP, or EP.}
#'     \item{R4}{Region 4 code: WP, IO, SH, CP, or EP.}
#'     \item{R5}{Region 5 code: WP, IO, SH, CP, or EP.}
#'     \item{CY}{Annual cyclone number: 01 through 99.}
#'     \item{YYYY}{Cyclone Year: 0000 through 9999.}
#'     \item{TY}{Highest level of tc development: TD, TS, TY, ST, TC, HU, SH, XX (unknown).}
#'     \item{I}{S, R, O; straight mover, recurver, odd mover.}
#'     \item{YYY1MMDDHH}{Starting DTG: 0000010100 through 9999123123.}
#'     \item{YYY2MMDDHH}{Ending DTG: 0000010100 through 9999123123}
#'     \item{SIZE}{Storm Size (MIDG (midget) , GIAN (giant), etc.).}
#'     \item{GENESIS_NUM}{Annual genesis number: 001 through 999.}
#'     \item{PAR1}{UNUSED}
#'     \item{PAR2}{UNUSED}
#'     \item{PRIORITY}{Priority for model runs (e.g., GFDN, GFDL, COAMPS-TC, H-WRF): 1-9.}
#'     \item{STORM_STATE}{Storm state: METWATCH,TCFA,WARNING or ARCHIVE}
#'     \item{WT_NUMBER}{Minute of warning or TCFA (00-59)}
#'     \item{STORMID}{Storm ID composed of basin designator and annual cyclone number (e.g. wp081993)}
#'   }
#' @export
get_storm_list <- function() {
  storm_list <-
    readr::read_csv(
      file = "ftp://ftp.nhc.noaa.gov/atcf/index/storm_list.txt",
      col_names = c("STORM_NAME", "RE", "X", "R2", "R3", "R4", "R5", "CY",
                    "YYYY", "TY", "I", "YYY1MMDDHH", "YYY2MMDDHH", "SIZE",
                    "GENESIS_NUM", "PAR1", "PAR2", "PRIORITY", "STORM_STATE",
                    "WT_NUMBER", "STORMID"
      ),
      col_types = "ccccccciiccccciccicic",
      skip = 1L
    ) %>%
    dplyr::mutate_at(
      .vars = c("YYY1MMDDHH", "YYY2MMDDHH"),
      .funs = as.POSIXct,
      format = "%Y%m%d%H",
      tz = "UTC"
    )
}
