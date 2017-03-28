#' @title test_data
#' @description Write data files for tests
#' @details Create small dataframes for testing purposes. This function should 
#' only be executed before closing out a version though obviously the tests can 
#' be run at any time. Should the tests fail it is likely because the HTML 
#' formatting changed in the archive pages. 
#' @export
.test_data <- function() {
  
  ep1998 <- get_storms(year = 1998, basin = "EP")
  save(ep1998, file = "./tests/testthat/data/ep1998.rda")
  
  al2015 <- get_storms(year = 2015, basin = "AL")
  save(al2015, file = "./tests/testthat/data/al2015.rda")
  
  return(TRUE)
}