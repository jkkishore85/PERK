#' file_input
#'
#' @description A function to input file .csv
#'
#' @return return input file tibble
#'
#' @importFrom readr read_csv
#' @importFrom tidyr as_tibble
#'
#' @noRd
file_input <- function(name, path){
  ext <- tools::file_ext(name)
  bad_format <- FALSE
  duplicated_vals <- FALSE
  if (all(c("csv", "CSV") != ext)) {
    bad_format = TRUE
    return(list(bad_format = bad_format))
  } else {
    dataInput <- readr::read_csv(path) %>%
      tidyr::as_tibble()
    return(list(dataInput = dataInput))
  }

}
