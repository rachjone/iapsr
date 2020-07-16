library(tidyverse)
library(magrittr)

#' Filter for positive pictures
#'
#' Filter the IAPS_id dataset for questions where individuals were asked "How positive does this make you feel?"
#'
#' @param data The \code{IAPS_id} file.
#'
#' @return filtered data set
#' @export

getPositive <- function(data) {

  positiveData <- data %>% dplyr::filter(stringr::str_detect(picture, ".P"))

  return(positiveData)
}
