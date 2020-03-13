library(tidyverse)
library(future)
source("R/00 shared.R")

#' capture CMA meteorological data
#'
#' @param path directory with files downloaded from CMA websites
#' @param YearRange  Year Range
#' @return a tibble object
#' @examples
#' path <- "D:/迅雷下载/气象数据"
#' YearRange <- c(1957, 2018)
#  DS <- capture.CMAdata.NEC(path, YearRange)
#' preview.tibble(DS)
#' @export
#'

capture.CMAdata.NEC <- function(path, YearRange) {
  require(future)
  StationINFO <- get.StationInfo()
  DS <- getDataSet(path = path)

  DS <- DS %>%
    map(~ .x %>%
          dplyr::filter(SID %in% StationINFO$SID &
                          Y >= YearRange[1] &
                          Y <= YearRange[2]))



  DS <- DS.preprocessed(DS,YearRange) %>%
    as_tibble() %>% nest(-SID)  %>%
    left_join(x = StationINFO, y = ., by = "SID")  %>%
    as.tibble() %>%
    dplyr::filter(!(data %>% map_lgl(is.null)))

  DS$data.ET0 <- pmap(list(
    ds = DS$data,
    latitude = DS$latitude,
    altitude = DS$altitude
  ),
  .f = ET0.daily)


  return(DS %>% dplyr::mutate_if(is.factor,as.character))
}



