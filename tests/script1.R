library(tidyverse)
library(glue)
source("R/00 shared.R")
source("R/CMA_data.R")

# path <- choose.dir()

path <- "D:/迅雷下载/气象数据"
YearRange = c(2018, 2018)

if(!is.na(path)){

  DS <-
    Taotao::capture.CMAdata.NEC(path = path,
                                YearRange = YearRange)
}

DS.ln.temp <- DS %>% dplyr::filter(EnProvince=="Liaoning")

DS.ln <- DS.ln.temp %>% dplyr::select(SID,StationNam,data.ET0) %>% unnest() %>%
  dplyr::select(SID,StationNam,Y,M,D,Rainfall) %>% nest(-StationNam,-SID)

DS.ln <- map2(DS.ln$SID,DS.ln$data,~.y %>% set_names(nm=c("Y","M","D",.x)))

DS.ln %>% purrr::reduce(.f = function(...)left_join(...,by = c("Y","M","D"))) %>%
  Taotao::preview.csv()

Taotao::convert.pdf2jpg("D:\\OneDrive - syau.edu.cn\\【00科研】\\【个人材料】\\Rmarkdown个人介绍\\assets\\08获得知识产权情况\\一种田间小区氨发挥的采集装置及使用方法受理.pdf")
