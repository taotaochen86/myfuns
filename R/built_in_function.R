
getTEMPFile <- function(fileext =".tiff"){
  require(glue)
  require(lubridate)
  require(stringr)
  timestring <- stringr::str_replace_all(as.character(lubridate::now()),"[- :]","")
  glue("{tempdir()}\\{timestring}{fileext}")
}
