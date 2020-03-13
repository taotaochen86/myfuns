#' RMarkDown citation tool
#'
#' @return a string
#' @examples
#' citenow()
#' @export
#'
citenow <- function() {
  pkg <- c("rbbt","stringr","glue")
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg) > 0) stop(paste("缺少package",new.pkg,"! 请安装工具包"))
  refr <- rbbt::bbt_bib_zotero(.action = rbbt::bbt_return)
  refr2 <- stringr::str_split(refr, "\\n\\n")[[1]]
  refr2 <- setdiff(refr2, "\n")
  key <- stringr::str_extract(refr2, "(?<=\\{)[^,]+")
  keystring <- paste0(glue::glue("@{key}"), collapse = ";")
  context <- rstudioapi::getActiveDocumentContext()
  rstudioapi::modifyRange(context$selection[[1]]$range,keystring,context$id)
}



