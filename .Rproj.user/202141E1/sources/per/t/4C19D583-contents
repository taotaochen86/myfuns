#' Chinese filename to Pinyin filename
#'
#'
#' @param filename a chinese filename
#' @param other_replace replace the non chinese character to a specifed char
#' @return a pinyin filename
#' @examples
#' citenow()
#' @export
#'
file.toPinyin <- function(filename,other_replace=NULL) {
  require(pinyin)
  require(xfun)
  mypy <- pydic(method = "toneless",
                multi = FALSE,
                dic = "pinyin2")
  ext <- xfun::file_ext(filename)
  file_noExt <- xfun::sans_ext(filename)
  newFile <-
    py(file_noExt,
       sep = "",
       other_replace = other_replace,
       dic = mypy)
  newFile <- xfun::with_ext(newFile, ext = ext)
  file.rename(filename, newFile)
  return(newFile)
}



#' Convert all filenames in folder to Pinyin files
#'
#'
#' @param dir a directory containing chinese filenames
#' @examples
#' folder.toPinyin('./')
#' @export
#'
folder.toPinyin <- function(dir) {
  require(pinyin)
  mypy <- pydic(method = "toneless",
                multi = FALSE,
                dic = "pinyin2")
  pinyin::file.rename2py(dir, mypy)
}






