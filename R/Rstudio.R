#' show help document using a chrome browser
#'
#' citenow()
#' @export
#'
Rstudio.chrome <- function() {
  options(browser = "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe")
}


#' preview a Bookdown Chapter
#'
#' Rstudio.previewChapter()
#' @export
#'

Rstudio.previewChapter <- function(){
  require(bookdown)
  require(rstudioapi)

  mfile = rstudioapi::documentPath()
  rstudioapi::documentSaveAll()
  xx <- bookdown::preview_chapter(mfile,
                            output_format = "bookdown::gitbook",
                            encoding = "UTF-8")

  shell.exec(xx)

}


#' subscript
#' @export
#'

Rstudio.subscript <- function() {
  id <- rstudioapi::documentId(allowConsole = TRUE)
  selection <- rstudioapi::selectionGet(id = id)
  text <- sprintf("~%s~",selection$value)
  rstudioapi::selectionSet(value = text, id = id)

}

#' subscript
#' @export
#'

Rstudio.supscript <- function() {
  id <- rstudioapi::documentId(allowConsole = TRUE)
  selection <- rstudioapi::selectionGet(id = id)
  text <- sprintf("^%s^",selection$value)
  rstudioapi::selectionSet(value = text, id = id)
}



