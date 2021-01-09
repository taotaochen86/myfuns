
# 01 convert pdf to jpeg --------------------------------------------------

#' convert pdf to jpeg
#'
#' @param filefFrom pdf file path, a string
#' @param pages page number needed to convert
#' @param dirSaved  directory to save jpeg files
#' @param dpi  jpeg resolution
#' @examples
#' pdf2jpg(pdfpath,dpi = 300)
#' @export
#'

convert.pdf2jpg <- function(filefFrom, pages = NULL, dirSaved = NULL, dpi = 300) {
  tmpfile <- tempfile(fileext = ".pdf")
  file.copy(from = filefFrom, tmpfile, overwrite = TRUE)


  Packages.InstallandLoad(c("pdftools"))
  if (is.null(dirSaved)) {
    dirSaved <- dirname(tmpfile)
  }
  prefix <-
    tmpfile %>%
    basename() %>%
    stringr::str_remove("\\.[^.]*$")

  info <- pdf_info(tmpfile)
  jpgFiles <- sprintf("%s/%02d.jpg", dirSaved, c(1:info$pages))

  if (!is.null(pages)) {
    jpgFiles <- jpgFiles[pages]
  }
  pdf_convert(
    pdf = filefFrom, format = "jpeg", pages = pages, filenames = jpgFiles,
    dpi = dpi, antialias = TRUE, opw = "", upw = "", verbose = TRUE
  )

  walk(
    jpgFiles,
    ~file.rename(from = .x, to = file.path(
      dirname(.x), str_c(prefix, "_", basename(.x))
    ))
  )
  shell.exec(dirname(tmpfile))
}


#' convert Excel datetime to R datetime
#'
#' @param x datetime colum read from Excel file
#' @export
#'

convert.Datetime.excel2R <- function(x){
  as.Date(x, origin = "1899-12-30")
}
