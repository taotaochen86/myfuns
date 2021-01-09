library("rstudioapi")
library("formatR")
# 01 Packages Install and Load -----------------------------------------------

#' install and load packages
#'
#' @param pkgs package lists
#' @examples
#' Packages.InstallandLoad(c("tidyverse", "glue"))
#' @export

Packages.InstallandLoad <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  invisible(sapply(pkg, library, character.only = TRUE))
}

# 02 auto_format --------------------------------------------------------------

#' auto_format
#' @examples
#' Taotao::auto_format()
#' @export


auto_format <- function() {
  Packages.InstallandLoad(c("rstudioapi", "styler"))
  fileparams <- getSourceEditorContext()
  filePath <- fileparams[["path"]]
  fileID <- fileparams[["id"]]
  styler::style_file(filePath)
  try(tidy_source(filePath, file = filePath, blank = TRUE), silent = TRUE)
  documentSave(id = fileID)
}
