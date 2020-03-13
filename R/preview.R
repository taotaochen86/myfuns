# 01 preview.csv ----------------------------------------------------------

#' preview dataset using Excel
#'
#' @param ds a data.frame dataset
#' @param sheetName sheet name of excel file
#' @param xl whether to use excel.link to preview data.frame
#' @examples
#' preview.csv(iris)
#' @export

preview.csv <- function(ds, sheetName = NULL, xl = FALSE, fileEncoding = "gb2312") {
  if (xl) {
    Packages.InstallandLoad(c("stringi", "excel.link"))
    if (is.null(sheetName)) {
      sheetName <- stringi::stri_rand_strings(1, 10)
    } else {
      sheetName <- sprintf("xl_%s", sheetName)
    }
    xl.sheet.add(sheetName)
    xl.sheet.activate(sheetName)
    xlc$a1 <- excel.link:::fix_encoding(ds)
  } else {
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(x = ds, file = tmpfile,fileEncoding = fileEncoding)
    shell.exec(tmpfile)
  }
}


# 02 data visualization ------------------------------------------------------
#
#' data visualization using shiny
#'
#' @param tibble a data.frame dataset
#' @examples
#' tibble_preview(iris)
#' @export
preview.tibble <- function(data_tibble) {
  Packages.InstallandLoad(c(
    "tidyverse", "shiny",
    "DT", "glue", "excel.link", "stringi"
  ))

  title <- substitute(data_tibble)
  # Application title
  ui <- fluidPage(
    titlePanel(glue("{title}数据集可视化")),
    h3('开发者：陈涛涛'),
    h3("开发日期：2018年9月21日"),
    helpText('本shiny小程序可实现tibble对象可视化，对于字符、数字等数据类型直接窗口中显示；对于data.frame、list或者嵌套tibble等复杂数据，可以通过双击窗口中"打开"按钮，用excel打开。'),
    uiOutput("tibble")
  )

  server <- function(input, output) {
    output$tibble <- renderUI({
      fluidRow(column(
        width = 8,
        DT::dataTableOutput("tibble_Window"),
        tags$script(
          HTML(
            '$(tibble_Window).on("click", "button", function () {
            Shiny.onInputChange("object_button",this.value);})'
        )
          )
        ))
    })
    output$tibble_Window <- renderDataTable({
      expand.grid(
        col = names(data_tibble),
        row = 1:nrow(data_tibble),
        stringsAsFactors = FALSE
      ) %>%
        mutate(button = map2(
          .x = row,
          .y = col,
          ~ifelse(
            is.list(data_tibble[.x, .y][[1]][[1]]),
            glue(
              '<button type="button" name="object_button" value="{.y}:{.x}" >打开</button>'
            ) %>% as.character(),
            data_tibble[.x, .y][[1]][[1]]
          )
        )) %>%
        spread(key = col, value = button) %>%
        dplyr::select(-row) %>%
        dplyr::select(one_of(names(data_tibble)))
    },
    server = FALSE,
    escape = FALSE,
    selection = "none",
    options = list(
      ordering = FALSE,
      columnDefs = list(list(className = "dt-center", targets = seq_along(data_tibble)))
    )
    )

    HSDData <- observeEvent(input$object_button, {
      temp <- input$object_button %>% stringr::str_split(":") %>% `[[`(1)
      out <- data_tibble[as.integer(temp[2]), temp[1]][[1]][[1]]
      xl.sheet.add(stri_rand_strings(1, length = 10))
      xlc$a1 <- out
    })
    }


  shinyApp(ui = ui, server = server,) %>% print()
  }



# 03 quick preview ggplot figure ---------------------------------------------

#' quick preview ggplot figure
#'
#' @param G a ggplot2 object
#' @examples
#'
#' @export
#'
preview.ggplot <- function(G, width = 17.5, height = 20, dpi = 600) {
  Packages.InstallandLoad(c("ggplot2","cowplot"))
  tmpfile <- tempfile(fileext = ".tiff")
  ggsave(
    plot = G, filename = tmpfile, units = "cm", width = width,
    height = height, dpi = dpi, compression = "lzw"
  )
  shell.exec(tmpfile)
}

# 04 quick preview tmap figure -----------------------------------------------
#' quick preview tmap figure
#'
#' @examples
#'
#' @export
preview.tmap <- function(..., ncol = 1, nrow = 1, width = 10, height = 7, res = 600) {
  require(tmap)
  file <- tempfile(fileext = ".tiff")
  tiff(
    filename = file,
    width = width,
    height = height,
    units = "cm",
    compression = "lzw",
    res = res
    # family="Arial Unicode MS"
  )
  tmap_arrange(..., ncol = ncol, nrow = nrow, outer.margins = 0) %>% print()
  dev.off()
  shell.exec(file)
}




# 05 preview laTex equation -----------------------------------------------

#' quick preview Latex expression
#'
#' @param tex.strings a laTex list
#' @examples
#' latex.preview(list("$y=x^2+1,r^2=0.96,p<0.05$", "$\\overset{y=x^2+1}{r^2=0.96,p<0.05}{a}$"))
#' latex.preview("$\\overset{grain\\,yield\\,grain\\,yield}{(kg\\,ha^{-1})}$")
#'  @export
#'

preview.latex <- function(tex.strings = "$y=x^2+x \\times y$") {
  Packages.InstallandLoad("latex2exp")
  if (!is.null(dev.list())) {
    dev.off()
  }
  plot(x = c(0, 1), y = c(0, 1), axes = FALSE, xlab = "", ylab = "", type = "n")
  for (i in seq_along(tex.strings)) {
    text(0.5, 0.6, TeX(tex.strings[[i]]), adj = c(0.5, 0.5 + i))
  }
}


