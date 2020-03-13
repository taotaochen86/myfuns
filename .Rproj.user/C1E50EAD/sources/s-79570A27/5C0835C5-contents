
# 01 ggplot2.barplot.oneway --------------------------------------------------

#' draw one way bar plot
#'
#' @param trt the treatment of the one-way experiment
#' @param means mean of each treatment
#' @param std standard deviation of each treatment
#' @param label Significance label of each treatment
#' @param xlab x axis title
#' @param ylab y axis title
#' @param theme.params y theme paramter used to revise ggplot2 theme
#' @return a ggplot2 object
#' @examples
#' preview.csv(iris)
#' @export

ggplot2.barplot.oneway <- function(trt, means, std, label, xlab = NULL, ylab = NULL, theme.params = NULL) {
  require(ggplot2)
  ds <- data.frame(trt, means, std, label)
  ggplot(data = ds, aes(x = trt, y = means)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = means - std, ymax = means + std), width = 0.2) +
    geom_text(aes(y = means + std, label = label), vjust = -1) +
    theme_classic() +
    labs(x = xlab, y = ylab) + theme.params
}


# 02 ggplot2.labs.multiLines -------------------------------------------------

#' add multi-lines xlab and ylab
#'
#' @param g a ggplot object
#' @param x.labs a vector or list of title for x axis
#' @param y.labs a vector or list of title for y axis
#' @param fontsize fontsize for both x and y axes
#' @return An arrangeGrob object
#' @examples
#' g <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
#' geom_point()
#' ggplot2.labs.multiLines(g,x.labs = list("Sepal Length","unit"),
#'                         fontsize = 7,padding =0.3) %>%
#' Taotao::preview.ggplot(width = 10,height = 7)
#'  @export

ggplot2.labs.multiLines <- function(g, x.labs = NULL, y.labs = NULL, fontsize = 7, padding = 0.4) {
  Packages.InstallandLoad(c("grid", "gridExtra"))

  g <- g + theme(
    plot.margin = margin(0, 0, -0.05, -0.05, unit = "cm"),
    axis.title = element_text(size = fontsize)
  )

  if (!is.null(x.labs)) {
    g <- g + theme(axis.title.x = element_blank())
  }
  if (!is.null(y.labs)) {
    g <- g + theme(axis.title.y = element_blank())
  }

  gp <- grid::gpar(fontsize = fontsize, font = 1)
  if (!is.null(x.labs)) {
    for (x.lab in x.labs) {
      g <- gridExtra::arrangeGrob(g, bottom = textGrob(x.lab, gp = gp), padding = unit(padding, "line"))
    }
  }
  if (!is.null(y.labs)) {
    for (y.lab in rev(y.labs)) {
      g <- gridExtra::arrangeGrob(g, left = textGrob(y.lab, gp = gp, rot = 90), padding = unit(padding, "line"))
    }
  }
  return(g)
}
