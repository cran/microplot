theme_collapse <- function (  ## the commented values are from theme_grey
  panel.grid.major=eb, ## element_line(colour = "white")
  panel.grid.minor=eb, ## element_line(colour = "white", size = 0.25)
  axis.ticks=eb,       ## element_line(colour = "grey20")
  axis.text=eb,        ## element_text(size = rel(0.8), colour = "grey30")
  axis.title=eb,       ## axis.title.x = element_text(
                       ##    margin = margin(t = 0.8 * half_line, b = 0.8 * half_line/2))
                       ## axis.title.y = element_text(angle = 90,
                       ##    margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2))
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
  ...,
  eb=ggplot2::element_blank()) {

  A <- ggplot2::theme_minimal()

  B <- ggplot2::theme(
    panel.grid.major = panel.grid.major,
    panel.grid.minor = panel.grid.minor,
    axis.ticks = axis.ticks,
    axis.text = axis.text,
    axis.title = axis.title,
    plot.margin = plot.margin,
    ...)

  ggplot2::`%+replace%`(A, B)
}

