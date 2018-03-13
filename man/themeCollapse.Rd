\name{theme_collapse}
\alias{theme_collapse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the \pkg{ggplot2} theme to remove all marginal space.
}
\description{
  Set the ggplot2 theme to remove all marginal space.
  By default the grid, ticks, tick labels, and axis labels are set to blank.
  Margins are set to 0.
}
\usage{
  theme_collapse(      ## the commented values are from theme_grey
  panel.grid.major=eb, ## element_line(colour = "white")
  panel.grid.minor=eb, ## element_line(colour = "white", size = 0.25)
  axis.ticks=eb,       ## element_line(colour = "grey20")
  axis.text=eb,        ## element_text(size = rel(0.8), colour = "grey30")
  axis.title=eb,       ## axis.title.x = element_text(
                       ##    margin = margin(t = 0.8 * half_line,
                       ##                    b = 0.8 * half_line/2))
                       ## axis.title.y = element_text(angle = 90,
                       ##    margin = margin(r = 0.8 * half_line,
                       ##                    l = 0.8 * half_line/2))
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
  ...,
  eb=ggplot2::element_blank())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{panel.grid.major, panel.grid.minor, axis.ticks, axis.text,
    axis.title, plot.margin}{
    \pkg{ggplot2} theme elements.  See \code{\link[ggplot2]{theme}} for information.
  }
  \item{\dots}{
    Other valid arguments to \code{ggplot2::theme}.
  }
  \item{eb}{Convenience for \code{ggplot2::element_blank()}.
  }
}
\details{
When very small plots are placed inside a LaTeX tabular environment, it is often
helpful
to suppress margins, axes, labels, titles.
}
\value{
A ggplot2 theme object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
The first draft of theme_collapse was written by Karen Byron.
}
\examples{
theme_collapse()
## Please see the package documentation for a simple example in context.

## Please see the demos for more interesting examples.
## demo(package="microplot")
}
\keyword{ ggplot2 }
