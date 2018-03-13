\name{graphicsList}
\alias{graphicsList}
\title{
Convert a list of "trellis" objects or list of "ggplot" objects into a
"graphicslist" object.
}
\description{
Convert a list of "trellis" objects or list of "ggplot" objects into a
"graphicslist" object.
}
\usage{
graphicsList(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
The list can either be a "list" object, such as \code{list(g1, g2, g3)},
or the actual list \code{g1, g2, g3}.  All the \code{g*} objects must be
the same class, either \code{"trellis"} or \code{"ggplot"}.
The "list" object may be an array with dim or dimnames, with \code{length(dim(object))}
either 1 or 2.  An actual list, or a "list" object with one dimension,
will be coerced to a column vector of graphics objects.
}
}
\value{
A \code{"graphicsList"} object which can be sent to \code{microplot.graphicsList}.
See \code{\link[microplot:graphicsList]{microplot.graphicsList}} for more discussion.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
\dontrun{
  latexSetOptions()

  ## graphicsList works the same for lattice and ggplot

  ## lattice
  tt <- data.frame(x=1:3, y=4:6, g=c("A","B","A"))

  ## t1 and t2: with key
  t1 <- lattice::xyplot(y ~ x, xlim=c(0,4), ylim=c(3,7), groups=g,
                        data=tt[1:2,], pch=19, col=2:3,
                        key=list(points=list(pch=19, col=2:3), text=list(levels(tt$g))))

  t2 <- lattice::xyplot(y ~ x, xlim=c(0,4), ylim=c(3,7), groups=g,
                        data=tt[3,  ], pch=19, col=2:3,
                        key=list(points=list(pch=19, col=2:3), text=list(levels(tt$g))))

  ## collapsed panels, no key
  latex(graphicsList(t1, t2), title="glt1", width.y.axis=.4, height.x.axis=.4)

  ## collapsed panels, one key
  latex(graphicsList(t1, t2), key=t1$legend$top$args$key, title="glt2",
        width.y.axis=.4, height.x.axis=.4)

  ## uncollapsed panels, one key per panel
  latex(graphicsList(list(t1, t2)), height.panel=2, width.panel=2, collapse=FALSE, title="glt1")

  ## uncollapsed panels, one key
  latex(graphicsList(list(update(t1, legend=NULL),
                          update(t2, legend=NULL))),
        height.panel=2, width.panel=2, collapse=FALSE,
        key=t1$legend$top$args$key, title="glt4")


  ## collapsed panels, no key
  msWord(graphicsList(matrix(list(t1, t2), 2, 1, dimnames=list(c("A","B"), "c"))),
         width.rowname=.5, title="Wt1", width.y.axis=.4, height.x.axis=.4)

  ## collapsed panels, one key
  msWord(graphicsList(matrix(list(t1, t2), 2, 1, dimnames=list(c("A","B"), "c"))),
         key=t1$legend$top$args$key,
         width.rowname=.5, title="Wt2", width.y.axis=.4, height.x.axis=.4)

  ## uncollapsed panels, one key per panel
  msWord(graphicsList(matrix(list(t1, t2), 2, 1, dimnames=list(c("A","B"), "c"))),
         height.panel=2, width.panel=2, collapse=FALSE,
         width.rowname=.5, title="Wt3")

  ## uncollapsed panels, one key
  msWord(graphicsList(matrix(list(update(t1, legend=NULL),
                                  update(t2, legend=NULL)),
                             2, 1, dimnames=list(c("A","B"), "c"))),
         height.panel=2, width.panel=2, collapse=FALSE,
         key=t1$legend$top$args$key,
         width.rowname=.5, title="Wt4")


  ## ggplot
  tt <- data.frame(x=1:3, y=4:6, g=c("A","B","A"))

  library(ggplot2)

  ## g1 and g2: with key
  g1 <-
    ggplot(tt[1:2,], aes(x,y, color=g)) +
    geom_point() +
    xlim(0,4) + ylim(3,7)

  g2 <-
    ggplot(tt[3,], aes(x,y, color=g)) +
    geom_point() +
    xlim(0,4) + ylim(3,7)

  g1key <- plot_grid(get_legend(g1))

  ## collapsed panels, no key
  latex(graphicsList(g1, g2), title="glg1", width.y.axis=.2, height.x.axis=.2)

  ## collapsed panels, one key
  latex(graphicsList(g1, g2), key=g1key, title="glg2", width.y.axis=.2, height.x.axis=.2)

  ## uncollapsed panels, one key per panel
  latex(graphicsList(list(g1, g2)), height.panel=2, width.panel=2, collapse=FALSE, title="glg3")

  ## uncollapsed panels, one key
  latex(graphicsList(list(g1+theme(legend.position="none"),
                          g2+theme(legend.position="none"))),
        height.panel=2, width.panel=2, collapse=FALSE,
        key=g1key, title="glg4")

  ## collapsed panels, no key
  msWord(graphicsList(g1, g2),
         width.rowname=.5, title="Wg1", width.y.axis=.4, ## width.y.axis=.2, ## not in R CMD check
         height.x.axis=.2,
         FlexTableWidths=c(.5, .45, 1))                  ## c(.5, .25, 1)    ## not in R CMD check

  ## collapsed panels, one key                           ## ditto
  msWord(graphicsList(g1, g2), key=g1key,
         width.rowname=.5, title="Wg2", width.y.axis=.4,
         height.x.axis=.2,
         FlexTableWidths=c(.5, .45, 1))

  ## uncollapsed panels, one key per panel
  msWord(graphicsList(list(g1, g2)), height.panel=2, width.panel=2, collapse=FALSE,
         width.rowname=.5, title="Wg3")

  ## uncollapsed panels, one key
  msWord(graphicsList(list(g1+theme(legend.position="none"),
                           g2+theme(legend.position="none"))),
         height.panel=2, width.panel=2, collapse=FALSE,
         key=g1key,
         width.rowname=.5, title="Wg4")

  detach("package:ggplot2") ## can't unload

}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
