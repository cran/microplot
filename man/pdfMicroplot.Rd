\name{pdfMicroplotTrellis}

%% \alias{pdfMicroplotGgplot}
%% \alias{postscriptMicroplotGgplot}
%% \alias{win.metafileMicroplotGgplot}
%% \alias{pngMicroplotGgplot}

\alias{pdfMicroplot_yfactorGgplot}
\alias{postscriptMicroplot_yfactorGgplot}
\alias{win.metafileMicroplot_yfactorGgplot}
\alias{pngMicroplot_yfactorGgplot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal subroutines used by microplot.ggplot and
microplot_yfactor.ggplot to write the individual graphics files.
}
\description{
Internal subroutines used by microplot.ggplot and
microplot_yfactor.ggplot to write the individual graphics files.
}
\usage{
## used by microplot_yfactor.ggplot
pdfMicroplot_yfactorGgplot(
   object, n, is.key, figPrefix, height, width, x.axis, y.axis,
   key, height.key, width.key, ...)
postscriptMicroplot_yfactorGgplot(
   object, n, is.key, figPrefix, height, width, x.axis, y.axis,
   key, height.key, width.key, ...)
win.metafileMicroplot_yfactorGgplot(
   object, n, is.key, figPrefix, height, width, x.axis, y.axis,
   key, height.key, width.key, ...)
pngMicroplot_yfactorGgplot(
   object, n, is.key, figPrefix, height, width, x.axis, y.axis,
   key, height.key, width.key, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Individual filename.  Used by the special case microplot_yfactor.ggplot function.
}
  \item{n}{
Number of panels in the lattice or ggplot array.
}
  \item{is.key}{
Logical.  If \code{TRUE} then key is lattice key (legend).
}
  \item{figPrefix}{
Character string used as prefix for the generated files.
}
  \item{height}{
Height in inches of the generated graphics files.
}
  \item{width}{
Width in inches of the generated graphics files.
}
  \item{x.axis, y.axis}{
Logical.  If \code{TRUE} then write a graphics file containing the
horizontal axis or the vertical axis.  At the moment, the y.axis value
is ignored.
}
  \item{key}{
For \code{"trellis"} objects, a \code{key} as described in
  \code{\link[lattice]{xyplot}}.\cr
For \code{"ggplot"} objects, a legend as described in \code{\link[ggplot2]{guide_legend}}.
}
  \item{height.key}{
Height in inches of key graphics file.  Defaults to same height as the panels.
}
  \item{width.key}{
Width in inches of key graphics file.  Defaults to same width as the panels.
}
  \item{\dots}{
Arguments to panel function, i.e., cex and such for trellis.  Similar
arguments for ggplot.  Currently ignored for the *_yfactorGgplot functions.
}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
