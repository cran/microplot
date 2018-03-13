\name{microplot}
\alias{microplot}
\alias{microplot.trellis}
\alias{microplot.ggplot}
\alias{microplot.graphicsList}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Take a trellis or ggplot object, or graphicsList object (list of
  trellis or ggplot objects), and generate a set of graphics files, one
  per panel of a multi-panel display.
}
\description{
  Take a trellis or ggplot object, or graphicsList object (list of
  trellis or ggplot objects), and generate a set of graphics files, one
  per panel of a multi-panel display.
 Additional files are generated for the axes, the axis labels, and the key.

This help file documents the \code{microplot} function.  See
\code{\link{microplot-package}}
for information on the entire \pkg{microplot} package.

}
\usage{
microplot(object, ...)

## complete for an array of lattice panels
\method{microplot}{trellis}(
          object=stop("trellis object is required", call. = FALSE),
          ## object must have class "trellis"
          figPrefix=first.word(deparse(substitute(object))),
          vectorgraph.colname=figPrefix,
          device=c("pdf","postscript","ps","png"),
          res=600, type=getOption("bitmapType"), ## used by png
          height.panel=1, width.panel=1, ## numeric in inches
          collapse=layoutCollapse, ## Zero out unwanted
                                   ## layout.heights and layout.widths.
                                   ## See below for example.
          height.x.axis=height.panel,
          axis.line=list(col="black"),
          xaxis.line=axis.line,
          par.settings.x.axis=
            list(layout.heights=list(panel=0, axis.bottom=1,
                                     axis.xlab.padding=0, xlab=0),
                 axis.line=xaxis.line),
          width.y.axis=width.panel,
          yaxis.line=axis.line,
          par.settings.y.axis=
            list(layout.widths=list(ylab=0, ylab.axis.padding=0,
                                    axis.left=1, panel=0),
                 axis.line=yaxis.line),
          height.xlab=height.panel,
          par.settings.xlab=
            list(layout.heights=list(panel=0, axis.bottom=0,
                                     axis.xlab.padding=0, xlab=1),
                 axis.line=list(col="transparent")),
          width.ylab=width.panel,
          par.settings.ylab=
            list(layout.widths=list(ylab=1, ylab.axis.padding=0,
                                    axis.left=0, panel=0),
                 axis.line=list(col="transparent")),
          key=FALSE,    ## FALSE or a list of arguments defining a key
          height.key=height.panel, width.key=width.panel,
          ...)  ## needed to match generic.  ignored in the trellis method


\method{microplot}{ggplot}(object, ## object has class "ggplot"
           collapse=theme_collapse(), ## theme_collapse(...) ?
           figPrefix=first.word(deparse(substitute(object))),
           vectorgraph.colname=figPrefix,
           height.panel=1, ## inch
           width.panel=1,  ## inch
           height.x.axis=height.panel,
           width.y.axis=width.panel,
           height.xlab=height.panel,
           width.ylab=width.panel,
           height.key=height.panel,
           width.key=width.panel,
           tick.text.size=7,
           key=FALSE,  ## FALSE, or a ggplot object which is a valid key
           device=c("pdf","postscript","ps","png"),
           res=600, type=getOption("bitmapType"), ## used by png
            ...)

\method{microplot}{graphicsList}(object, ## an array of identically structured,
           ## single-panel, graphics objects (trellis or ggplot)
           ## with dim and dimnames
           figPrefix=first.word(deparse(substitute(object))),
           device=c("pdf","postscript","ps","png"),
           res=600, type=getOption("bitmapType"), ## used by png
           height.panel=1, width.panel=1, ## numeric in inches
           key=FALSE,  ## FALSE, or a trellis or ggplot object which is a valid key
           height.key=height.panel, width.key=width.panel,
           ## valid arguments for microplot.trellis or microplot.ggplot
           ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{"trellis"}, or \code{"ggplot"}, or a
\code{graphicsList} object of either all similarly constructed
\code{"trellis"} objects of all similarly constructed \code{ggplot} objects.
}
\item{collapse}{
  Function that zeros out unwanted \code{layout.heights} and
  \code{layout.widths} space for \code{"trellis"} objects
  (see \code{\link{layoutCollapse}}), or that specifies a \code{"theme"}
  for \code{ggplot} objects (see \code{\link{theme_collapse}}).
}
  \item{figPrefix}{
    Character string used as prefix for the generated files.
    The 'latex' macro \code{\\includegraphics} requires that there be no
    \code{"."} in the filename basename.  We replace all \code{"."} in
    the \code{figPrefix} by \code{"-"}.
}
\item{vectorgraph.colname}{
Character string used as column name when a vector of filenames is
converted to a column matrix of filenames.
}

\item{height.panel, width.panel}{
Height and width in inches of the generated graphics files.
}

\item{height.x.axis, width.y.axis}{Dimensions for axis
  graphics files---usually smaller than for panel contents.}

\item{height.xlab, width.ylab}{Dimensions for
  graphics files containing axis labels---usually smaller than for panel contents.}

\item{par.settings.x.axis, par.settings.y.axis, par.settings.xlab,
  par.settings.ylab}{\code{"trellis"} only:
  \code{par.settings} for axis and xlab files.
 }

\item{axis.line, xaxis.line, yaxis.line}{\code{"trellis"} only: the usual \code{lwd},
  \code{col}, \code{cex} and such that could be defined in the
  \code{scales} argument for \code{xyplot}.}

\item{key}{
    Logical or list (for lattice) or ggplot object (for ggplot).
    If logical and \code{FALSE} there is no key (legend).
    If a list for \code{microplot.trellis}, it must be defined as described in
    \code{\link[lattice]{xyplot}} for \code{"trellis"} objects.
    If a \code{"ggplot"} object for \code{microplot.ggplot} it must be a
    valid \code{ggplot} object and will be displayed in the location
    appropriate for a legend.
}

\item{height.key, width.key}{
Height and width in inches of key graphics file.  Defaults to same
height and width as the panels.
}
\item{device}{
  Function used to construct the graphics files.

  For \code{latex} with \code{(options("latexcmd")=="pdflatex")} use
  \code{"pdf"} (the default for \code{pdflatex}).

  For \code{latex} with \code{(options("latexcmd")=="latex")} use
  \code{"postscript"} (the default for \code{latex}).
  \code{"ps"} is equivalent to \code{"postscript"}.

  For \code{MSWord} use \code{"png"}.  \code{png} defaults to
  \code{res=600, type=getOption("bitmapType")}.
}
\item{res, type}{\code{res} is nominal resolution in ppi.  \code{type}
  is either operating system-specific or \code{"cairo"}.  See
  \code{\link[grDevices]{png}} and \code{\link[grDevices]{cairo}}.}
%% \item{n}{
%%     Number of panels in the ggplot array.\cr
%%     Used only in the
%% \code{microplot_yfactor.ggplot} method.  The other methods determine
%% \code{n} from the \code{dim} of the \code{object} argument.
%% }
%% \item{xlim, ylim}{x limits and y limits of the data.  This is required
%%   in \code{microplot.ggplot}
%%   to enforce the same scale in all panels.}
\item{tick.text.size}{Text size of the tick labels in the x and y axes
  (\code{microplot.ggplot}).}
\item{\dots}{
Arguments to panel function, i.e., \code{cex} and such for \pkg{lattice}.  Similar
arguments for \pkg{ggplot}.  Currently ignored for the
\code{microplot.ggplot} function.
}
}
\value{
Matrix of filenames with same \code{dim} and \code{dimnames} as the
argument \code{object}.  The result has class \code{"microplotMatrix"}.
There may be one or more attributes.
\item{\code{axis.names}}{contains filenames for the x and y axes.}
\item{\code{lab.names}}{contains filenames for the xlab and ylab.}
\item{\code{key.name}}{contains the filename for key (legend).}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link{latex.trellis}}, \code{\link{as.includegraphics}},
  \code{\link[Hmisc]{latex}}, \code{\link{msWord}}
}
\examples{
## See demos
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
