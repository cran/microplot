\name{microplot}
\alias{microplot}
\alias{microplot.trellis}
\alias{microplot.ggplot}
\alias{microplot_yfactor}
\alias{microplot_yfactor.ggplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Take a trellis or ggplot object and generate a set of graphics files,
one per panel of a multi-panel display.
}
\description{
Take a trellis or ggplot object and generate a set of graphics files,
one per panel of a multi-panel display.  \code{microplot.trellis} is
complete.
\code{microplot.ggplot} at present is a placeholder.  Full development
requires more familiarity with ggplot than I have.
\code{microplot_yfactor.ggplot} is a special-case example for a
one-dimensional array of ggplot panels.

This help file documents the \code{microplot} function.  See
\code{\link{microplot-package}}
for information on the \pkg{microplot} package.

}
\usage{
microplot(object, ...)

## complete for an array of lattice panels
\method{microplot}{trellis}(
          object=stop("trellis object is required", call. = FALSE),
          ## object must have class "trellis"
          figPrefix=Hmisc::first.word(deparse(substitute(object))),
          device=c("pdf","postscript","png",
                 ## other devices require a user-written function
                   if (version$os == "mingw32") "win.metafile"),
          height=1, width=1, ## numeric in inches
          collapse=layoutCollapse, ## Zero out unwanted
                                   ## layout.heights and layout.widths.
                                   ## See below for example.
          height.x.axis=height,
          par.settings.x.axis=
            list(layout.heights=list(panel=0, axis.bottom=1,
                                     axis.xlab.padding=1, xlab=1),
                 axis.line=list(col="black")),
          width.y.axis=width,
          par.settings.y.axis=
            list(layout.widths=list(ylab=1, ylab.axis.padding=1,
                                    axis.left=1, panel=0),
                 axis.line=list(col="black")),
          key=FALSE,    ## FALSE or a list of arguments defining a key
          height.key=height, width.key=width,
          ...)  ## needed to match generic.  ignored in the trellis method

## placeholder until someone designs a fully general method for ggplot.
\method{microplot}{ggplot}(object,
          collapse=NULL, ## this doesn't work yet
          figPrefix = Hmisc::first.word(deparse(substitute(object))),
          height = stop("Specify height of each panel in inches.", call.=FALSE),
          width  = stop("Specify width of each panel in inches.",  call.=FALSE),
          x.axis=TRUE, y.axis=FALSE,
          key = FALSE,
          height.key = height,
          width.key = width,
          device = c("pdf", "postscript", "png",
                     if (version$os == "mingw32") "win.metafile"),
          ...)

## special-case for a one-dimensional array of ggplot panels.
microplot_yfactor(object, ...)
\method{microplot_yfactor}{ggplot}(object,
                  obji = object,
                  figPrefix = Hmisc::first.word(deparse(substitute(object))),
                  height =
                    stop("Specify height of each panel in inches.", call.=FALSE),
                  width  =
                    stop("Specify width of each panel in inches.",  call.=FALSE),
                  x.axis=TRUE, y.axis=FALSE,
                  key = FALSE,
                  height.key = height,
                  width.key = width,
                  device = c("pdf", "postscript", "png",
                             if (version$os == "mingw32") "win.metafile"),
                  n,
                  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{"trellis"} object or (eventually) \code{"ggplot"} object with an
array \code{layout}.
}
\item{collapse}{
  Function that zeros out unwanted \code{layout.heights} and \code{layout.widths} space.
}
  \item{obji}{
Eventually \code{"ggplot"} object with an
array \code{layout=c(1,1)}, with all margins reduced to 0, and with all
labels suppressed.
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
\item{height.x.axis, par.settings.x.axis, width.y.axis,
  par.settings.y.axis}{Dimensions and \code{par.settings} for axis
  graphics files are usually different than for panel contents.}
\item{x.axis, y.axis}{no longer used in \code{microplot.trellis} because
graphics files for the x.axis and y.axis are now always written.  Still
used in the ggplot method because they haven't been rewriteen yet.
 }
  \item{key}{
Logical or list.  If logical and \code{FALSE} there is no key (legend).
If a list, it must be defined as described in
\code{\link[lattice]{xyplot}} for \code{"trellis"} objects and as
described in \pkg{\link{ggplot2}} for \code{"ggplot"} objects.
}
  \item{height.key}{
Height in inches of key graphics file.  Defaults to same height as the panels.
}
  \item{width.key}{
Width in inches of key graphics file.  Defaults to same width as the panels.
}
  \item{device}{
    Function used to construct the graphics files.

    For \code{latex} with \code{(options("latexcmd")=="latex")} use
\code{"pdf"} (the default).

For \code{latex} with \code{(options("latexcmd")=="tex")} use \code{"postscript"}.

For \code{MSWord} use \code{"png"}, or on 'Windows' \code{"win.metafile"}.
  }
\item{n}{
    Number of panels in the ggplot array.\cr
    Used only in the
\code{microplot_yfactor.ggplot} method.  The other methods determine
\code{n} from the \code{dim} of the \code{object} argument.
}
  \item{\dots}{
Arguments to panel function, i.e., \code{cex} and such for \pkg{lattice}.  Similar
arguments for \pkg{ggplot}.  Currently ignored for the \code{microplot_yfactor.ggplot} function.
}
}
\value{
Array of filenames with same \code{dim} and \code{dimnames} as the argument \code{object}.
There may one or two attributes
\itemize{
\item \code{axis.name } Filename of graphics files containing the
horizontal and vertical axes.
\item \code{key.name  } Filename of graphics file containing the key (legend).
}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
## See demos
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
