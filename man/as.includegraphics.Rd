\name{as.includegraphics}
\alias{as.includegraphics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert a filename into a complete LaTeX \code{includegraphics} command.
  This is useful for \code{pdf}, \code{png}, and \code{jpeg} files that are recognized by
  the LaTeX \code{includegraphics} macro in the \pkg{graphicx} package
  when used with the \code{pdflatex} command.
  This is useful for \code{bmp} and \code{tiff} files that are recognized by
  the LaTeX \code{includegraphics} macro in the \pkg{graphicx} package
  when used with the \code{latex} command.
}
\description{
Convert a filename into a complete LaTeX includegraphics command.
The directory name is included in the command.  The \code{includegraphics}
macro is generated with the height and optional width.  An optional
raise value is available for vertical alignment.  An optional \code{trim}
argument is available to remove excess margins from the image.  See the
Details section for use of the \code{trim} argument to take panels out
of an externally produced graphics file.
}
\usage{
as.includegraphics(object,
                   height="1em",
                   width=NULL,
                   wd=getwd(),
                   raise=NULL,
                   viewport=NULL, ## if specified, then left bottom right top.
                                  ## used for pdf png jpeg
                   bb=NULL, ## if specified, then left bottom right top.
                            ## used for bmp tiff
                   trim="0 0 0 0", ## left bottom right top
                   clip="true"
                   )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A character vector of filenames for files that contain graphics.
}
  \item{height}{
Character vector containing a LaTeX distance (by default \code{"1em"}).
}
  \item{width}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    Keeping the default keeps the original aspect ratio.
    Specifying a value will stretch the figure unless the height is set
    to \code{NULL}.
}
  \item{wd}{
The directory in which the files reside.  The default is the full path
to the current
working directory that R is using.  The full path is necessary when
using the \code{Hmisc::print.latex} and related functions because they
run the operating system's \code{latex} or \code{pdflatex} command in
a temporary directory.  The relative path to the current directory
(\code{wd="."})
is sufficient if the file will be brought into a larger \code{tex} file
with the LaTeX \code{input} macro.
}
  \item{raise}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    This value may be negative.  Use it if the default vertical
    alignment of the graphs in the table is not satisfactory.
}
\item{viewport}{Size in pixels of the image file.  This is the MediaBox
  in a \code{pdf} file.  It is the number reported by the operating system for a
  \code{png} or \code{jpeg} file.  The \code{viewport} is optional.  When specified it must be a
  character string containing four numbers in order: left, bottom, right, top.}
\item{bb}{Bounding Box: Size in pixels of the image file.  It is the number reported by the operating system for a
  \code{bmp} or \code{jpeg} file.  When specified it must be a
  character string containing four numbers in order: left, bottom, right, top.}
\item{trim}{Size in pixels to be trimmed.  It must be a
  character string containing four numbers in order:
  left, bottom, right, top.  See the manual for the LaTeX
  package \pkg{graphicx} for details.
   See the Details section for additional use of the \code{trim} argument.
}
\item{clip}{Character value \code{"true"} or \code{"false"}.}
}
\value{
A vector of LaTeX statements with the LaTeX macro includegraphics for
the input filenames.
}
\details{
  We recommend that the aspect ratio be controlled by the R functions
  that generated the figure.

  We recommend that only one of the arguments \code{height} and \code{weight} be used
  in \code{as.includegraphics}.  Using both will change the aspect ratio
  and consequently stretch the figure.  The \code{trim} argument is used
  to remove excess margins from the figure.

  Either the \code{viewport} or \code{bb} should be specified, not both.

  The \code{trim} argument can be used to take apart an externally
  produced graphics file and use its components in a LaTeX table.
  See the files \code{examples/irisSweaveTakeApart.Rtex} and\cr
  \code{examples/irisSweaveTakeApart-Distributed.pdf} for an example.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link[Hmisc]{latex}}, \code{\link{microplot}}
  }
\examples{
as.includegraphics("abc.pdf")
## [1] "\\includegraphics[height=1em]{/Users/rmh/abc.pdf}"
## This form, with the full pathname, is required when the Hmisc::print.latex
## and related functions are used to display the current .tex file on screen.

as.includegraphics("abc.pdf", wd=".")
## [1] "\\includegraphics[height=1em]{./abc.pdf}"
## This form, with the relative path, is optional when the .tex file will be
## embedded into a larger file, and will not be displayed on screen.

as.includegraphics(c("abc.pdf", "def.pdf"), raise="-1em")
## [1] "\\raisebox{-1em}{\\includegraphics[height=1em]{/Users/rmh/abc.pdf}}"
## [2] "\\raisebox{-1em}{\\includegraphics[height=1em]{/Users/rmh/def.pdf}}"



## Please see the package documentation ?microplot for a simple example in context.

## Please see the demos for more interesting examples.
## demo(package="microplot")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ documentation }
\keyword{ utilities}
