\name{as.includegraphics}
\alias{as.includegraphics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert a filename into a complete LaTeX \code{includegraphics} command.
  This is useful for \code{pdf}, \code{png}, and \code{jpeg} files that are recognized by
  the LaTeX \code{includegraphics} macro in the \pkg{graphicx} package
  when used with the \code{pdflatex} command.
  This is useful for \code{ps}, \code{bmp}, and \code{tiff} files that are recognized by
  the LaTeX \code{includegraphics} macro in the \pkg{graphicx} package
  when used with the \code{latex} command.
}
\description{
Convert a filename into a complete 'LaTeX' \code{includegraphics} command.
The directory name is included in the command.  The \code{includegraphics}
macro is generated with the \code{height} and optional \code{width}.  An optional
\code{raise} value is available for vertical alignment.  An optional \code{trim}
argument is available to remove excess margins from the image.  See the
Details section for use of the \code{trim} argument to trim panels in
an externally produced graphics file.
}
\usage{
as.includegraphics(object,
           height=NULL, ## LaTeX measurement (character)
           width=NULL, ## retains original aspect ratio, LaTeX measurement (character)
           scale=NULL, ## number
           raise=NULL, ## LaTeX measurement (character)
           hspace.left=NULL,  ## LaTeX measurement (character)
           hspace.right=NULL, ## LaTeX measurement (character)
           wd=getwd(), ## working directory
           viewport=NULL, ## if specified, then left bottom right top (character)
           ## used for pdf png jpeg
           ## See MediaBox in pdf file.
           ## Ask operating system for png or jpg file.
           bb=NULL, ## if specified, then left bottom right top (character)
           ## used for bmp tiff ps, ask operating system for values
           trim=NULL, ## for example, "0 0 0 0" left bottom right top (character)
           x.axis=TRUE, ## may be logical or a list of arguments
           y.axis=TRUE, ## may be logical or a list of arguments
           key=FALSE)   ## may be logical or a list of arguments
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A character vector or matrix of filenames for graphics files.
The argument may include attribues
\code{axis.names} and \code{keyname} for graphics files containing the
x.axis, y.axis, and key (legend).
attributes,}
\item{scale}{Scale factor (number) applied to figure.
  If \code{height} or \code{width} is specified, then \code{scale} is ignored.
}
\item{height, width}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    Specifying at most one of these retains the original aspect ratio.
    Specifying a value for both might distort the figure by changing the
    aspect ratio.
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
  \item{hspace.left, hspace.right}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    This value may be negative.  Use it if the default distance on the
    left or right between columns of graphs in the table is not satisfactory.
}
\item{viewport}{Size in pixels of the image file.  This is the MediaBox
  in a \code{pdf} file.  It is the number reported by the operating system for a
  \code{png} or \code{jpeg} file.  The \code{viewport} is optional.  When specified it must be a
  character string containing four numbers in order: left, bottom, right, top.}
\item{bb}{Bounding Box: Size in pixels of the image file.  It is the
  number reported by the operating system for a \code{ps},
  \code{bmp}, or \code{jpeg} file.  When specified it must be a
  character string containing four numbers in order: left, bottom, right, top.}
\item{trim}{Size in pixels to be trimmed.  It must be a
  character string containing four numbers in order:
  left, bottom, right, top.  See the manual for the LaTeX
  package \pkg{graphicx} for details.
   See the Details section for additional use of the \code{trim} argument.
}
\item{x.axis, y.axis, key}{logical, or list of arguments to nested calls
  to as.includegraphics.}
}
\value{
A vector or matrix of 'LaTeX' statements with the 'LaTeX' macro \code{\\includegraphics} for
each of the input filenames.  If the input argument has
\code{axis.names} or \code{keyname}
attributes, then the value will also have those attributes, enclosed in
\code{\\includegraphics} statements.  The arguments allow different
\code{\\includegraphics}
options for the panels, the x.axis, the y.axis, and the key (legend).
}
\details{
  We recommend that the aspect ratio be controlled by the 'R' functions
  that generated the figure.

  We recommend that only one of the arguments \code{height} and \code{weight} be used
  in \code{as.includegraphics}.  Using both will change the aspect ratio
  and consequently stretch the figure.  The \code{trim} argument is used
  to remove excess margins from the figure.

  Either the \code{viewport} or \code{bb} should be specified, not both.

  The \code{trim} argument can be used to take apart an externally
  produced graphics file and use subsets of its area as components in a 'LaTeX' table.
  See the files \code{examples/irisSweaveTakeApart.Rtex} and
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
