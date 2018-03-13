\name{as.includegraphics}
\alias{as.includegraphics}
\alias{as.includegraphics.default}
\alias{as.includegraphics.trellis}
\alias{as.includegraphics.ggplot}
\alias{as.includegraphics.graphicsList}
\alias{as.includegraphics.microplotMatrix}
\alias{as.includegraphics.includegraphicsMatrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert a filename into a complete 'LaTeX' \code{\\includegraphics}
  expression for use with 'LaTeX' \code{\\includegraphics} macro in the
  \pkg{graphicx} package.  This is used for \code{pdf} and \code{png}
  files with the system \code{pdflatex} command.  This is used for
  \code{ps} files with the system \code{latex} command.
}
\description{
  Convert a filename into a complete 'LaTeX' \code{\\includegraphics}
  expression for use with 'LaTeX' \code{\\includegraphics} macro in the
  \pkg{graphicx} package.  This is used for \code{pdf} and \code{png}
  files with the system \code{pdflatex} command.  This is used for
  \code{ps} files with the system \code{latex} command.  The argument
  \code{wd} is included in the pathname in the generated expression.
  The \code{\\includegraphics} macro is generated with the \code{height}
  and optional \code{width} specified by the
  \code{height.includegraphics} and \code{width.includegraphics}
  arguments; the default \code{NULL} means use the values in the
  graphics (\code{pdf}, \code{png}, \code{ps}) files.  If either is
  specified, the other should be left as \code{NULL} to retain the
  original aspect ratio.
  ## An optional \code{raise} value is available for vertical alignment.
  An optional \code{trim} argument is available to remove excess margins
  from the image.  See the Details section for use of the \code{trim}
  argument to trim panels in an externally produced graphics file.
}
\usage{
as.includegraphics(object, ...)

\method{as.includegraphics}{default}(object,
           height.includegraphics=NULL, ## LaTeX measurement (character)
           width.includegraphics=NULL, ## retains original aspect ratio,
           ##                             LaTeX measurement (character)
           scale=NULL, ## number
           raise=NULL, ## LaTeX measurement (character)
           tabularinclude=TRUE,
           hspace.left=NULL,  ## LaTeX measurement (character)
           hspace.right=NULL, ## LaTeX measurement (character)
           wd=getwd(), ## working directory.  No embedded spaces in directory name.
           viewport=NULL, ## if specified, then left bottom right top (character)
           ## used for pdf png jpeg
           ## See MediaBox in pdf file.
           ## Ask operating system for png or jpg file.
           bb=NULL, ## if specified, then left bottom right top (character)
           ## used for bmp tiff ps, ask operating system for values
           trim=NULL, ## for example, "0 0 0 0" left bottom right top (character)
           x.axis.includegraphics=TRUE, ## logical or a list of arguments
           ##                              to latex \includegraphics[here]{}
           y.axis.includegraphics=TRUE, ## logical or a list of arguments
           xlab.includegraphics=FALSE,  ## logical or a list of arguments
           ylab.includegraphics=FALSE,  ## logical or a list of arguments
           key.includegraphics=!is.null(attr(object, "key.name")),
           ##                           ## logical or a list of arguments
           as.attr=FALSE, ## logical
           label.x.axis="", ## empty, nchar=0
           label.y.axis=" ", ## one space, nchar=1
           columnKey=NULL, ## see ?microplotAttrDisplay
           ...)

\method{as.includegraphics}{microplotMatrix}(object, ...) ## principal usage.  Calls default.

\method{as.includegraphics}{includegraphicsMatrix}(object, ...) ## returns object

\method{as.includegraphics}{trellis}(object, ...) ## generates an informative error message.

\method{as.includegraphics}{ggplot}(object, ...) ## generates an informative error message.

\method{as.includegraphics}{graphicsList}(object, ...) ## generates an informative error message.


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A \code{"microplotMatrix"}, that is
    a character vector or matrix of filenames for graphics files.  The
    argument may include attributes \code{axis.names}, \code{lab.names}, and
    \code{key.name} for graphics files containing the \code{"x.axis"},
    \code{"y.axis"}, \code{"xlab"}, \code{"ylab"}, and \code{"key"} (legend)
    panels.
  }
\item{scale}{Scale factor (number) applied to figure.
  If either \code{height.includegraphics} or \code{width.includegraphics} is specified, then \code{scale} is ignored.
}
\item{height.includegraphics, width.includegraphics}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    Specifying at most one of these retains the original aspect ratio.
    Specifying a value for both might distort the figure by changing the
    aspect ratio. Specifying \code{trim} on height of a panel requires
    a new height to be specified to retain the aspect ratio.
    Specifying \code{trim} on width of a panel requires
    a new width to be specified to retain the aspect ratio.
    See \code{demo("latex")} and \code{demo("latex-ggplot")} for an example.
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
with the LaTeX \code{\\input} macro.  Should the working directory have an
embedded blank anywhere in its pathname, then \code{as.includegraphics} will
generate an informative error.  This is to protect you from a
less-informative error that the system 'latex' command would otherwise generate.
The recommended repair is to \code{setwd()} to a directory whose path has no
embedded blanks anywhere.  A workaround is to use \code{wd="."} in the
\code{latex} call.  Automatic printing with  \code{Hmisc::print.latex}
will not work.  \code{\\input\{\}} of the generated
\code{.tex} file into your larger \code{.tex} will work.  Moving the
generated \code{.tex} file in the temporary directory to your working directory
will work.
}
  \item{raise}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    This value may be negative.  Use it if the default vertical
    alignment of the graphs in the table is not satisfactory.
    Usually a better approach would be to use the \code{arraystretch}
    argument to \code{\link{latex.trellis}}.
}
\item{tabularinclude}{Logical. When \code{TRUE} place the generated
  \code{\\includegraphics\{\}} statements inside a \code{tabular}
  environment.
This makes the center of the included graphic align with the text on the
same line of the tabular environment.}
\item{hspace.left, hspace.right}{
    Character vector containing a LaTeX distance (by default \code{NULL}).
    This value may be negative.  Use it if the default distance on the
    left or right between columns of graphs in the table is not satisfactory.
}
\item{viewport}{Size in pixels of the image file.  This is the MediaBox
  in a \code{pdf} file.  It is the number reported by the operating system for a
  \code{png} file.  The \code{viewport} is optional.  When specified it must be a
  character string containing four numbers in order: left, bottom, right, top.}
\item{bb}{Bounding Box: Size in pixels of the image file.  It is the
  number reported by the operating system for a \code{ps} file.
  When specified it must be a
  character string containing four numbers in order: left, bottom, right, top.}
\item{trim}{Size in pixels to be trimmed.  It must be a character string
  containing four numbers in order: left, bottom, right, top.  See the
  manual for the LaTeX package \pkg{graphicx} for details.  When
  \code{trim} is used, either \code{height.includegraphics} or
  \code{width.includegraphics} will also need to be changed.
  See \code{demo("latex")} and \code{demo("latex-ggplot")} for an example.
  See the Details section for additional use of the \code{trim} argument.
}
\item{x.axis.includegraphics, y.axis.includegraphics}{logical,
  or list of arguments to nested calls to as.includegraphics.}
\item{xlab.includegraphics, ylab.includegraphics,
  key.includegraphics}{logical, or list of arguments to nested calls
  to as.includegraphics.}
\item{as.attr}{Logical.  When \code{TRUE} the attributes in the
  \code{"microplotMatrix"} argument become
  attributes in the \code{"includegraphicsMatrix"} result.
  When \code{FALSE}, the \code{label.x.axis}, \code{label.y.axis}, and \code{columnKey}
  arguments are passed through to \code{\link{microplotAttrDisplay}}.}
\item{label.x.axis, label.y.axis}{Labels that will used by
  \code{\link{microplotAttrDisplay}} in the column
  name of the \code{y.axis} and the \code{y.axis} position for the
  \code{x.axis} in the 'latex' display of the graphic.}
  \item{columnKey}{
If \code{as.attr} is \code{FALSE} and the key in \code{attr(object,
  "key.name")} is non-null, then \code{\link{microplotAttrDisplay}}
will place its \code{key.name} as a new last value in
the specified columns.  The column numbering is with respect to the
input \code{object}, before the \code{y.axis} or \code{ylab} are evaluated.
}
\item{\dots}{
Other arguments currently ignored.
}
}
\value{
  A \code{"includegraphicsMatrix"} object, a vector or matrix of 'LaTeX'
  expressions with the 'LaTeX' macro
  \code{\\includegraphics} for each of the input filenames.  If the
  input argument has \code{axis.names} or \code{lab.names} or
  \code{key.name} attributes, then the value will also have those
  attributes, enclosed in \code{\\includegraphics} statements.  The
  arguments allow different \code{\\includegraphics} options for the
  panels, the x.axis, the y.axis, xlab, ylab, and the key (legend).
  The location of the
  files listed in the input argument attributes depends on the value of
  the \code{as.attr} argument.  When \code{as.attr} is \code{TRUE} the
  \code{object} attributes will become \code{result} attributes.  When
  \code{as.attr} is \code{FALSE}, see the \link{microplotAttrDisplay} for
  details.
}
\details{
  We recommend that the aspect ratio be controlled by the 'R' functions
  that generated the figure.  \code{as.includegraphics} will use the
  height and width values that are encoded in the \code{pdf},
  \code{png}, \code{ps} files.
  If you need to change the size of the image
  we recommend that at most one of
  \code{height.includegraphics}
  and \code{width.includegraphics} be used
  in \code{as.includegraphics}.  Using both will change the aspect ratio
  and consequently stretch the figure.  The \code{trim} argument is used
  to remove excess margins from the figure; when \code{trim} is
  specified for height or width, the \code{height.includegraphics} or
  \code{width.includegraphics} will also need to be specified
  to retain the aspect ratio.
  See \code{demo("latex")} and \code{demo("latex-ggplot")} for an example.

  Either the \code{viewport} (for \code{pdf} or \code{png} files) or
  \code{bb} (for \code{ps} files) should be specified, not both.

  The \code{trim} argument can be used to take apart an externally
  produced graphics file and use subsets of its area as components in a 'LaTeX' table.
  See the files \code{examples/irisSweaveTakeApart.Rtex} and
  \code{examples/irisSweaveTakeApart-Distributed.pdf} for an example.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link{latex.trellis}}, \code{\link{microplot}}, \code{\link[Hmisc]{latex}}
  }
\examples{
as.includegraphics("abc.pdf")
## [1] "\\setlength{\\tabcolsep}{0pt}\\begin{tabular}{c}
##      \\includegraphics{/Users/rmh/Rwd/abc.def}\\end{tabular}"
## attr(,"class")
## [1] "includegraphicsMatrix" "character"
## This form, with the full pathname, is required when the Hmisc::print.latex
## and related functions are used for automatic display of
## the current .tex file on screen.

as.includegraphics("abc.pdf", wd=".")
## [1] "\\setlength{\\tabcolsep}{0pt}\\begin{tabular}{c}
##      \\includegraphics{./abc.pdf}\\end{tabular}"
## attr(,"class")
## [1] "includegraphicsMatrix" "character"
## This form, with the relative path, is optional when the .tex file will be
## embedded into a larger file, and will not be automatically displayed on screen.


## Please see the package documentation ?microplot for a simple example in context.

## Please see the demos for more interesting examples.
## demo(package="microplot")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ documentation }
\keyword{ utilities}
