\name{msWord}
\alias{msWord}
\alias{msWord.microplotMatrix}
\alias{msWord.trellis}
\alias{msWord.ggplot}
\alias{msWord.graphicsList}
\alias{msWord.graphicsClass}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display a table in 'MS Word' containing panels from \R graphs in
its cells.
}
\description{
Display a table in 'MS Word' containing panels from \R graphs in
its cells.
msWord methods for "trellis", "ggplot", "graphicsList",
"microplotMatrix", and "includegraphicsMatrix" objects.
The output file can be a \code{.docx} or \code{.html} file.
}
\usage{
msWord(object, ...)

\method{msWord}{microplotMatrix}( ## calls msWord.graphicsClass
       object, ## microplotMatrix
       ## (matrix of filenames containing individual panels)
       ## (0 columns permitted)
       filetype=c("docx","html"),
       dataobject=matrix(, nrow(object), 0), ## numeric or character matrix
       data.first=TRUE,
       title=first.word(deparse(substitute(object))),
       rowlabel=title,
       rowname=rownames(object),
       data.header="data",
       graph.header="graph",
       graph.file.directory="./",
       axis.files=attr(object,"axis.names"),
       lab.files=attr(object,"lab.names"),
       key.file=attr(object,"key.name"),
       x.axis=(!is.null(axis.files) && !is.null(axis.files["x"])),
       y.axis=(!is.null(axis.files) && !is.null(axis.files["y"])),
       xlab=FALSE,
       ylab=FALSE,
       label.x.axis="",  ## empty, nchar=0
       label.y.axis=" ", ## one space, nchar=1
       height.panel=1, ## inches
       width.panel=1, ## inches
       height.x.axis=height.panel[1], ## inches ## [1] is defensive for lazy evaluation
       width.y.axis=width.panel[1], ## inches
       height.xlab=height.panel[1], ## inches
       width.ylab=width.panel[1], ## inches
       height.key=height.panel[1], ## inches
       width.key=width.panel[1], ## inches
       FlexTableWidths=NULL, ## inches ## value used will be an attribute of result
       rmh.borders=TRUE,
       caption=NULL,
       file=paste0(title, ".", filetype),
       doc.title="Microplot",
       width.rowname=.4,
       width.dataobject=1,
       width.between=.1,
       landscape=FALSE,
       rgroup=NULL,
       n.rgroup=NULL,
       rgroup.exclude.borders=NULL,
       key.align="center",  ## flextable::align
       ...)

\method{msWord}{graphicsClass}( ## calls msWord.graphicsClass
           object, ## called by trellis, ggplot, graphicsList methods
           ## microplot arguments
           figPrefix=first.word(deparse(substitute(object))),
           device="png",
           key=FALSE,
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## microplot,
           ## msWord.microplotMatrix
           )

\method{msWord}{trellis}( ## calls msWord.graphicsClass
           object=stop("trellis object is required", call. = FALSE),
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix,
           ... ## can include arguments to
           ## msWord.graphicsClass,
           ## microplot,
           ## msWord.microplotMatrix
           )

\method{msWord}{ggplot}(object=stop("ggplot object is required", call. = FALSE),
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix,
           ... ## can include arguments to
           ## msWord.graphicsClass,
           ## microplot,
           ## msWord.microplotMatrix
           )

\method{msWord}{graphicsList}(object=stop("graphicsList object is required", call. = FALSE),
           ## matrix or vector of trellis objects or ggplot objects,
           ## with dim and dimnames,
           ## normally each containing one panel.
           ## The axes and key will be taken from object[[1]].
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix,
           ... ## can include arguments to
           ## msWord.graphicsClass,
           ## microplot,
           ## msWord.microplotMatrix
           )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{"microplotMatrix"} (Matrix of filenames containing individual panels) (0 columns permitted).
}
  \item{filetype}{
File extension of generated file.  Character, either \code{"docx"} or
\code{"html"}.
For \code{filetype="docx"} the result is a variable containing the
filename of a generated \code{MS Word} file.  See the discussion of
\code{value}
below for additional information on the value.
For \code{filetype="html"} the result is a variable containing a
\code{flextable} object.  See the discussion of \code{value} below to learn
how to save the \code{html} file.
}
  \item{dataobject}{
Numeric or character matrix (or data.frame).
}
  \item{data.first}{
Logical.  If \code{TRUE}, then output file will have \code{dataobject} columns
first, then graphics \code{object} columns.
If \code{FALSE}, then output file will have graphics \code{object} columns
first, then \code{dataobject} columns.
}
  \item{title}{
Basename of generated file.  We replace all \code{"."} in
    the \code{title} by \code{"-"}.
}
\item{figPrefix}{See \code{\link{microplot.trellis}}.
    The 'latex' macro \code{\\includegraphics} requires that there be no
    \code{"."} in the filename basename.  We also enforce this
    requirement for
    \code{msWord}.  We replace all \code{"."} in
    the \code{figPrefix} by \code{"-"}.
}
  \item{rowlabel}{
Header name for column of rownames in the generated file.
}
\item{rowname}{Rownames of constructed table. Default is rownames of
          \code{object}.  Specify \code{rowname=NULL} to suppress the
          use of row names.}
  \item{data.header, graph.header}{
Header names for groupings of data columns and graph columns.  Used
when both \code{dataobject} and \code{object} are present, one of them is not missing.
}
  \item{graph.file.directory}{
Directory containing files named in \code{object}.
}
  \item{axis.files, lab.files, key.file}{Filenames for graphics files containing
  axes, axis labels, and key (legend).
}
\item{x.axis, xlab}{Logical.  If \code{x.axis==TRUE} each column of
graphs will have the \code{x.axis} file placed as the last item in that column.
If both are \code{TRUE} then the \code{xlab} file will be placed in the row
following the \code{x.axis} file.}
\item{y.axis, ylab}{Logical.  If \code{y.axis==TRUE} each row of graphs
will have the \code{y.axis} file placed as the first item in that row.
If both are \code{TRUE}, then the \code{ylab} file will be placed on each
row immediately before the \code{y.axis} file.}
\item{label.x.axis, label.y.axis}{Labels that will used in the column
  name of the \code{y.axis} and the \code{y.axis} position for the
  \code{x.axis} in the 'latex' display of the graphic.}
\item{height.panel, width.panel, height.x.axis, width.y.axis,
  height.xlab, width.ylab}{See \code{\link{microplot}}.}
\item{height.key, width.key, device, key}{See \code{\link{microplot}}.}
\item{FlexTableWidths}{
Widths of all columns, including header and between columns, in inches.
The default is to base the widths on the widths of individual columns
  above.
The actual widths used are returned as an attribute of the returned
filename.
You may wish to examine these values from the first run, and then modify
them on second and later runs.
}
  \item{rmh.borders}{
My preferences for borders on cells based on the American Statistical
Association (\url{https://academic.oup.com/DocumentLibrary/JSSMET/ASASTYLE_GUIDE.pdf})
style sheet.  The \pkg{flextable} default, with full
borders between rows, does not conform.
}
  \item{caption}{
The table will be rendered with a numbered caption containing this
  string as the caption value.
}
  \item{file}{
Name of generated file.
}
  \item{doc.title}{
Title that appears in the MS Word Properties list.
}
  \item{width.rowname}{
Number of inches for the rowname column.
}
  \item{width.dataobject}{
Number of inches for each column In the \code{data.object}.
}
  \item{width.between}{
Number of inches for the column between the object (graphs) and the
  dataobject (numbers or text).
}
  \item{landscape}{
Logical.  If \code{TRUE} then the table is produced in a landscape
  orientation.  If \code{FALSE}, then in a portrait orientation.
}
  \item{rgroup, n.rgroup, rgroup.exclude.borders}{
Argument names borrowed from \code{\link[Hmisc]{latex}}.
\code{rgroup} gives the names of groupings of rows in the table.
\code{n.rgroup} gives the number of rows within each group.
\code{rgroup.exclude.borders} is not borrowed. It gives the row
numbers of rows which are not to have a border between the rowname and
  the body of the table.
}
\item{key.align}{\code{flextable::align} one of \code{left}, \code{right}, \code{center}.}
%% \item{x}{\code{print} methods require that their argument be named \code{x}.}
  \item{\dots}{
Additional arguments are currently ignored by
\code{msWord.microplotMatrix}.
They are forwarded to other methods by the other functions documented here.
}
}
\note{
The \code{msWord} function uses facilities provided by the
  \pkg{flextable} and \pkg{officer} packages.
}
\value{
  The returned value depends on the \code{filetype} argument.

  For \code{filetype="docx"}, the value is the name of a generated file
  \code{docx} file with class \code{"msWordFilename"}.  Printing the
  returned value will display the generated file on screen.  The result
  has an attribute \code{"microplotMatrix"} containing its argument
  \code{object}, which contains the names of the generated microplots.
  The result has an attribute showing the actual \code{FlexTableWidths}
  used.  The user may wish to use the \code{FlexTableWidths} argument on
  a following run to modify these values.


  For \code{filetype="html"}, the value is a \code{\link[flextable]{flextable}} object.
  Printing the returned value will display the generated table in a
  browser window.
   If you wish to keep the html object, you MUST save it manually!
 The original file is in a temporary directory and will vanish when R is closed.
 Switch to the browser window and save the displayed table as a 'Web Archive'
 in a directory of your choice.  Choose an appropriate basename for the saved file,
 as the default basename of the file is the noninformative \code{"index"}.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## See demos.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ documentation }% use one of  RShowDoc("KEYWORDS")
\keyword{ utilities }% __ONLY ONE__ keyword per line
