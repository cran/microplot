\name{MSWord_TablePlusGraphicColumn}
\alias{MSWord_TablePlusGraphicColumn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct an 'MS Word' file containing a table with several columns of
numbers and one column of graphics.
}
\description{
Construct an 'MS Word' file containing a table with several columns of
numbers and one column of graphics.  This function needs to be rewritten
as a method for the generic \code{\link{MSWord}}.
}
\usage{
MSWord_TablePlusGraphicColumn(
   object,
   filetype=c("docx","html"),
   title=Hmisc::first.word(deparse(substitute(object))),
   rowname.header,
   data.header,
   graph.header,
   graph.file.directory="./",
   panel.files, ## make sure they are in the correct order
   axis.files,  ## axis.files["x"] is used.  axis.files["y"] is currently ignored.
   legend.file,
   height=stop("specify height of each panel in inches",
               call.=FALSE),
   width=stop("specify width of each panel in inches",
               call.=FALSE),
   height.key=height, width.key=width, ## inches
   FlexTableWidths, ## inches
   rmh.borders=TRUE,
   caption,
   file=paste0(title, ".", filetype),
   doc.title="Microplot",
   ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Character-valued matrix or data.frame.
}
  \item{title}{
 Name of file to create without the \code{.docx} or \code{.html} extension.
}
  \item{rowname.header}{
Column header for column of rownames.
}
  \item{data.header}{
Group header for set of data columns.
}
  \item{graph.header}{
Column header for columns of graphics.
}
  \item{graph.file.directory}{
    Directory containing the filenames listed in the arguments\cr
    \code{panel.files}, \code{axis.files}, \code{legend.file}.
}
  \item{panel.files}{
Vector of graphics filenames, one for each row of the \code{object}.
}
  \item{axis.files}{
    Vector of filenames for graphics files.
    The file named in \code{axis.files["x"]} contains the common
    horizontal axis of the files in \code{panel.files}.
    The file named in \code{axis.files["y"]} contains the common
    vertical axis of the files in \code{panel.files}.
}
  \item{legend.file}{
Filename for graphics file containing the key (legend) for the files in \code{panel.files}.
}
  \item{height}{
Height in inches of the generated graphics files.
}
  \item{width}{
Width in inches of the generated graphics files.
}
  \item{height.key}{
Height in inches of key graphics file.  Defaults to same height as the panels.
}
  \item{width.key}{
Width in inches of key graphics file.  Defaults to same width as the panels.
}
  \item{FlexTableWidths}{
See \code{\link[ReporteRs]{setFlexTableWidths}}.
}
  \item{rmh.borders}{
The default value \code{TRUE} uses solid borders only to distinguish the
column labels and the row labels from the body of the table.  The
alternate value \code{FALSE} allows the \pkg{ReporteRs} package to construct a
table with solid borders surrounding all cells.
}
  \item{caption}{
Caption used in the generated 'MS Word' table.
}
  \item{filetype}{
Type of file to generate: \code{.docx} for 'MS Word', or \code{.html}
  for 'HTML'.
}
  \item{file}{
Name of the file to create.  The default file name is \code{x.docx} or
\code{x.html} (depending on the value of \code{filetype}) where \code{x}
is the first word in the name of the argument for \code{object}.
}
  \item{doc.title}{
\code{title} required by \code{\link[ReporteRs]{docx}} for the title of
the \code{.docx} file (in the document properties).
}
  \item{\dots}{
Other arguments are ignored.
}
}

\value{
Invisibly, the name of the constructed \code{.docx} or \code{.html} file.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{\code{\link{MSWord}}}

\examples{
\dontrun{
  demo("tablesPlusGraphicColumn", package="microplot", ask=FALSE) ## both "word" and latex
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ html }
