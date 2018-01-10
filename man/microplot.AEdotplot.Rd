\name{microplot.AEdotplot}
\alias{microplot.AEdotplot}
\alias{latex.AEdotplot.microplot}
\alias{MSWord.AEdotplot.microplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Take apart a \code{"AEdotplot"} object and construct a set of
graphics files each of the Adverse Events.
}
\description{
Take apart a \code{"AEdotplot"} object and construct a set of
graphics files each of the Adverse Events.
}
\usage{
\method{microplot}{AEdotplot}(object,
          figPrefix = Hmisc::first.word(substitute(object)),
          title = "Adverse Event",
          main = attr(object, "main")[[1]],
          sub = attr(object, "sub")[[1]],
          height = 0.2,
          width.pct = 2,
          width.RR = 1.5,
          key = attr(object, "ae.key"),
          device = c("pdf", "postscript", "png",
                     if (version$os == "mingw32") "win.metafile"),
          ...)

\method{latex}{AEdotplot.microplot}(object, ...)

\method{MSWord}{AEdotplot.microplot}(object,
       filetype=c("docx","html"),
       graph.file.directory = "./",
       width.pct = attr(object, "width.pct"),
       width.RR = attr(object, "width.RR"),
       height = attr(object, "height"),
       title = attr(object, "title"),
       caption = attr(object, "caption"),
       figPrefix = attr(object, "figPrefix"),
       rmh.borders = TRUE,
       FlexTableWidths = c(1.5, 0.6, 0.6, 0.6, 0.6, 0.7, 2.1, 1.6),
       file=paste0(figPrefix, ".", filetype),
       ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    An \code{"AEdotplot"} object constructed by the \code{\link[HH:ae.dotplot7a]{AEdotplot}} function.
}
  \item{\dots}{ Additional arguments.\cr
    For \code{microplot.AEdotplot}, ignored.\cr
    For \code{latex.AEdotplot.microplot}, forwarded to the
    \code{\link[Hmisc]{latex}} function.\cr
    For \code{MSWord.AEdotplot.microplot}, ignored.
}
  \item{figPrefix}{
Character string used as prefix for the generated files.
}
  \item{title}{
Column head for the rownames of the generated table.
}
  \item{main, sub, key}{
Standard \code{\link[lattice]{xyplot}} arguments.
}
  \item{height}{
Height in inches of the generated graphics files.
}
  \item{width.pct}{
Width in inches of the generated graphics files for the "Percent" column.
}
  \item{width.RR}{
Width in inches of the generated graphics files for the "Relative Risk" column.
}
  \item{device}{
Function used to construct the graphics files.
}
  \item{graph.file.directory}{
MSWord method only.  The \pkg{ReporteRs} function needs this to be explicit.
}
  \item{caption}{
Caption used in the generated 'MS Word' table.
}
  \item{rmh.borders}{Logical.
The default value \code{TRUE} uses solid borders only to distinguish the
column labels and the row labels from the body of the table.  The
alternate value \code{FALSE} allows the \pkg{ReporteRs} package to construct a
table with solid borders surrounding all cells.
}
  \item{FlexTableWidths}{
See \code{\link[ReporteRs]{setFlexTableWidths}}.
}
  \item{file}{
Name of the file to create.
}
  \item{filetype}{
\code{MSWord} only. Type of file to generate: \code{.docx} for 'MS Word', or \code{.html}
  for 'HTML'.
}
}
\details{
The \code{\link{microplot.AEdotplot}} function does most of the work for
the \code{latex.AEdotplot} and\cr
\code{MSword.AEdotplot} functions.  This
       \code{microplot} method takes apart the
\code{"AEdotplot"} object, constructs the set of graphics files, and
returns the data.frame containing the tabled data and the set of
graphics files.  The data.frame is then sent to either
 \code{\link[Hmisc]{latex}} or to \code{\link{MSWord.AEdotplot.microplot}}.

 The calling sequence of functions is
 \preformatted{
 a. latex.AEdotplot
   b. microplot.AEdotplot
      c. microplot.trellis
   b. latex.AEdotplot.microplot
      c. single call to latex.default with an argument list into the
          Hmisc::latex function tells Hmisc::latex what work to do.
##
a. MSWord.AEdotplot
   b. microplot.AEdotplot
      c. microplot.trellis
   b. MSWord.AEdotplot.microplot
      c. direct calls to many functions (FlexTable, textPoperties,
         parProperties, pot_img, setFlexTableBorders, borderNone,
         borderProperties, cellProperties, setFlexTableWidths, docx,
         addParagraph, addFlexTable, writeDoc) inside the ReporteRs
         package.  Unlike the latex.AEdotplot function, which uses an
         argument list into Hmisc::latex for organization, the
         MSWord.AEdotplot function must do the work itself with the
         primitives supplied by ReporteRs.
}
}
\value{
For \code{microplot.AEdotplot}, a data.frame of tabular values and
filenames. The data.frame has attributes
\itemize{
\item \code{figPrefix}, \code{title}, \code{sub}, \code{height},
      \code{width.pct}, \code{width.RR}, \code{device   }
   Copied from the arguments to \code{microplot}.

\item \code{caption   } Copied from the \code{"main"} argument to \code{microplot}.

\item \code{cgroup}, \code{n.cgroup}, \code{rgroup}, \code{n.rgroup   }
   Values appropriate to the \code{"AEdotplot"} object that will be
   passed forward to the \code{latex.AEdotplot} method or the
   \code{MSWord.AEdotplot} method.  These variables are documented in \code{\link[Hmisc]{latex}}.
 }

For \code{latex.AEdotplot.microplot}, a \code{"latex"} object containing
the filename of the file containing a 'latex' tabular environment.
Printing the return value at the console will generate a preview of the
table.  Use \code{print.default} to display the name of the generated
\code{.tex} file.

For \code{MSWord.AEdotplot.microplot}, a character string containing the
name of the generated \code{.docx} or \code{.html} file.
}
\references{
Ohad Amit, Richard M. Heiberger, and Peter W. Lane. (2008)
``Graphical Approaches to the Analysis of Safety Data from Clinical Trials''.
\emph{Pharmaceutical Statistics},
\bold{7}, 1, 20--35.
% Published Online: 26 Feb 2007\cr
\url{http://www3.interscience.wiley.com/journal/114129388/abstract}
% \url{http://www3.interscience.wiley.com/cgi-bin/abstract/114129388/ABSTRACT}.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[HH:ae.dotplot7a]{AEdotplot}}
}
\examples{
\dontrun{
  demo("AEdotplot-latex",  package="microplot", ask=FALSE)
  demo("AEdotplot-MSWord", package="microplot", ask=FALSE) ## both "word" and "html"
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
