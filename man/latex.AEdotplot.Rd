\name{latex.AEdotplot}
\alias{latex.AEdotplot}
\alias{msWord.AEdotplot}
\alias{microplot.AEdotplot}
\title{
Display the AE (Adverse Events) dotplot of incidence and relative risk
from the HH package
in a 'LaTeX' tabular environment or in an 'MS Word' or an 'HTML' table.
}
\description{
  The \code{\link[HH:ae.dotplot7a]{AEdotplot}} function constructs a
  display of the most frequently occurring AEs (Adverse Events) in the
  active arm of a clinical study.
  The \code{\link[Hmisc]{latex}} method takes the
  incidence panel and the relative risk panel from the
  \code{AEdotplot} and places them in a 'LaTeX' tabular environment
  along with the numerical table of counts, percents, and relative risks.
  The \code{\link{msWord}} method takes the
  incidence panel and the relative risk panel from the
  \code{AEdotplot} and places them in an 'MS Word' table
  along with the numerical table of counts, percents, and relative risks.
}
\usage{
\method{latex}{AEdotplot}(object, figPrefix = first.word(deparse(substitute(object))),
      rowlabel="Most Frequent On-Therapy Adverse Events",
      device="pdf", ...)

\method{msWord}{AEdotplot}(object, figPrefix = first.word(deparse(substitute(object))),
       device="png",
       height.panel=.25, height.x.axis=.45,
       width.left=2, width.right=1.5,
       height.key=height.panel,
       width.dataobject=.7,
       rowlabel="Adverse Event", width.rowname=2,
       ...)

\method{microplot}{AEdotplot}(object, figPrefix, width.left=2, width.right=1.5,
          height.panel=.2, height.x.axis=.45, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An \code{"AEdotplot"} object as constructed by the \code{\link[HH:ae.dotplot7a]{AEdotplot}}.
}
  \item{figPrefix}{
Beginning characters for names of the sequence of generated graphics
files.
    The 'latex' macro \code{\\includegraphics} requires that there be no
    \code{"."} in the filename basename.  We replace all \code{"."} in
    the \code{figPrefix} by \code{"-"}.
}
\item{device}{
Forwarded to \code{microplot}.
}
\item{width.left, width.right}{\code{width.left} is
  \code{width.panel} for the Percent column and \code{width.right} is the
  \code{width.panel} for the Relative Risk column of the
  \code{AEdotplot}.  See  \code{\link[HH:ae.dotplot7a]{AEdotplot}} and \link{microplot}.}
\item{height.panel, height.x.axis, height.key}{See
  \code{\link{microplot}}.}
\item{width.dataobject, width.rowname}{See \code{\link{msWord}}.}
\item{rowlabel}{See \code{\link{latex.trellis}} or \code{\link{msWord}}.}
\item{\dots}{
Additional arguments to \code{microplot.AEdotplot} are forwarded to the
\code{microplot.trellis} method.  Additional arguments to
\code{latex.AEdotplot} are forwarded to \code{microplot.AEdotplot} and to
\code{\link{latex}}.
Additional arguments to \code{\link{msWord.AEdotplot}} are forwarded to \code{microplot.AEdotplot} and to
\code{\link{msWord}}.
}
}
\value{
For \code{latex.AEdotplot}, the \code{"latex"} object giving the
pathname of the \code{.tex} file containing
the 'LaTeX' tabular environment constructed by the \code{\link[Hmisc]{latex}} function.
For \code{msWord.AEdotplot}, the  \code{"msWordFilename"} object giving the
pathname of the \code{.docx} file containing
the generated table constructed by the \code{\link{msWord.microplotMatrix}}
functionm which in turn uses functions in the \pkg{\link{ReporteRs}}
package.

The \code{microplot.AEdotplot} method returns a list containing the
\code{"microplotMatrix"} and the data.frame.
	}
\details{
The \code{\link{microplot.AEdotplot}} function does most of the work, taking apart the
\code{"AEdotplot"} object and constructing  from it the set of graphics files
identified in a \code{"microplotMatrix"} object and collecting the
numerical data into a data.frame.  The \code{"microplotMatrix"} and the
          data.frame are returned.

The \code{\link[Hmisc]{latex}} and \code{\link{msWord}} methods call the
\code{microplot} method
and then the \code{latex} or \code{msWord} generic.
The \code{msWord} method has more arguments than the \code{latex}
method because it doesn't pick up
the height and width dimensions from the graphics (\code{.png}) files.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
See \code{\link[HH:ae.dotplot7a]{AEdotplot}} and \code{\link[Hmisc]{latex}} for
details on the operation of the \code{latex} method.\cr
}
\examples{
## See
## demo("AEdotplot", package="microplot", ask=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ html }