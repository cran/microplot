\name{latex.AEdotplot}
\alias{latex.AEdotplot}
\alias{MSWord.AEdotplot}
\title{
Display the AE (Adverse Events) dotplot of incidence and relative risk
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
  The \code{\link{MSWord}} method takes the
  incidence panel and the relative risk panel from the
  \code{AEdotplot} and places them in an 'MS Word' table
  along with the numerical table of counts, percents, and relative risks.
}
\usage{
\method{latex}{AEdotplot}(object, figPrefix = Hmisc::first.word(substitute(object)),
      device="pdf", ...)

\method{MSWord}{AEdotplot}(object, filetype="docx", figPrefix = Hmisc::first.word(substitute(object)),
       device="png", ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An \code{"AEdotplot"} object as constructed by the \code{\link[HH:ae.dotplot7a]{AEdotplot}}.
}
  \item{figPrefix}{
Beginning characters for names of the sequence of generated graphics files.
}
\item{device}{
Forwarded to \code{microplot}.
}
\item{filetype}{
Forwarded to \code{MSWord}.
}
\item{\dots}{
Additional arguments forwarded to the \code{\link[Hmisc]{latex}}
function (which will use them) or to the\cr
\code{\link{MSWord.AEdotplot.microplot}} function (which will ignore them).
}
}
\value{
For \code{latex.AEdotplot}, the \code{"latex"} object giving the pathname of the \code{.tex} file containing
the 'LaTeX' tabular environment constructed by the \code{\link[Hmisc]{latex}} function.
For \code{MSWord.AEdotplot}, the \code{"character"} object giving the
pathname of the \code{.docx} file containing
the generated table constructed by by \code{MSWord.AEdotplot.microplot}
function which in turn uses functions in the \pkg{\link{ReporteRs}} package.
}
\details{
The \code{\link{microplot.AEdotplot}} function does most of the work, taking apart the
\code{"AEdotplot"} object and constructing the set of graphics files and
returning the data.frame containing the tabled data and the set of
graphics files.
The data.frame is sent to either
 \code{\link[Hmisc]{latex}} or to \code{\link{MSWord.AEdotplot.microplot}}.
For more detail see \code{\link{microplot.AEdotplot}}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
See \code{\link[HH:ae.dotplot7a]{AEdotplot}} and \code{\link[Hmisc]{latex}} for
details on the operation of the \code{latex} method.\cr
See \code{\link{microplot.AEdotplot}} for details on how the
\code{\link{microplot}}
function works with the \code{"AEdotplot"} object.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

\dontrun{
## The function is currently defined as
function (object, figPrefix = Hmisc::first.word(substitute(object)),
    ...)
{
    Hmisc::latex(microplot(object, figPrefix = figPrefix, target = "latex",
        ...))
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ html }