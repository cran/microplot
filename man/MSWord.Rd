\name{MSWord}
\alias{MSWord}
\title{
Generate a .docx or .html file from an R object.
}
\description{
Generate a \code{.docx} or \code{.html} file from an R object.
}
\usage{
MSWord(object, filetype=c("docx", "html"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
     For the
\code{\link{MSWord.AEdotplot}} method, the argument is an \code{"AEdotplot"} object
constructed with the \code{\link[HH:ae.dotplot7a]{AEdotplot}} function.
For the more general methods the object will be a data.frame
containing
formatted numbers (class \code{"character"}, not
class \code{"numeric"}) and the pathnames of \code{.png}
(or \code{.wmf} on MS Windows) files.
At this writing the only more general method is the\cr
\code{\link{MSWord_TablePlusGraphicColumn}}
function which requires several columns containing formatted numerical
data (hence \code{"character"}, not \code{"numeric"}) and the
last column containing the pathnames to the graphics files.
\cr
\code{MSWord_TablePlusGraphicColumn}
is written as a function, not a method, so its full name must be used.
}
  \item{filetype}{
Type of file to generate: \code{.docx} for 'MS Word', or \code{.html}
  for 'HTML'.
}
  \item{\dots}{
Additional arguments used by methods.
}
}
\details{
  See \code{\link{MSWord.AEdotplot}} for \code{"AEdotlot"} objects.

  See \code{\link{MSWord_TablePlusGraphicColumn}} for numerical data
  with a single graphics column.

  For more general objects, there is not yet a cover routine.  It will
  be necessary for the user to write the code directly using the
  functions provided in the \pkg{ReporteRs} package.  Should anyone want
  to write a more general \code{MSWord} method, I will be happy to
  include it (with full credit to the author) in a future release of the
  \pkg{microplot} package.
}
\value{
Invisibly, the name of the constructed \code{.docx} or \code{.html} file.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## See the examples in the demo files.
\dontrun{
  demo("bwplot-lattice-MSWord",   package="microplot")
  demo("tablesPlusGraphicColumn", package="microplot")
  demo("boxplot-ggplot-MSWord",   package="microplot")
  demo("AEdotplot-MSWord",        package="microplot")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
