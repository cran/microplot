\name{show.latexConsole}
\alias{show.latexConsole}
\alias{show.dvilC}
\alias{dvi.latexConsole}
\alias{show.OSfilename}
\alias{print.OSfilename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Revisions of Hmisc latex and dvi functions that display the generated
latex file on screen and divert the console log to a file.  New
print methods that display Operating System files (ps, docx, html) on screen.
}
\description{
Revisions of Hmisc latex and dvi functions that display the generated
latex file on screen and divert the console log to a file.  New
functions that display Operating System files
(\code{"psFilename"}, \code{"msWordFilename"}, or \code{"htmlFilename"}) on screen.
}
\usage{

\method{dvi}{latexConsole}(object, prlog=FALSE, nomargins=TRUE, width=5.5, height=7, ...,
                 ConsoleToFile=TRUE)

\method{show}{latexConsole}(object)

\method{show}{dvilC}(object, width = 5.5, height = 7,
           ConsoleToFile=TRUE)

\method{print}{OSfilename}(x, wait=FALSE, ...)

\method{show}{OSfilename}(x, wait=FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    For \code{show.latexConsole} and \code{dvi.latexConsole}, a \code{c("latexConsole","latex")}
    object created by \code{latex.trellis}.
    For \code{show.dvilC}, a \code{c("dvilC","dvi")} object created by \code{dvi.latexConsole}.
  }x
  \item{ConsoleToFile}{Logical.  \code{TRUE} diverts 'latex' and 'dvips'
           console output to a file (and prints the file name).
           \code{FALSE} displays the console output on the 'R' console.}
  \item{prlog, nomargins, width, height}{See \code{\link[Hmisc]{latex}}.}

\item{x}{The generic functions for \code{print} and \code{show} require
  \code{x} as the argument name.}

\item{wait}{Logical, defaults to \code{FALSE}. Argument to
          \code{\link{system}} command.  \code{FALSE} indicates the print
          should run asynchronously, meaning the R interpreter is
          immediately ready for the next command. With \code{TRUE} the R
          interpreter would wait for the command to finish.}

\item{\dots}{ignored}
}
\details{
Extensions to 'Hmisc' functions \code{dvi.latex}, \code{show.latex}, \code{show.dvi}.
}
\value{
  For \code{dvi.latexConsole}, a \code{c('dvilC', 'dvi')} object.

  For \code{show.latexConsole} and \code{show.dvilC}, when \code{viewer="dvips"} a
    \code{c("psFilename", "OSfilename")} object,
    otherwise \code{NULL}.

    For \code{print.OSfilename}, the input argument is returned invisibly.
    For \code{show.OSfilename}, \code{NULL}.

  }

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[Hmisc]{latex}}, \code{\link{microplot-package}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{LaTeX}
\keyword{postscript}
\keyword{HTML}
\keyword{documentation}
\keyword{utilities}
