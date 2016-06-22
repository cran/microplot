\name{as.orgfile}
\alias{as.orgfile}
\title{
Place a filename or filepath in the format used by org-mode
}
\description{
Place a filename or filepath in the format used by org-mode, by
surrounding it with \code{"[["} and \code{"]]"}.
}
\usage{
as.orgfile(object, wd = getwd(), ...)
}

\arguments{
  \item{object}{
Vector of character strings containing filenames.
}
  \item{wd}{
The directory in which the files reside.  The default is the current
working directory that R is using.
}
  \item{\dots}{
Ignored.
}
}
\value{
A character vector containing the input strings surrounded by \code{"[["}
  and \code{"]]"}.
}
\author{ Nooreen Dabbish <nerd@temple.edu> and Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link{microplot}}
  }

\examples{
as.orgfile("abcd.png")
as.orgfile("abcd.png", wd=".")
as.orgfile(c("abcd.png", "efgh.png"))

## For an example in context, please see the package example:
##    system.file(package="microplot", "examples/irisOrgHtml.org")
## Copy file irisOrgHtml.org to a directory in which you have write privileges,
## open it in emacs, and enter
## C-c C-e b           on Macintosh
## C-c C-e ho          on Windows
## C-c C-e <something> on linux

}
\keyword{ documentation }
\keyword{ utilities}
