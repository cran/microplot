\name{latexCheckOptions}
\alias{latexCheckOptions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check whether the options for latex functions have been specified.
}
\description{
Check whether the options for latex functions have been specified.
If any of\cr
\code{options()[c("latexcmd","dviExtension","xdvicmd")]}
are \code{NULL}, an error message is displayed.
}
\usage{
latexCheckOptions(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Any arguments are ignored.
}
}
\value{
If any \code{NULL} options are detected, the invisible text of the
error message.  If all three options have non-\code{NULL} values, NULL.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link[Hmisc]{latex}}, \code{\link{microplot}}
}
\keyword{ utilities}
