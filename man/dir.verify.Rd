\name{dir.verify}
\alias{dir.verify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Verifies existence of, or creates, a directory.
}
\description{
Verifies existence of, or creates, a directory.
}
\usage{
dir.verify(path)
}
\arguments{
  \item{path}{
    A character vector containing a single path name.
    See \code{\link[base:files2]{dir.exists}} for more detail.
}
}
\value{
  Logical.  \code{TRUE} if the directory already exists or is newly created.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link[base:files2]{dir.exists}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities}
