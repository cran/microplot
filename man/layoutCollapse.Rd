\name{layoutCollapse}
\alias{layoutHeightsCollapse}
\alias{layoutWidthsCollapse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the lattice \code{par.settings} to remove all marginal space.
}
\description{
  Set the lattice \code{par.settings} to remove all marginal space.
  By default everything in \code{layout.heights} or \code{layout.widths} is set to 0
  except for \code{panel}.  The user can specify values for all the standard
  items in either of those items.
}
\usage{
layoutHeightsCollapse(...)
layoutWidthsCollapse(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Any item name in \code{layout.heights} for \code{layoutHeightsCollapse} or\cr
in \code{layout.widths} for \code{layoutWidthsCollapse}.
}
}
\details{
When very small plots are placed inside a LaTeX tabular environment, it is often
helpful
to suppress margins, axes, labels, titles.
}
\value{
A list which may be used as input to the \code{par.settings} argument in a
lattice call.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
lattice::trellis.par.get("layout.heights")
lattice::trellis.par.get("layout.widths")
layoutHeightsCollapse()
layoutWidthsCollapse()
layoutWidthsCollapse(axis.left=1)

## Please see the package documentation for a simple example in context.

## Please see the demos for more interesting examples.
## demo(package="microplot")
}
\keyword{ lattice }
