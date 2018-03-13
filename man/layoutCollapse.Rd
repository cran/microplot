\name{layoutCollapse}
\alias{layoutHeightsCollapse}
\alias{layoutWidthsCollapse}
\alias{layoutCollapse}
%% \alias{layout.1.1.Collapse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the lattice \code{par.settings} to remove all marginal space.
}
\description{
  Set the lattice \code{par.settings} to remove all marginal space.
  By default \code{layoutHeightsCollapse} and \code{layoutWidthsCollapse} set
  everything in \code{layout.heights} or \code{layout.widths} to 0
  except for \code{panel}.  The user can specify values for all the standard
  items in either of those items.\cr
  \code{layoutCollapse} by default sets \code{layout=c(1,1)},
  collapses to 0 all heights and widths except for
  \code{panel}, removes all labels and strip labels, and sets all axis
  lines to \code{col="transparent"}.
  %% \code{layout.1.1.Collapse} sets \code{layout=c(1,1)},
  %% collapses to 0 all heights and widths except for
  %% \code{panel}, removes all labels and strip labels, and sets all axis
  %% lines to \code{col="transparent"}.
}
\usage{
%% layout.1.1.Collapse(x)
layoutCollapse(x,
               xlab="",
               ylab="",
               xlab.top=NULL,
               ylab.right=NULL,
               main=NULL,
               sub=NULL,
               strip=FALSE,
               strip.left=FALSE,
               layout.heights=layoutHeightsCollapse(),
               layout.widths=layoutWidthsCollapse(),
               strip.border=list(col="transparent"),
               axis.line=list(col="transparent"),
               layout=c(1,1),
               ...)
layoutHeightsCollapse(...)
layoutWidthsCollapse(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    For \code{layoutCollapse} any argument to
    \code{\link[lattice]{update.trellis}}.

    For \code{layoutHeightsCollapse} any item name in\cr
    \code{\link[lattice]{trellis.par.get}()$layout.heights}.

    For \code{layoutWidthsCollapse} any item name in\cr
    \code{\link[lattice]{trellis.par.get}()$layout.widths}.
}
\item{x}{Any \code{"trellis"} object.}
\item{xlab, ylab, xlab.top, ylab.right, main, sub}{Standard \code{\link[lattice]{xyplot}} arguments.}
\item{strip, strip.left, strip.border, axis.line, layout}{Standard \code{\link[lattice]{xyplot}} arguments.}
\item{layout.heights, layout.widths}{Arguments to \code{\link[lattice]{trellis.par.get}}.
}
}
\details{
When very small plots are placed inside a LaTeX tabular environment, it is often
helpful
to suppress margins, axes, labels, titles.
}
\value{
For \code{layoutCollapse}, a \code{"trellis"} object.

For \code{layoutHeightsCollapse} and \code{layoutWidthsCollapse},
a list which may be used as input to the \code{par.settings} argument in a
lattice call.
%%  For \code{layout.1.1.Collapse}, a \code{"trellis"} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
\dontrun{
  lattice::trellis.par.get("layout.heights")
  lattice::trellis.par.get("layout.widths")
  layoutHeightsCollapse()
  layoutWidthsCollapse()
  layoutWidthsCollapse(axis.left=1)

  A <- lattice::xyplot(Sepal.Length ~ Sepal.Width | Species, data=iris)

  A                 ## one page with three panels

  layoutCollapse(A) ## three pages with one unlabeled panel on each

  layoutCollapse(A, ## one page with panels labeled by ylab
                 layout=c(1,3),
                 ylab=levels(iris$Species),
                 layout.heights=list(axis.bottom=1),
                 layout.widths=list(axis.left=1),
                 axis.line=list(col="green"))
}

## Please see the package documentation for a simple example in context.

## Please see the demos for more interesting examples.
## demo(package="microplot")
}
\keyword{ lattice }
