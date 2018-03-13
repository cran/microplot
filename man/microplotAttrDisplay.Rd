\name{microplotAttrDisplay}
\alias{microplotAttrDisplay}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Specify how to display the microplots for x.axis, y.axis, xlab, ylab, and key.
}
\description{
Specify how to display the microplots for x.axis, y.axis, xlab, ylab, and key.
}
\usage{
microplotAttrDisplay(ii,
                     y.axis=unname(attr(ii, "axis.names")["y"]),
                     x.axis=unname(attr(ii, "axis.names")["x"]),
                     ylab=unname(attr(ii, "lab.names")["y"]),
                     xlab=unname(attr(ii, "lab.names")["x"]),
                     key=attr(ii, "key.name"),
                     columnKey=NULL,
                     label.x.axis="", ## empty, nchar=0
                     label.y.axis=" " ## one space, nchar=1
                     )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ii}{
A \code{"microplotMatrix"} or \code{"includegraphicsMatrix"} of
filenames of graphics files each containing
one panel of an array of plots.  There may be up to three attributes
containing additional filenames.
}
  \item{x.axis, y.axis}{
Vector of filenames containing graphic files of axes.
}
\item{label.x.axis, label.y.axis}{Labels that will used in the column
  name of the \code{y.axis} and the \code{y.axis} column for the
  \code{x.axis} row in the 'latex' display of the graphic.}
  \item{xlab, ylab}{
Vector of filenames containing graphic files of axis labels.
}
  \item{key}{
Filename containing a graphics file containing a key (legend).
}
  \item{columnKey}{
If the key is non-null, then place its filename as a new last value in
the specified columns.  The column numbering is with respect to the
input ii before the y.axis or ylab are evaluated.
}
}
\value{
  Revised version of the input \code{ii}, possibly augmented with additional rows
for the x.axis, xlab, and key, and additional columns for the ylab and
y.axis.  The xlab is ignored unless the x.axis is also specified.
The ylab is ignored unless the y.axis is also specified.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{microplot}}
}
\examples{

\dontrun{
latexSetOptions()

filenames <-
  structure(c("tt010.pdf", "tt007.pdf", "tt004.pdf", "tt001.pdf",
              "tt011.pdf", "tt008.pdf", "tt005.pdf", "tt002.pdf",
              "tt012.pdf", "tt009.pdf", "tt006.pdf", "tt003.pdf"),
            .Dim = c(4L, 3L),
            .Dimnames = structure(list(
              rr = c("d", "c", "b", "a"),
              cc = c("E", "F", "G")),
              .Names = c("rr", "cc")),
            axis.names = structure(c("tt013.pdf", "tt014.pdf"), .Names = c("x", "y")),
            lab.names = structure(c("tt015.pdf", "tt016.pdf"), .Names = c("x", "y")),
            key.name = "tt017.pdf",
            class = c("microplotMatrix", "matrix"))

filenames

as.includegraphics(filenames, wd=".")

as.includegraphics(filenames, wd=".", as.attr=FALSE) ## default

as.includegraphics(filenames, wd=".", as.attr=TRUE)

as.includegraphics(filenames, wd=".", columnKey=1)

as.includegraphics(filenames, wd=".", columnKey=1:3)

as.includegraphics(filenames, wd=".", xlab=TRUE, ylab=TRUE)

as.includegraphics(filenames, wd=".",
                   label.x.axis="X tick values", label.y.axis="Y tick values")

tt <- data.frame(x=1:4, y=c(2,3,4,1), group=c("A","A","B","B"))
latex(lattice::xyplot(y ~ x | group, data=tt))
latex(lattice::xyplot(y ~ x | group, data=tt),
      label.x.axis="X Range", label.y.axis="Y Range")

demo("AEdotplot", package="microplot", ask=TRUE)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
