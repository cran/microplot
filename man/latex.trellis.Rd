\name{latex.trellis}
\alias{latex.trellis}
\alias{latex.includegraphicsMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hmisc::latex method for "trellis" objects.
}
\description{
Hmisc::latex method for "trellis" objects.
}
\usage{
latex.trellis(## microplot arguments
              object=stop("trellis object is required", call. = FALSE),
                         ## object must have class "trellis"
              obji=NULL, ## Vector or matrix of trellis objects, with dimnames,
                         ## normally each containing one panel.
                         ## When obji is non-null, the axes and key will be
                         ## taken from object.  We need object to be a trellis
                         ## object so the latex generic will dispatch to
                         ## latex.trellis.
              figPrefix=Hmisc::first.word(deparse(substitute(object))),
              device=c("pdf","postscript","png", ## other devices require
                                                 ## a user-written function
                       if (version$os == "mingw32") "win.metafile"),
              height=1, width=1, ## microplot, numeric, in inches
              collapse=layoutCollapse, ## see below for example.
                                       ## zero out unwanted layout.heights
                                       ## and layout.widths.
              height.x.axis=height,
              par.settings.x.axis=
                list(layout.heights=list(panel=0, axis.bottom=1,
                                         axis.xlab.padding=1, xlab=1),
                     axis.line=list(col="black")),
              width.y.axis=width,
              par.settings.y.axis=
                list(layout.widths=list(ylab=1, ylab.axis.padding=1,
                                        axis.left=1, panel=0),
                     axis.line=list(col="black")),
              key=FALSE,    ## FALSE or a list of arguments defining a key
              height.key=height, width.key=width,
              ##
              ## as.includegraphics arguments
              height.as=NULL, ## character with TeX units, for example "1in"
              width.as=NULL,  ## keeps aspect ratio
              scale=NULL,     ## ignored if either height.as or width.as
                              ## is specified.
              raise=NULL,     ## argument to as.includegraphics
              hspace.left=NULL,  ## argument to as.includegraphics
              hspace.right=NULL, ## argument to as.includegraphics
              wd=getwd(),     ## working directory for graphics files
              viewport=NULL,  ## if specified, then left bottom right top (character)
              ## used for pdf png jpeg
              ## See MediaBox in pdf file.
              ## Ask operating system for png or jpg file.
              bb=NULL,   ## if specified, then left bottom right top (character)
                         ## used for bmp tiff ps, ask operating system for values
              trim=NULL, ## for example, "0 0 0 0" left bottom right top (character)
              x.axis.includegraphics=TRUE, ## logical or a list of arguments
              y.axis.includegraphics=TRUE, ## logical or a list of arguments
              key.includegraphics=is.list(key), ## logical or a list of arguments
              ##
              ## latex.includegraphicsMatrix arguments,
              ## including arguments to latex.default
              ...)

latex.includegraphicsMatrix(
           ii, ## ii is not currently an includegraphicsMatrix object.
           rowlabel="rowname", ## latex, column heading of row dimnames
           title="figPrefix",  ## latex.default
           rowseparator=FALSE,
           return.value=c("latex","R"),
           bottom=if (!is.null(attr(ii, "key.name")))
                    attr(ii, "key.name"),
           ...) ## arguments to latex.default
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
\code{"trellis"} object, usually a multi-panel object.
}
  \item{obji}{\code{NULL}, or a
vector or matrix of \code{"trellis"} objects, with dimnames, normally
each containing one panel.  When \code{obji} is non-null, the axes and key will
be taken from \code{object}.  We need \code{object} to be a \code{"trellis"} object so the
\code{latex} generic will dispatch to \code{latex.trellis}.
}
\item{figPrefix, device, height, width, collapse}{See \code{\link{microplot.trellis}}.}
\item{height.x.axis, par.settings.x.axis, width.y.axis,par.settings.y.axis}{See \code{\link{microplot.trellis}}.}
\item{key, height.key, width.key}{See \code{\link{microplot.trellis}}.}
  \item{height.as, width.as}{
Forwarded to \code{\link{as.includegraphics}} as \code{height} and \code{weight}.
  }
  \item{scale, raise, hspace.left, hspace.right, wd, viewport, bb, trim}{See \code{\link{as.includegraphics}}}
  \item{x.axis.includegraphics, y.axis.includegraphics, key.includegraphics}{
Forwarded to \code{\link{as.includegraphics}} as \code{x.axis},
           \code{y.axis}, and \code{key}.
}
\item{ii}{The input to \code{latex.includegraphicsMatrix} is the result
          of \code{as.includegraphics} call; a matrix of
           LaTeX \code{\\includegraphics} statements, possibly with
           \code{axis.names} or \code{keyname} attributes.}
\item{title, rowlabel}{Arguments to \code{Hmisc::latex}.}
\item{rowseparator}{
logical.  If \code{TRUE}, then place a blank line between rows.
}
\item{return.value}{The default \code{"latex"} sends \code{ii} to
           \code{Hmisc::latex} for processing by the operationg system
           latex.  The alternate \code{"R"} returns the \code{ii} matrix
           for further processing, for example to \code{rbind} a
           numerical table before sending the result to
           \code{Hmisc::latex}.}
\item{bottom}{default argument to \code{latex.default}'s
           \code{insert.bottom} argument.}
  \item{\dots}{
Further arguments to \code{latex.default}.
}
}
\value{
The default value of \code{latex.trellis}, when
\code{return.value="latex"}, is a \code{"latex"} object containing two
components. The \code{file} component contains the pathname of the
generated \code{.tex} file.  The \code{style} component contains the
value \code{graphicx} indicating that the latex
\code{\\usepackage{graphicx}} is required.  See the demo in
\code{demo/latex.r} for an elaborate example, almost a tutorial.

\item{file}{Pathname of the generated \code{.tex} file.}
\item{style}{\code{"graphicx"}}

When \code{return.value="latex"} and argument \code{file=""}, the
generated LaTeX code is printed to standard output.  See the discussion
of the \code{file} argument in \code{\link[Hmisc]{latex}} to learn how
to use this feature with Sweave.

The alternate value of \code{latex.trellis} when \code{return.value="R"}
is a matrix of \code{\\includegraphics{}} statements that may be
combined with other matrices and then sent to \code{latex.default}.  See
the last example in \code{demo/bwplot-lattice.r}.

Function \code{latex.includegraphicsMatrix} takes the output of
\code{as.includegraphics} as its input \code{ii} and returns a
\code{"latex"} object.  If there are \code{axis.names} attributes, they
are placed in front of each row of panels and beneath each column of
panels.  If there is a \code{keyname} attribute, then it is forwarded to
\code{latex.default} as the \code{insert.bottom} argument.

}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[Hmisc]{latex}}
}
\examples{
## See the examples in the help files, the demo files, and in the
## examples subdirectory.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ documentation }% use one of  RShowDoc("KEYWORDS")
\keyword{ utilities }% __ONLY ONE__ keyword per line
\keyword{ LaTeX }
