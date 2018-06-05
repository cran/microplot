\name{latex.trellis}
\alias{latex.trellis}
\alias{latex.ggplot}
\alias{latex.graphicsList}
\alias{latex.microplotMatrix}
\alias{latex.includegraphicsMatrix}
\alias{latex.graphicsClass}
%% \alias{latex}
%% \alias{dvi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display a table in 'latex' containing panels from \R graphs in
its cells.
}
\description{
Display a table in 'latex' containing panels from \R graphs in
its cells.
Hmisc::latex methods for "trellis", "ggplot", "graphicsList",
"microplotMatrix", and "includegraphicsMatrix" objects.
}
\usage{
\method{latex}{graphicsClass}(  ## called by trellis, ggplot, graphicsList methods
      object,
      figPrefix=first.word(deparse(substitute(object))),
      title=figPrefix, ## subject to lazy evaluation
      ##
      ## microplot arguments
      device={
        latexcmd <- options()$latexcmd
        if (is.null(latexcmd))
          latexcmd <- "latex"
        switch(latexcmd,
               pdflatex="pdf",
               latex=,
               "postscript")
      },
      ... ## can include arguments to
      ## latex.graphicsClass,
      ## microplot,
      ## as.includegraphics,
      ## latex.includegraphicsMatrix,
      ## latex.default
      )

\method{latex}{trellis}( ## calls latex.graphicsClass
      object=stop("trellis object is required", call. = FALSE),
      figPrefix=first.word(deparse(substitute(object))),
      title=figPrefix, ## subject to lazy evaluation
      ... ## can include arguments to
      ## latex.graphicsClass,
      ## microplot,
      ## as.includegraphics,
      ## latex.includegraphicsMatrix,
      ## latex.default
      )

\method{latex}{ggplot}( ## calls latex.graphicsClass
      object=stop("ggplot object is required", call. = FALSE),
      figPrefix=first.word(deparse(substitute(object))),
      title=figPrefix, ## subject to lazy evaluation
      ... ## can include arguments to
      ## latex.graphicsClass,
      ## microplot,
      ## as.includegraphics,
      ## latex.includegraphicsMatrix,
      ## latex.default
      )

\method{latex}{graphicsList}( ## calls latex.graphicsClass
      object=stop("graphicsList object is required", call. = FALSE),
      figPrefix=first.word(deparse(substitute(object))),
      title=figPrefix, ## subject to lazy evaluation
      ... ## can include arguments to
      ## latex.graphicsClass,
      ## microplot,
      ## as.includegraphics,
      ## latex.includegraphicsMatrix,
      ## latex.default
      )

\method{latex}{includegraphicsMatrix}(
      object,
      dataobject, data.first=TRUE,
      title=first.word(deparse(substitute(object))),
      microplotMatrix=NULL,
      arraystretch=1,     ## The normal interrow space is multiplied by arraystretch,
      ##                     so changing it from its default value of 1 to 1.5 makes
      ##                     the rows 1.5 times farther apart.
      ##                     Uses the latex.default argument 'insert.top'.
      bottom.hline.raise=NULL, ## character with latex unit, for example "-10ex"
      ##        arraystretch interferes with bottom.hline.raise
      ##        Pick arraystretch first.
      bottom=if (!is.null(attr(object, "key.name")))
               attr(object, "key.name"),
      col.just.object=rep("c", ncol(object)),
      col.just.dataobject=rep("r", ncol(dataobject)),
      n.cgroup=NULL, ## generated below if cgroup is specified in ... and n.cgroup is not
      ...) ## arguments to latex.default

\method{latex}{microplotMatrix}(object,
      title=first.word(deparse(substitute(object))),
      ...) ## all ... arguments are forwarded to both
           ## as.includegraphics and latex.includegraphicsMatrix

%% latex(object, ...) ## imported from Hmisc

%% dvi(object, ...)   ## imported from Hmisc

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    For \code{latex.trellis}, a \code{"trellis"} object,
    usually a multi-panel object.

    For \code{latex.ggplot}, a \code{"ggplot"} object,
    usually a multi-panel object.

    For \code{latex.graphicsList}, a \code{"graphicsList"} object,
    usually a list of single panel graphics objects.  All items in the list
    must be \code{trellis} objects or all must be \code{ggplot} objects.

    For \code{latex.microplotMatrix}, a
    \code{"microplotMatrix"} object, the result of calling
    \code{microplot} on a \code{"trellis"} or
    \code{"ggplot"} object; a matrix of LaTeX filenames,
    possibly with \code{axis.names} or \code{lab.names} or \code{key.name}
    attributes.

    For \code{latex.includegraphicsMatrix}, a
    \code{"includegraphicsMatrix"} object, the result of
    calling \code{as.includegraphics} on a
    \code{"microplotMatrix"} object; a matrix of LaTeX
    \code{\\includegraphics} expressions, possibly with
    \code{axis.names} or \code{lab.names} or \code{key.name} attributes.

	 }
\item{device}{Function used to construct the graphics files.  See \code{\link{microplot}}.}
\item{dataobject}{Numeric or character matrix (or data.frame).}
\item{data.first}{
  Logical.  If \code{TRUE}, then output file will have \code{dataobject} columns
first, then graphics \code{object} columns.
If \code{FALSE}, then output file will have graphics \code{object} columns
first, then \code{dataobject} columns.
}
\item{figPrefix}{See \code{\link{microplot.trellis}}.
    The 'latex' macro \code{\\includegraphics} requires that there be no
    \code{"."} in the filename basename.  We replace all \code{"."} in
    the \code{figPrefix} by \code{"-"}.
}
\item{title}{Arguments to \code{Hmisc::latex}.}
\item{microplotMatrix}{The \code{microplotMatrix} will be made an
  attribute of the resulting \code{latex} object.}
\item{arraystretch}{The normal interrow space is multiplied by
  \code{arraystretch}, so changing it from its default value of 1 to 1.5
  makes the rows 1.5 times farther apart.  Uses the \code{latex.default}
  argument \code{insert.top}.
}
\item{bottom.hline.raise}{Character string with latex unit, for example
  \code{"-10ex"}.  \code{arraystretch} interacts with \code{bottom.hline.raise}. Pick
  \code{arraystretch} first.
}
\item{bottom}{default argument to \code{latex.default}'s
  \code{insert.bottom} argument.}
\item{col.just.object, col.just.dataobject}{
  Column justification.  See \code{\link{formatDF}}.
  %% copy of \code{\link[Hmisc]{format.df}} which is now deprecated.
  The default centers graph panel columns
  and right justifies \code{dataobject} columns because it assumes the
  \code{dataobject} contains formatted (hence aligned) numerical data.
}
\item{n.cgroup}{See \code{\link[Hmisc]{latex}}. When \code{cgroup} is
  specified it always appears in \code{...}. \code{ncgroup} is an
  optional input here because we have enough information to generate it.}
  \item{\dots}{
    Arguments to \code{microplot.trellis}, \code{microplot.ggplot},
                    \code{as.includegraphics}, \code{latex.includegraphicsMatrix},
                    \code{latex.default}.
}
}
\value{
The value of these \code{latex} methods is a \code{"latex"} object
containing two components.
\item{file}{Pathname of the generated \code{.tex} file.}
\item{style}{\code{"graphicx"},
indicating that the \code{latex} \code{\\usepackage{graphicx}} is required}
See \code{demo/HowToUseMicroplot.r} for a tutorial.  See the demos in
\code{demo/latex.r} and \code{demo/latex-ggplot.r} for an elaborate
example.

When one of the \code{...} arguments is \code{file=""}, the
generated LaTeX code is printed to standard output.  See the discussion
of the \code{file} argument in \code{\link[Hmisc]{latex}} to learn how
to use this feature with Sweave.

Function \code{latex.includegraphicsMatrix} takes the output of
\code{as.includegraphics} as its input and returns a
\code{"latex"} object.  If there is a \code{key.name} attribute, then it is forwarded to
\code{latex.default} as the \code{insert.bottom} argument.
The result has an attribute \code{"includegraphicsMatrix"}
containing its argument \code{object} and an attribute
\code{"microplotMatrix"} containing the \code{"microplotMatrix"} object
from which the \code{"includegraphicsMatrix"} was constructed.

Function \code{latex.microplotMatrix} takes the output of
\code{microplot} as its input and forwards it to
\code{latex.includegraphicsMatrix}.
All \code{...} arguments are forwarded to
\code{latex.includegraphicsMatrix}.  The return value is a
\code{"latex"} object.

Functions \code{latex.trellis} and \code{latex.ggplot} and
\code{latex.graphicsList}
take their input and forward it through \code{latex.graphicsClass} to
\code{microplot}
and then to \code{latex.microplotMatrix}.

The print method for \code{"latex"} objects, described in
\code{\link[Hmisc:latex]{dvi}},
is to display the latexed file on the screen at 5.5in wide by 7in tall.
The dimensions can be changed by an explicit call to the the \code{dvi} method with other
dimensions, for example\cr
\code{dvi(latex(MyTrellisObject), height.panel=11, width.panel=8.5)}\cr
See an example in \code{demo("latex", package="microplot", ask=TRUE)}

The format of the screen display depends on three options described in
\code{\link{latexSetOptions}} and \code{\link[Hmisc]{latex}}.

For \code{pdflatex} normally use:\cr
\code{latexSetOptions("pdflatex")}

For \code{latex} normally use:\cr
latexSetOptions("latex")

}

\details{
  The explicit result is a \code{"latex"} object containing the name of a
  generated \code{.tex} file in the current directory.  The file
  contains a latex \code{\\tabular} environment holding a
  \code{\\table}.  The cells of the \code{\\table} contain each of the
  filenames wrapped in an \code{\\includegraphics} expression.  To get
  the name of the created file, you must save the returned value from
  the \code{"latex"} function and display it with \code{print.default}.

  The \code{print} method for \code{"latex"} objects wraps the generated
  file in a minimal complete \code{latex} file, runs that file through
  the system \code{pdflatex} or \code{latex} (depending on the value of
  \code{options("latexcmd")}) to create a \code{pdf} file (or \code{dvi} file,
  depending on the value of \code{options("dviExtension")}), and
  displays it on the screen.  To get the name of the displayed file, you
  must explicitly use the \code{dvi} function on the \code{"latex"}
  object and save the otherwise invisible return value.  If it is a
  \code{pdf} file it can be included with an \code{\\includegraphics}
  expression into another \code{.tex} file for use with \code{pdflatex}.
  If it is a \code{dvi} file it can be converted with \code{dvips} to a
  \code{.ps} file and included with an \code{\\includegraphics}
  expression into another \code{.tex} file for use with \code{latex}.

}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[Hmisc]{latex}}, \code{\link{microplot}}, \code{\link{as.includegraphics}}
}
\examples{
## See the examples in the help files, the demo files, and in the
## examples subdirectory.

## The example here shows how to locate the generated .tex file and the displayed .pdf file.
## The .tex file can be brought into a larger .tex file with an \include statement.
## The .pdf file can be brought into a larger .tex file with an \includegraphics statement.

\dontrun{
## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()

mpgGraph <- lattice::xyplot(mpg ~ wt, group=factor(cyl), data=mtcars,
                            xlim=c(.8, 6.2), ylim=c(9,37),
                            pch=as.character(levels(factor(mtcars$cyl))), cex=2)
mpgGraph ## on your interactive device
mpgLatex <-
  latex(mpgGraph,
        height.panel=2, width.panel=3, ## inch. pick numbers that look right to you.
        height.x.axis=.37, width.y.axis=.44,  ## inch. these require trial and error.
        height.xlab=.18, width.ylab=.27,      ## inch. these require trial and error.
        rowname=NULL,    ## suppress rownames, see ?latex
        colheads=FALSE)  ## suppress colnames, see ?latex
print.default(mpgLatex)  ## file is in your working directory
mpgPdf <- dvi(mpgLatex)
print.default(mpgPdf) ## File is in a temporary directory.
                      ## If Macintosh shows "//", replace by "/" before using.
mpgPdf

}
## Sweave users can bring the generated files directly into their
## document.  See the discussion of the \code{file} argument in
## \code{\link[Hmisc]{latex}} to learn how to use this feature with
## Sweave.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ documentation }% use one of  RShowDoc("KEYWORDS")
\keyword{ utilities }% __ONLY ONE__ keyword per line
\keyword{ LaTeX }
