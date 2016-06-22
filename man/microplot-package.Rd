\name{microplot-package}
\alias{microplot-package}
\alias{microplot}
\alias{sparkline}
\docType{package}
\title{
\packageTitle{microplot}
}
\description{
\packageDescription{microplot}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{microplot}
\packageIndices{microplot}

Microplots are small plots that fit into the cells of a table that
otherwise consists of text and numbers.  A special case of a microplot
is known as a sparkline.

The examples in this package show tables of simple or complex graphs
placed into one or more columns of a table.  The graphs can be produced
by any graphical system in R.  We show lattice, base, and ggplot2
graphics.  The tables can be targeted for display in either LaTeX or
HTML.  We show both.

The best way to learn this package is to read the example and demo files.

This examples and demo files uses operating system \code{latex} command
and the R \code{pdf()} graphics device.  They therefore require that the
three options\cr
      \code{options()[c("latexcmd","dviExtension","xdvicmd")]}\cr
all be set to non-\code{NULL} values.
Please see the "System options" in the "Details" section of\cr
\code{?Hmisc::latex} for discussion of the options available.

I normally use the options\cr
     \code{options(latexcmd="pdflatex")}\cr
     \code{options(dviExtension="pdf")}\cr
on Macintosh, on Windows, on Linux.

The \code{"xdvicmd"} option is dependent on details of the operating system.
The goal is to name the function that opens \code{pdf} files.
In order to open \code{pdf} files, the \code{"xdvicmd"} option must be specified.
On systems I have looked, all these work\cr
\code{options(xdvicmd="open") ## Macintosh, Windows, SMP linux}\cr
\code{options(xdvicmd="xdg-open") ## ubuntu linux}\cr
For anything else, you might need to experiment.

Should you prefer to use the operating system \code{latex} command to
write \code{dvi} files, then leave the first two options unspecified.
You might need to change the \code{"xdvicmd"}.  See \code{?Hmisc::latex}
for guidance.  Also you will need to modify the examples to specify a
different R device function instead of the \code{pdf} function used
here.

The examples in this DESCRIPTION file are inside \code{dontrun} environments
because they depend on options and write files.  You must set the
options for your system before running the example manually.

Most of the examples are shown using the \code{Hmisc::latex} function
\code{\link[Hmisc]{latex}}
(I am coauthor of that function).
The \pkg{microplot} package also works with the \code{xtable::xtable}
function \code{\link[xtable]{xtable}}.
The last example in this help file shows a simple use of \code{xtable}

The demos in the demo directory are not inside a \code{dontrun}
environment.  You must set the options for your system before running
them.  I recommend that you run them manually, not automatically.  You
will need to read them closely to see what they are doing.

To run the demos automatically, use

\code{demo("bwplot",     package="microplot", ask=FALSE)}

\code{demo("NTplot",     package="microplot", ask=FALSE)}

\code{demo("timeseries", package="microplot", ask=FALSE)}

The \code{examples} directory
\code{paste0(system.file(package="microplot"), "/examples")}
includes complete working examples of \pkg{Sweave} (both LaTeX-style and
Noweb-style), \pkg{knitr}, \code{emacs} \pkg{orgmode}, and
\pkg{rmarkdown}
input files and their \code{pdf} output files.  These files must be
copied into a directory in which you have write privilege, and that
directory must be made the current working directory with \code{setwd}.
They will not work from the installed package directory.

}
\author{
\packageAuthor{microplot}

Maintainer: \packageMaintainer{microplot}
}
\keyword{ package }
\keyword{ microplot }
\keyword{ sparkline }
\seealso{
\code{\link[Hmisc]{latex}}
}
\examples{
\dontrun{
latexCheckOptions()

## These are the LaTeX options I use
options(latexcmd="pdflatex") ## Macintosh, Windows, linux
options(dviExtension="pdf")  ## Macintosh, Windows, linux

options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
## or
options(xdvicmd="xdg-open")  ## ubuntu linux

## See ?Hmisc::latex for discussion of these options.
}


## This example writes a set of pdf files and then uses the Hmisc::latex
## function to display them in LaTeX.

## The graphs are constructed three times, once each with lattice,
## base graphics, and ggplot2.

\dontrun{
  tmp <- matrix(rnorm(20), 2, 5, byrow=TRUE,
                dimnames=list(c("A", "B"), paste0("X", 1:5)))

  tmp.df <- data.frame(y=as.vector(t(tmp)),
                      group=factor(rep(row.names(tmp), each=5)))
  tmp.df
}

  ## All three examples use the pdf device

  ## lattice example
\dontrun{

  library(lattice)

  tmp.lattice <- bwplot(group ~ y | group, data=tmp.df, layout=c(1,2))
  tmp.lattice

  pdf("tmpl\%03d.pdf", onefile=FALSE, height=.4, width=4) ## inch
  update(tmp.lattice, layout=c(1,1), xlab=NULL, ylab=NULL,
         par.settings=list(layout.heights=layoutHeightsCollapse(),
                           layout.widths=layoutWidthsCollapse(),
                           axis.line=list(col="transparent")),
         scales=list(y=list(relation="free", at=NULL)))
  dev.off()

  tmpl.graphnames <- paste0("tmpl", sprintf("\%03i", 1:2), ".pdf")

  tmpl.display <- data.frame(round(tmp, 2),
                             graphs=as.includegraphics(tmpl.graphnames, raise="-.55ex"))
  tmpl.display

  tmpl.latex <- Hmisc::latex(tmpl.display)
  tmpl.latex$style <- "graphicx"
  tmpl.latex  ## this line requires latex in the PATH, and Hmisc to be loaded
  ## Hmisc::print.latex(tmpl.latex) ## if Hmisc is not loaded

}

## base graphics example
\dontrun{

  pdf("tmpb\%03d.pdf", onefile=FALSE, height=.5, width=3) ## inch
  par( bty="n", xaxt="n", omd=c(0,1, 0,1), mai=c(0,0,0,0))
  boxplot(tmp["A",], horizontal=TRUE, ylim=range(tmp)) ## ylim is correct for horizontal plot
  boxplot(tmp["B",], horizontal=TRUE, ylim=range(tmp)) ## ylim is correct for horizontal plot
  dev.off()

  tmpb.graphnames <- paste0("tmpb", sprintf("\%03i", 1:2), ".pdf")

  tmpb.display <-
    data.frame(round(tmp, 2),
               graphs=as.includegraphics(tmpb.graphnames, height="2em", raise="-1.4ex"))
  tmpb.display

  tmpb.latex <- Hmisc::latex(tmpb.display)
  tmpb.latex$style <- "graphicx"
  tmpb.latex  ## this line requires latex in the PATH, and Hmisc to be loaded
  ## Hmisc::print.latex(tmpb.latex) ## if Hmisc is not loaded

}

## ggplot2 example, whole set constructed as a unit, then printed one panel at a time.
\dontrun{

  library(ggplot2)
  tmpga <-
    ggplot(tmp.df, aes(group, y)) +
      geom_boxplot(outlier.size = 8) +
        coord_flip() +
          theme_collapse()
  tmpga ## on interactive device

  pdf("tmpga\%03d.pdf", onefile=FALSE, height=1, width=4) ## inch
  for (i in 1:length(levels(tmp.df$group))) {
    tmpga$coordinates$limits$x <- c(i, i)  ## I dislike this usage.
                                        ## I want to use "+" with some ggplot2 function,
          ## but have not figured out the right incantation.
    print(tmpga)
  }
  dev.off()

  graphnames <- paste0("tmpga", sprintf("\%03i", 1:2), ".pdf")

  tmpga.display <- data.frame(round(tmp, 2),
                            graphs=as.includegraphics(graphnames, raise="-.7ex"))
  tmpga.display

  tmpga.latex <- Hmisc::latex(tmpga.display)
  tmpga.latex$style <- "graphicx"
  tmpga.latex  ## this line requires latex in the PATH, and Hmisc to be loaded
  ## Hmisc::print.latex(tmpga.latex) ## if Hmisc is not loaded

}

## ggplot2 example, constructed one panel at a time.
\dontrun{

  library(ggplot2)

  pdf("tmpgb\%03d.pdf", onefile=FALSE, height=1, width=4) ## inch
  ggplot(tmp.df[1:5,], aes(group, y)) +
    geom_boxplot(outlier.size = 8) + ylim(range(tmp.df[,1])) +
      coord_flip() +
        theme_collapse()
  ggplot(tmp.df[6:10,], aes(group, y)) +
    geom_boxplot() + ylim(range(tmp.df[,1])) +
      coord_flip() +
        theme_collapse()
  dev.off()

  graphnames <- paste0("tmpgb", sprintf("\%03i", 1:2), ".pdf")

  tmpgb.display <- data.frame(round(tmp, 2),
                            graphs=as.includegraphics(graphnames, raise="-.7ex"))
  tmpgb.display

  tmpgb.latex <- Hmisc::latex(tmpgb.display)
  tmpgb.latex$style <- "graphicx"
  tmpgb.latex  ## this line requires latex in the PATH, and Hmisc to be loaded
  ## Hmisc::print.latex(tmpgb.latex) ## if Hmisc is not loaded

}

## xtable example
\dontrun{
  tmpl.display ## from lattice example above
  tmplx.name <- "tmpldisplayxtable.tex"
  print(xtable::xtable(tmpl.display),
        caption.placement = "top",
	sanitize.text.function = function(x) x,  ## xtable converts "\abc" to "$\backslash$abc"
        file=tmplx.name)                         ## sanitize restores it back to "\abc".
  tmpx.latex <- list(file=tmplx.name, style="graphicx")
  class(tmpx.latex) <- "latex"

  tmpx.latex  ## this line requires latex in the PATH, and Hmisc to be loaded
  ## Hmisc::print.latex(tmpx.latex) ## if Hmisc is not loaded
}


## Please see the demos for more interesting examples.
##xs demo(package="microplot")

}
