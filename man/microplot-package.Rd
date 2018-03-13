\name{microplot-package}
\alias{microplot-package}
\alias{sparkline}
\docType{package}
\title{
  Display microplots (sparklines) from R graphics panels in tables in
  'LaTeX', 'Word', 'HTML', 'Excel'.
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
by any graphical system in 'R'.  We show \pkg{lattice}, \pkg{ggplot2}, and \pkg{base}
graphics.  The tables can be targeted for display in 'LaTeX', 'MS Word'
on any operating system, 'MS Excel' on 'Windows', or 'HTML'.  We show examples of each.

The functions produce valid 'LaTeX' \code{.tex} files or 'Word'
\code{.docx} files in the working directory.  If 'LaTeX' or 'Word' are
installed, then the generated files can be displayed on screen as
illustrated in the help file examples and the \code{demo} directory.  The
\code{.tex} files can be \code{\\include}d in a larger \code{.tex} file.
Or the generated \code{.pdf} file can be displayed in `LaTeX' with
an \code{\\includegraphics} statement.
The images in the displayed \code{.docx} file can be copied and pasted
into a larger 'Word' file.

The best way to learn this package is to read the examples and \code{demo} files.
The primary function \code{\link{microplot}} takes a \code{trellis} or \code{ggplot}
object and generates a set of graphics files, one per panel of a
multi-panel display.  The \code{latex} and \code{msWprd} functions place
the graphics files into a table.

The \code{latex} examples (in the help file \code{examples}) and \code{demo}
files use the operating system \code{pdflatex} command with the 'R'
\code{pdf()} or \code{png()} graphics device.  Or they could use the
operating system \code{latex} command with the 'R'
\code{ps()} graphics device.  They therefore require that the
three options\cr
   \code{options()[c("latexcmd","dviExtension","xdvicmd")]}\cr
   all be set consistently.
   The recommended settings for \code{pdflatex} with \code{pdf} graphics files
   are included as the defaults in the function call\cr
   \code{latexSetOptions()}\cr
   The recommended settings for \code{latex} with \code{ps} graphics files
   may be specified with the function call\cr
   \code{latexSetOptions("latex")}\cr

Please see \code{\link{latexSetOptions}} for details on the recommended
settings
for use with the \pkg{microplot} package.  See the "System options"
section in the "Details" section of
\code{\link[Hmisc]{latex}} for discussion of the options themselves.

The examples in this \code{help} file are inside \code{dontrun} environments
because they depend on \code{options} and they write files.  You must set the
\code{options} for your system before running the example manually.

Most of the 'LaTeX' examples are shown using the \code{Hmisc::latex} function
\code{\link[Hmisc]{latex}}
(I am coauthor of that function).
The \pkg{microplot} package also works with the \code{xtable::xtable}
function \code{\link[xtable]{xtable}}.
An example in the \code{demo} directory shows a simple use of \code{xtable}.

The demos in the \code{demo} directory are not inside a \code{dontrun}
environment.  You must set the options for your system before running
them.  Each demo sets the options for \code{pdfltex}.  Should you prefer
\code{latex} you will need to run the demos manually.
I recommend that you run the demos with \code{ask=TRUE} because
will need to read them closely to see what they are doing.

To run the demos manually (with a stop at each graph), use
\preformatted{
     demo("HowToUseMicroplot"         , package="microplot", ask=TRUE)
     demo("latex"                     , package="microplot", ask=TRUE)
     demo("latex-ggplot"              , package="microplot", ask=TRUE)
     demo("msWord"                    , package="microplot", ask=TRUE)
     demo("LegendrePolynomials"       , package="microplot", ask=TRUE)
     demo("timeseries"                , package="microplot", ask=TRUE)
     demo("NTplot"                    , package="microplot", ask=TRUE)
     demo("bwplot-lattice"            , package="microplot", ask=TRUE)
     demo("boxplot-ggplot"            , package="microplot", ask=TRUE)
     demo("tablesPlusGraphicColumn"   , package="microplot", ask=TRUE)
     demo("regrcoef"                  , package="microplot", ask=TRUE)
     demo("iris"                      , package="microplot", ask=TRUE)
     demo("AEdotplot"                 , package="microplot", ask=TRUE)
     demo("xtable"                    , package="microplot", ask=TRUE)
     demo("Examples"                  , package="microplot", ask=TRUE)
   }
To run the demos automatically, with no stops, use \code{ask=FALSE}.

The \code{examples} directory
\code{system.file(package="microplot", "examples")}
includes complete working examples of \pkg{Sweave} (both LaTeX-style and
Noweb-style), \pkg{knitr}, \code{emacs} \pkg{orgmode}, and
\pkg{rmarkdown}
input files and their \code{pdf} output files.  These files must be
copied into a directory in which you have write privilege, and that
directory must be made the current working directory with \code{setwd}.
They will not work from the installed package directory.

The 'Excel' for Windows example is in file
\code{examples/irisExcel.xls}.  The 'VBA' code in that file shows how to
place the individual microplots into 'Excel' cells.

See also the vignette:\cr
\code{vignette("rmhPoster", package="microplot")}

}
\author{
\packageAuthor{microplot}

Maintainer: \packageMaintainer{microplot}
}
\keyword{ package }
\keyword{ microplot }
\keyword{ sparkline }
\keyword{ LaTeX }
\keyword{ HTML }
\keyword{ Excel }
\keyword{ Sweave }
\keyword{ knitr }
\keyword{ rmarkdown }
\keyword{ org-mode }
\keyword{ Word }
\keyword{ utilities}
\keyword{ lattice}
\keyword{ ggplot}
\keyword{ documentation }
\keyword{ xtable }

\seealso{
\code{\link[Hmisc]{latex}}, \code{\link{latex.trellis}}, \code{\link{microplot}}, \code{\link{as.includegraphics}}

}
\examples{
## This example writes a set of pdf files and then uses the latex
## function to display them in LaTeX.

## The graphs are constructed three times, once each with lattice,
## base graphics, and ggplot2.

\dontrun{

  ## 0. set options for pdflatex and pdf graphics files
  latexSetOptions()


  ## 1. define dataset
  tmp <- matrix(rnorm(10), 2, 5, byrow=TRUE,
                dimnames=list(c("A", "B"), paste0("X", 1:5)))

  tmp.df <- data.frame(y=as.vector(t(tmp)),
                       group=factor(rep(row.names(tmp), each=5)))
  tmp.df


  ## 2. lattice example for latex and msWord

  tmp.lattice <- lattice::bwplot(group ~ y | " " * group, data=tmp.df, layout=c(1,2),
                                 as.table=TRUE, xlim=c(-2.1, 1.3),
                                 scales=list(y=list(relation="free", at=NULL)))
  tmp.lattice


  ## 3. using the latex.trellis method
  latex(tmp.lattice,
        height.panel=.3, width.panel=3, ## inches
        x.axis=TRUE, y.axis=FALSE,
        rowlabel="group", caption="latex.trellis of lattice graph column")

  latex(tmp.lattice, dataobject=formatDF(tmp, dec=2),
        height.panel=.3, width.panel=1.5, ## inches
        x.axis=FALSE, y.axis=FALSE,
        rowlabel="group", caption="latex.trellis of numeric data and lattice graph column")


  ## 4. MS Word example.  Uses functions in ReporteRs package.

  tmplw.docx <-
    msWord(tmp.lattice, dataobject=format(tmp, digits=2),
           height.panel=.3, width.panel=2, ## inches
           height.x.axis=.35, width.y.axis=.3,
           figPrefix="tmplw",
           y.axis=FALSE,
           rowlabel="group", width.rowname=.6,
           data.header="data values", width.dataobject=.6,
           graph.header="bwplot",
           caption="Lattice bwplot using msWord function")
  print.default(tmplw.docx)
  tmplw.docx  ## print method opens file
  ## cut and paste this graph into a larger .docx file.


  ## 5. ggplot2 example for latex and msWord

  library(ggplot2)

  tmpga.df <- cbind(tmp.df, fake="ff")
  tmpga <-
    ggplot(tmpga.df, aes(fake, y)) +
    geom_boxplot(outlier.size = 2) +
    facet_wrap(~ group, ncol=1) +
    coord_flip() + ylim(-2, 1.1)
  tmpga ## on interactive device


  ## 6. using the latex.ggplot method

  latex(tmpga, height.x.axis=.2, width.y.axis=.2, y.axis=FALSE)

  latex(tmpga, dataobject=formatDF(tmp, dec=2),
        height.panel=.5, height.x.axis=.2, width.y.axis=.2, y.axis=FALSE)


  ## 7. msWord with ggplot

  tmpga.docx <-
    msWord(tmpga, dataobject=format(tmp, digits=2),
           height.panel=.25, height.x.axis=.2, width.y.axis=.2, y.axis=FALSE,
           rowlabel="group", width.rowname=.6,
           data.header="data values", width.dataobject=.6,
           graph.header="bwplot",
           caption="ggplot2 boxplot using msWord function")
  print.default(tmpga.docx)
  tmpga.docx  ## print method opens file


  detach("package:ggplot2")


  ## 8. base graphics example
  ## This must be done with an explicit loop because
  ## base graphics doesn't produce a graphics object.

  dir.verify("tmpb")
  pdf("tmpb/fig\%03d.pdf", onefile=FALSE, height=.5, width=3) ## inch
  par( bty="n", xaxt="n", omd=c(0,1, 0,1), mai=c(0,0,0,0))
  boxplot(tmp["A",], horizontal=TRUE, ylim=range(tmp)) ## ylim for horizontal plot
  boxplot(tmp["B",], horizontal=TRUE, ylim=range(tmp)) ## ylim for horizontal plot
  dev.off()

  tmpb.graphnames <- paste0("tmpb/fig", sprintf("\%03i", 1:2), ".pdf")

  tmpb.display <-
    data.frame(round(tmp, 2),
               graphs=as.includegraphics(tmpb.graphnames, height="2em", raise="-1.4ex"))
  tmpb.display

  ## we are now using the latex.data.frame method in the Hmisc package
  tmpb.latex <- latex(tmpb.display, rowlabel="group",
                      caption="latex.default of base graphs")
  tmpb.latex$style <- "graphicx"
  tmpb.latex  ## this line requires latex in the PATH


  ## 9. detail for latex of lattice.  This is essentially what the
  ## latex.trellis method does all together.

  dir.verify("tmpl") ## create a new subdirectory of the working directory
  pdf("tmpl/fig\%03d.pdf", onefile=FALSE, height=.5, width=2.5) ## inch
  update(tmp.lattice, layout=c(1,1), xlab="",
         par.settings=list(layout.heights=layoutHeightsCollapse(),
                           layout.widths=layoutWidthsCollapse(),
                           axis.line=list(col="transparent"),
                           strip.border=list(col="transparent")))
  dev.off()

  tmpl.graphnames <- paste0("tmpl/fig", sprintf("\%03i", 1:2), ".pdf")
  names(tmpl.graphnames) <- rownames(tmp)

  tmpl <-
    as.includegraphics(tmpl.graphnames)
  ## retains dimensions from pdf() statement
  tmpl
  tmpl.latex <- latex(tmpl, rowlabel="group",
                      caption="latex.default of lattice graph column")
  tmpl.latex  ## this line requires latex in the PATH

  tmplw <-
    data.frame(round(tmp, 2),
               graphs=as.includegraphics(tmpl.graphnames, width="1in"))
  ## retains aspect ratio from pdf() statement
  tmplw

  tmplw.latex <- latex(tmplw, rowlabel="group",
                       caption="latex.default of numeric data and lattice graph column")
  tmplw.latex$style <- "graphicx"
  tmplw.latex  ## this line requires latex in the PATH


  ## 10. detail for latex of ggplot.
  ## left as an exercise.  It is very similar to the detail for latex with lattice.

}

## Please see the demos for more interesting examples.
## demo(package="microplot")

}
