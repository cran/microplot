latexCheckOptions()

if (FALSE) { ## These are the settings for my machines
  ## Set options for Hmisc::latex
  options(latexcmd='pdflatex')
  options(dviExtension='pdf')
  options(xdvicmd='open') ## Macintosh, Windows, SMP linux
  latexCheckOptions()
}

library(HH)

## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses lattice graphics.
## See the file bwplot.ggplot.r for a similar example with ggplot2 graphics.
## See ?microplot for simple examples with lattice graphics, base graphics, and ggplot2 graphics.


## I recommend demo("bwplot.lattice", package="microplot", ask=FALSE)

## This example is based on the example in Figure 13.2, page 431 of
## the Second Edition of Statistical Analysis and Data Display,
## Richard M. Heiberger and Burt Holland, Springer 2015.
##      http://www.springer.com/us/book/9781493921218

## The book's version (HH::HHscriptnames(13) ## chunk 6) does manual
## alignment.  The version here uses the LaTeX tabular environment for
## alignment.


## This example is completely reproducible without looking at the
## script in the HH package.
##
## This example starts with a minimal script based on chunks 1, 2, 4, 6
data(cc176, package="HH")
data(col3x2, package="HH")
cc176.aov <- aov(wt.d ~ rep + wt.n + n.treats*minutes*current,
                 data=cc176)

cc176$y.adj <- cc176$wt.d  -
  (cc176$wt.n - mean(cc176$wt.n))*coef(cc176.aov)["wt.n"]

tmp <-
sapply(split(cc176$y.adj, cc176$current),
       function(x)
         c(min=min(x),
           "m-sd"=mean(x)-sd(x),
           mean=mean(x),
           "m+sd"=mean(x)+sd(x),
           max=max(x)))

t(tmp)[4:1,]
## end of minimal script

## the next section is the new material using the microplot package.
##
BW <-
lattice::bwplot(HH::unpositioned(current) ~ y.adj | current, data=cc176,
       panel=HH::panel.bwplot.intermediate.hh,
       xlab=NULL,
       par.settings=list(
         layout.heights=layoutHeightsCollapse(),
         layout.widths=layoutWidthsCollapse(),
         axis.line=list(col="transparent")),
       layout=c(1,1),
       scales=list(y=list(relation="free"))
       )

pdf("cc176bwplot%03d.pdf", onefile=FALSE, height=.4, width=3)  ## inch ## BB = 0 0 216 28
BW  ## four individual boxplots without axes
update(BW[3], ## x-axis
       par.settings=list(layout.heights=list(axis.bottom=1, panel=0),
                         axis.line=list(col="black")))
dev.off()

graphnames <- c(
"cc176bwplot001.pdf",
"cc176bwplot002.pdf",
"cc176bwplot003.pdf",
"cc176bwplot004.pdf",
"cc176bwplot005.pdf")

graphicsnames <- as.includegraphics(graphnames)

treatment <-
data.frame(rbind(format(t(tmp), digits=4), ""),
           bwplot=graphicsnames,
           check.names=FALSE)[c(4:1, 5),]
treatment

## Without a displayed x-axis
cc176.latex <- Hmisc::latex(treatment[1:4,], rowlabel="Treatment")
cc176.latex$style <- "graphicx"
cc176.latex  ## this line requires latex in the path

## With a displayed x-axis
cc176x.latex <- Hmisc::latex(treatment, rowlabel="Treatment")
cc176x.latex$style <- "graphicx"
cc176x.latex  ## this line requires latex in the PATH
