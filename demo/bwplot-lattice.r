## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()


## This example is based on the example in Figure 13.2, page 431 of
## the Second Edition of Statistical Analysis and Data Display,
## Richard M. Heiberger and Burt Holland, Springer 2015.
##      http://www.springer.com/us/book/9781493921218

## The book's version (HH::HHscriptnames(13) ## chunk 6) does manual
## alignment.  The version here uses the LaTeX tabular environment for
## alignment.

data(cc176, package="HH")
data(col3x2, package="HH")
data(cc176.y.adj, package="microplot")  ## cc176.y.adj, cc176fivenumsd ## see ?cc176.y.adj


## 1. lattice.  produce plot, conditioned on current to get one box per panel
BW <-
  lattice::bwplot(HH::unpositioned(current) ~ cc176.y.adj | current, data=cc176,
                  xlab="", col=col3x2,
                  panel=HH::panel.bwplot.intermediate.hh,
                  strip=FALSE, layout=c(1,4), scales=list(y=list(relation="free")))
BW


## latex
latex(BW, y.axis=FALSE)

latex(BW, y.axis=FALSE, height.panel=.25, width.panel=2, height.x.axis=.47,
      rowlabel="Current", vectorgraph.colname="Boxplots")

latex(BW, dataobject=formatDF(cc176fivenumsd[4:1,], dec=2), y.axis=FALSE,
      height.panel=.25, width.panel=2, height.x.axis=.47,
      rowlabel="Current", vectorgraph.colname="Boxplots")


## Without a displayed x-axis
latex(BW, dataobject=formatDF(cc176fivenumsd[4:1,], dec=2), y.axis=FALSE,
      height.panel=.25, width.panel=2, height.x.axis=.47,
      rowlabel="Current", vectorgraph.colname="Boxplots",
      x.axis=FALSE)



## Word


## with x axis
bwplot1.docx <- msWord(BW, height.panel=.3, width.panel=2, y.axis=FALSE, title="BW1",
                       rowlabel="Current", width.rowname=1, vectorgraph.colname="Box Plots")
bwplot1.docx

bwplot2.docx <- msWord(BW, height.panel=.3, width.panel=2, y.axis=FALSE, title="BW2",
                       rowlabel="Current", width.rowname=1, vectorgraph.colname="Box Plots",
                       dataobject=format(cc176fivenumsd[4:1,], digits=4), width.dataobject=.6)
bwplot2.docx


## without x axis
bwplot3.docx <- msWord(BW, height.panel=.3, width.panel=2, y.axis=FALSE, title="BW3",
                       rowlabel="Current", width.rowname=1, vectorgraph.colname="Box Plots",
                       x.axis=FALSE)
bwplot3.docx

bwplot4.docx <- msWord(BW, height.panel=.3, width.panel=2, y.axis=FALSE, title="BW4",
                       rowlabel="Current", width.rowname=1, vectorgraph.colname="Box Plots",
                       dataobject=format(cc176fivenumsd[4:1,], digits=4), width.dataobject=.6,
                       x.axis=FALSE)
bwplot4.docx




## With explicit call to microplot

graphicsnames.png <-
  microplot(BW, height.panel=.3, width.panel=2, device="png", vectorgraph.colname="Box Plots")

latex(
  as.includegraphics(
    microplotAttrDisplay(graphicsnames.png, y.axis=FALSE),
    width.includegraphics="2in"), ## needed with "png".
  rowlabel="Current")

msWord(graphicsnames.png, y.axis=FALSE, title="BW5",
       height.panel=.3, width.panel=2,  ## must be same as in microplot() call.
       width.rowname=1, rowlabel="Current")
