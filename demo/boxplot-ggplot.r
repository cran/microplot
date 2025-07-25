library(microplot)
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

library(ggplot2)

cc176.current <- cbind(cc176, y.adj=cc176.y.adj, fake="ff")

BW <-
  ggplot(cc176.current, aes(fake, y.adj)) +
  geom_boxplot(color = col3x2[1:4]) +
  facet_wrap(~ current, ncol=1, as.table=FALSE) +
  coord_flip()
BW

latex(BW, height.panel=.2, width.panel=2, height.x.axis=.2,
      rowlabel="Current", vectorgraph.colname="Boxplots", y.axis=FALSE)

latex(BW, height.panel=.2, width.panel=2, height.x.axis=.2,
      rowlabel="Current", vectorgraph.colname="Boxplots", y.axis=FALSE,
      dataobject=formatDF(cc176fivenumsd[4:1,], dec=2))


## Word

## graph
msWord(BW, height.panel=.2, width.panel=2, height.x.axis=.2,
       rowlabel="Current", width.rowname=.8,
       vectorgraph.colname="Boxplots", y.axis=FALSE, title="Box1")

## graph + data
msWord(BW, height.panel=.2, width.panel=2, height.x.axis=.2,
       rowlabel="Current", width.rowname=.8,
       vectorgraph.colname="Boxplots", y.axis=FALSE, title="Box2",
       dataobject=format(cc176fivenumsd[4:1,], digits=4), width.dataobject=.6)

## suppress x axis
msWord(BW, height.panel=.2, width.panel=2, height.x.axis=.2,
       rowlabel="Current", width.rowname=.8,
       vectorgraph.colname="Boxplots", y.axis=FALSE, title="Box3",
       x.axis=FALSE)

detach("package:ggplot2") ## can't unload, imported by other packages
