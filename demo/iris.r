library(microplot)
## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()


## This example uses lattice graphics.
## See ?microplot for simple examples with base graphics and ggplot2.

## I recommend demo("iris", package="microplot", ask=FALSE)


iris.melt <- reshape2::melt(iris, id="Species")
irisBW <- lattice::bwplot( ~ value | Species * variable, data=iris.melt, xlab="")
latticeExtra::useOuterStrips(irisBW)

latex(irisBW)

## 12 box plots in 4 rows and 3 columns
latex(irisBW,
      height.panel=.3, height.x.axis=.45, y.axis=FALSE,
      caption="Box Plots for each Species and Measurement.",
      rowlabel="Measurement")


## Each of the twelve with its five number summary
iris2 <- array(iris.melt$value,
               dim=c(50, 3, 4),
               dimnames=list(NULL,
                 levels(iris.melt$Species),
                 levels(iris.melt$variable)))
iris2.fivenum <- apply(iris2, 2:3, fivenum)
dimnames(iris2.fivenum)[[1]] <- c("min", "Q1", "med", "Q3", "max")
iris2.fivenum.f <- format(iris2.fivenum, nsmall=2)
names(dimnames(iris2.fivenum.f)) <- c("fivenum", "Species", "variable")

irisBWSv <- lattice::bwplot( ~ value | interaction(variable,
                                                   factor(Species, levels=rev(levels(Species)))),
                            data=iris.melt, xlab="", layout=c(1, 12))
irisBWSv

BW5num <-
  rbind(
    t(iris2.fivenum.f[,1,4:1]),
    t(iris2.fivenum.f[,2,4:1]),
    t(iris2.fivenum.f[,3,4:1]))
BW5num

latex(irisBWSv, dataobject=BW5num,
      height.panel=.3, height.x.axis=.37, y.axis=FALSE,
      rowlabel="Measurement", vectorgraph.colname="Box Plots",
      rgroup=c(levels(iris.melt$Species), ""),
      n.rgroup=c(4,4,4,1),
      cgroup=c("Five Number Summary", ""),
      n.cgroup=c(5, 1),
      caption="Five Number Summary and Box Plots, with x axis.")

latex(irisBWSv, dataobject=BW5num,
      height.panel=.3, height.x.axis=.37, y.axis=FALSE, x.axis=FALSE,
      rowlabel="Measurement", vectorgraph.colname="Box Plots",
      rgroup=levels(iris.melt$Species),
      n.rgroup=c(4,4,4),
      cgroup=c("Five Number Summary", ""),
      n.cgroup=c(5, 1),
      caption="Five Number Summary and Box Plots, without x axis.")
