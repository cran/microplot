## These are the settings for my machines
## Set options for Hmisc::latex
options(latexcmd='pdflatex')
options(dviExtension='pdf')
if (nchar(Sys.which("open"))) {
  options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
} else {
  options(xdvicmd="xdg-open")  ## ubuntu linux
}
latexCheckOptions()


## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses lattice graphics.
## See ?microplot for simple examples with base graphics and ggplot2.

## I recommend demo("iris", package="microplot", ask=FALSE)


iris.melt <- reshape2::melt(iris, id="Species")
irisBW <- lattice::bwplot( ~ value | Species * variable, data=iris.melt)
irisBW
latticeExtra::useOuterStrips(irisBW)

graphnames <- microplot(irisBW, height=.4, width=1, device="pdf") ## inches

graphicsnames <- as.includegraphics(graphnames, height="2em", raise="-1.3ex")

## 12 box plots in 4 rows and 3 columns
BW.latex <- Hmisc::latex(graphicsnames,
                         rowlabel="Measurement",
                         cgroup="Species",
                         n.cgroup=3,
                         caption="Box Plots for each Species and Measurement.",
                         label="irisBW")
BW.latex$style <- "graphicx"
BW.latex  ## this line requires latex in the path


## Each of the twelve with its five number summary
iris2 <- array(iris.melt$value,
               dim=c(50, 3, 4),
               dimnames=list(NULL,
                 levels(iris.melt$Species),
                 levels(iris.melt$variable)))
iris2.fivenum <- apply(iris2, 2:3, fivenum)
dimnames(iris2.fivenum)[[1]] <- c("min", "Q1", "med", "Q3", "max")
iris2.fivenum.f <- format(iris2.fivenum, nsmall=2)

BW5num <-
rbind(
cbind(t(iris2.fivenum.f[,1,4:1]), "Box Plots"=graphicsnames[,1]),
cbind(t(iris2.fivenum.f[,2,4:1]), "Box Plots"=graphicsnames[,2]),
cbind(t(iris2.fivenum.f[,3,4:1]), "Box Plots"=graphicsnames[,3]))

BW5num.latex <- Hmisc::latex(format(BW5num, nsmall=2),
                             rowlabel="Measurement",
                             rgroup=levels(iris.melt$Species),
                             n.rgroup=c(4,4,4),
                             cgroup=c("Five Number Summary", ""),
                             n.cgroup=c(5, 1),
                             caption="Five Number Summary and Box Plots for each Species and Measurement.",
                             label="irisBW5num")
BW5num.latex$style <- "graphicx"
BW5num.latex  ## this line requires latex in the path


## with the latex.trellis function just the figure, no numerical values

Hmisc::latex(irisBW, height=.3, raise="-2ex", y.axis=FALSE)

Hmisc::latex(t(irisBW), height=.3, raise="-2ex", y.axis=FALSE)
t(irisBW)
