latexCheckOptions()

if (FALSE) { ## These are the settings for my machines
  ## Set options for Hmisc::latex
  options(latexcmd='pdflatex')
  options(dviExtension='pdf')
  options(xdvicmd='open') ## Macintosh, Windows, SMP linux
  latexCheckOptions()
}

## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses lattice graphics.
## See ?microplot for simple examples with base graphics and ggplot2.

## I recommend demo("iris", package="microplot", ask=FALSE)


iris.melt <- reshape2::melt(iris, id="Species")
irisBW <- lattice::bwplot( ~ value | Species * variable, data=iris.melt)
irisBW
latticeExtra::useOuterStrips(irisBW)

irisBW.update <-
update(irisBW,
       xlab=NULL,
       par.settings=list(
         layout.heights=layoutHeightsCollapse(),
         layout.widths=layoutWidthsCollapse(),
         axis.line=list(col="transparent")),
       layout=c(1,1)
       )

pdf("irisBW%03d.pdf", onefile=FALSE, height=.4, width=1.6)  ## inch ## BB = 0 0 216 28
irisBW.update  ## twelve individual boxplots without axes
dev.off()

graphnames <- paste0("irisBW", sprintf("%03i", 1:12), ".pdf")

graphicsnames <- t(matrix(as.includegraphics(graphnames, height="2em", raise="-1.3ex"),
                          nrow=3, ncol=4,
                          dimnames=dimnames(irisBW)))

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

## Species and Measurement in same columns, Measurement names adjusted to be unique
BW5num <-
rbind(
data.frame(t(iris2.fivenum[,1,]), "Box Plots"=graphicsnames[,1], check.names=FALSE),
data.frame(t(iris2.fivenum[,2,]), "Box Plots"=graphicsnames[,2], check.names=FALSE),
data.frame(t(iris2.fivenum[,3,]), "Box Plots"=graphicsnames[,3], check.names=FALSE))

BW5num.latex <- Hmisc::latex(BW5num,
                             rowlabel="Measurement",
                             rgroup=levels(iris.melt$Species),
                             n.rgroup=c(4,4,4),
                             cgroup=c("Five Number Summary", ""),
                             n.cgroup=c(5, 1),
                             caption="Five Number Summary and Box Plots for each Species and Measurement.",
                             label="irisBW5num")
BW5num.latex$style <- "graphicx"
BW5num.latex  ## this line requires latex in the path


## Species and Measurement in separate columns
BW5num <-
rbind(
data.frame(t(iris2.fivenum[,1,]), "Box Plots"=graphicsnames[,1], check.names=FALSE),
data.frame(t(iris2.fivenum[,2,]), "Box Plots"=graphicsnames[,2], check.names=FALSE),
data.frame(t(iris2.fivenum[,3,]), "Box Plots"=graphicsnames[,3], check.names=FALSE))
BW5num$Measurement=levels(iris.melt$variable)
BW5num <- BW5num[, c(7,1:6)]

BW5num.latex <- Hmisc::latex(BW5num,
                             rowname=" ",
                             rowlabel="Species",
                             rgroup=levels(iris.melt$Species),
                             n.rgroup=c(4,4,4),
                             cgroup=c("", "Five Number Summary", ""),
                             n.cgroup=c(1, 5, 1),
                             caption="Five Number Summary and Box Plots for each Species and Measurement.",
                             label="irisBW5num")
BW5num.latex$style <- "graphicx"
BW5num.latex  ## this line requires latex in the path
