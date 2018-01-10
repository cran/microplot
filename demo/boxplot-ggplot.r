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


library(ggplot2)

## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses ggplot2 graphics.
## See the file bwplot.lattice.r for a similar example with lattice graphics.
## See ?microplot for simple examples with lattice graphics, base graphics, and ggplot2 graphics.

## I recommend demo("boxplot.ggplot", package="microplot", ask=FALSE)

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

## make single plot with all features
BW <- ggplot(cc176, aes(current, y.adj)) +
  geom_boxplot(color = "darkblue", outlier.shape = 1, outlier.size = 2, size = 0.4) +
  coord_flip()
BW  ## on interactive device

## remove background, labels, margins
BW+theme_collapse() ## on interactive device

## I really want microplot() here.  I am not familiar enough with ggplot to write that function myself.
## microplot_yfactor.ggplot is designed for this special case of a factor on the y-axis.
graphnames <- microplot_yfactor(BW, height=.2, width=2, n=length(levels(cc176$current))) ## inches  ## latex, pdf

graphicsnames <- as.includegraphics(graphnames, raise="-.6em")

treatment <-
data.frame(rbind(format(t(tmp), digits=4), ""),
           boxplot=c(graphicsnames, attr(graphicsnames,"axis.names")["x"]),
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


detach("package:ggplot2")
