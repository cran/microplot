latexCheckOptions()

if (FALSE) { ## These are the settings for my machines
  ## Set options for Hmisc::latex
  options(latexcmd='pdflatex')
  options(dviExtension='pdf')
  options(xdvicmd='open') ## Macintosh, Windows, SMP linux
  latexCheckOptions()
}

library(HH)
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

## make single plot with all features
BW <- ggplot(cc176, aes(current, y.adj)) +
  geom_boxplot(color = "darkblue", outlier.shape = 1, outlier.size = 2, size = 0.4) +
  coord_flip()
BW  ## on interactive device

BW +
  xlab(NULL) + ylab(NULL) +
  theme_collapse(axis.ticks=element_line(),
                 axis.ticks.y = element_blank(),
                 axis.text=element_text(),
                 axis.text.y = element_blank(),
                 axis.title=element_text(),
                 axis.line.x=element_line(color="black"))
## on interactive device

BW+theme_collapse() ## on interactive device

BW.xlim <- BW$coordinates$limits$x  ## save x limits

pdf("cc176boxplot%03d.pdf", onefile=FALSE, height=.4, width=3)  ## inch ## BB = 0 0 216 28
for (i in 1:length(levels(cc176$current))) { ## four individual boxplots without axes
  BW$coordinates$limits$x <- c(i, i)
  print(BW + theme_collapse())
}
BW$coordinates$limits$x <- c(-1, -1)
BW + xlab(NULL) + ylab(NULL) +
  theme_collapse(axis.ticks=element_line(),
                 axis.ticks.y = element_blank(),
                 axis.text=element_text(),
                 axis.text.y = element_blank(),
                 axis.title=element_blank(),
                 axis.line.x=element_line(color="black"))
dev.off()

BW$coordinates$limits$x <- BW.xlim  ## restore x limits

graphnames <- c(
"cc176boxplot001.pdf",
"cc176boxplot002.pdf",
"cc176boxplot003.pdf",
"cc176boxplot004.pdf",
"cc176boxplot005.pdf")

graphicsnames <- as.includegraphics(graphnames)

treatment <-
data.frame(rbind(format(t(tmp), digits=4), ""),
           boxplot=graphicsnames,
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
