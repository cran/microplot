latexCheckOptions()

## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses lattice graphics.
## See ?microplot for simple examples with base graphics and ggplot2.

## I recommend demo("timeseries", package="microplot", ask=FALSE)

## This example is based on HH2 Figures 18.4 and 18.5, pages 651 and 652 of
## the Second Edition of Statistical Analysis and Data Display,
## Richard M. Heiberger and Burt Holland, Springer 2015.
##      http://www.springer.com/us/book/9781493921218
## This example does not use any functions or data from the HH package.

co2df <- data.frame(co2,
                    month=rep(
                      factor(month.name, levels=month.name),
                      length=length(co2)),
                    year=rep(factor(1959:1997), each=12, length=length(co2)))
head(co2df)
tail(co2df)

co2plot <-
  lattice::xyplot(co2 ~ month | year, data=co2df,
                  xlab=NULL, ylab=NULL,
                  type="b", col="black",
                  scales=list(y=list(at=c(320, 360))),
                  layout=c(1, 39),
                  main=" ",
                  par.settings=list(
                    layout.heights=layoutHeightsCollapse(),
                    layout.widths=
                      layoutWidthsCollapse(ylab.axis.padding=2, axis.left=1),
         ## axis.line=list(col="transparent"),
         clip=list(panel=FALSE)
       ))
co2plot


pdf("co2%03d.pdf", onefile=FALSE, height=.4, width=2)  ## inch
update(co2plot, layout=c(1, 1))
dev.off()

graphnames <- paste0("co2", sprintf("%03i", 1:39), ".pdf")

co2display <- data.frame(matrix(co2, byrow=TRUE, ncol=12,
                                dimnames=list(1959:1997, month.name)))
co2display$graphs <- as.includegraphics(graphnames)

co2.latex <- Hmisc::latex(co2display[seq(1, 39, 2), c(1,4,7,10,13)],
                          rowlabel="Year")
co2.latex$style <- "graphicx"
co2.latex  ## this line requires latex in the PATH
