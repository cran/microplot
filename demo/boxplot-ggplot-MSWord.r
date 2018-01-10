## MS Word example using ggplot2 png files. The functions in the ReporteRs
## package are used to build the .docx file.

library(ggplot2)

## This example shows ggplot graphics and MS Word.
## For lattice graphics and MS Word see demo("bwplot-lattice-MSWord", ask=FALSE).

## This example is based on the example in Figure 13.2, page 431 of
## the Second Edition of Statistical Analysis and Data Display,
## Richard M. Heiberger and Burt Holland, Springer 2015.
##      http://www.springer.com/us/book/9781493921218


## 1. Produce table of numbers
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


## 2. ggplot
## make single plot with all features
BW <- ggplot(cc176, aes(current, y.adj)) +
  geom_boxplot(color = "darkblue", outlier.shape = 1, outlier.size = 2, size = 0.4) +
  coord_flip()
BW  ## on interactive device

## I really want microplot() here.  I am not familiar enough with ggplot to write that function myself.
## microplot_yfactor.ggplot is designed for this special case of a factor on the y-axis.
graphnames <- microplot_yfactor(BW, height=.4, width=3, n=length(levels(cc176$current)), ## inches
                                device="png")  ## word, png

## 4. Use MSWord_TablePlusGraphicColumn to construct an MS Word table.
##    MSWord_TablePlusGraphicColumn is a wrapper that uses functions in the ReporteRs package.

boxplot.docx <-
  MSWord_TablePlusGraphicColumn(format(t(tmp)[4:1,], digits=4), title="boxplot",
                                rowname.header="Current",
                                data.header="5 number summary",
                                graph.header="boxplot",
                                panel.files=graphnames[4:1],
                                axis.files=attr(graphnames, "axis.names"),
                                height=.3, width=2, ## inches
                                FlexTableWidths=c(1, rep(.7, 5), 2), ## inches, rownames count as a column
                                rmh.borders=TRUE,
                                caption="ggplot boxplot using MSWord_TablePlusGraphicColumn function")

boxplot.docx  # file name
system(paste("open", boxplot.docx))  ## pick up this graph and paste it into a larger .docx file.


