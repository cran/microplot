## MS Word example using lattice png files.  The functions in the ReporteRs
## package are used to build the .docx file.

## This example shows lattice graphics and MS Word.
## For ggplot2 graphics and MS Word see demo("boxplot-ggplot-MSWord", ask=FALSE).

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

t(tmp)[4:1,]  ## table as printed by R
## end of minimal script


## 2. lattice.  produce plot, conditioned on current to get one box per panel
BW <-
lattice::bwplot(HH::unpositioned(current) ~ y.adj | current, data=cc176,
       panel=HH::panel.bwplot.intermediate.hh,
       strip=FALSE, layout=c(1,4), scales=list(y=list(relation="free")))
BW  ## on interactive device


## 3. use microplot() to get a set of png files, one per panel, and one more for the axis
graphnames <- microplot(BW, height=.3, width=2, device="png")



## 4. Use MSWord_TablePlusGraphicColumn to construct an MS Word table.
##    MSWord_TablePlusGraphicColumn is a wrapper that uses functions in the ReporteRs package.

bwplot.docx <-
  MSWord_TablePlusGraphicColumn(format(t(tmp)[4:1,], digits=4), title="bwplot",
                                rowname.header="Current",
                                data.header="5 number summary",
                                graph.header="bwplot",
                                panel.files=graphnames,
                                axis.files=attr(graphnames, "axis.names"),
                                height=.3, width=2, ## inches
                                FlexTableWidths=c(1, rep(.7, 5), 2), ## inches, rownames count as a column
                                rmh.borders=TRUE,
                                caption="lattice bwplot using MSWord_TablePlusGraphicColumn function")

bwplot.docx  # file name
system(paste("open", bwplot.docx))  ## pick up this graph and paste it into a larger .docx file.


## 5. Use MSWord_TablePlusGraphicColumn(..., filetype="html") to construct an HTML table.
##    MSWord_TablePlusGraphicColumn is a wrapper that uses functions in the ReporteRs package.

bwplot.html <-
  MSWord_TablePlusGraphicColumn(format(t(tmp)[4:1,], digits=4), title="bwplot",
                                rowname.header="Current",
                                data.header="5 number summary",
                                graph.header="bwplot",
                                panel.files=graphnames,
                                axis.files=attr(graphnames, "axis.names"),
                                height=.3, width=2, ## inches
                                FlexTableWidths=c(1, rep(.7, 5), 2), ## inches, rownames count as a column
                                rmh.borders=TRUE,
                                caption="lattice bwplot using MSWord_TablePlusGraphicColumn function",
                                filetype="html")

bwplot.html  # file name
system(paste("open", bwplot.html))
