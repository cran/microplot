library(microplot) ## microplot_1.0-29, microplot_1.0-25 is on CRAN

## Hmisc options for pdflatex
## graphics files are .pdf
latexSetOptions()





## Legendre Polynomials
data(LegendrePolyMatrices) ## see ?LegendraPolyMatrices
alphas <- dimnames(LegendrePolyMatrices)[[1]]
betas  <- dimnames(LegendrePolyMatrices)[[2]]


## set of plots
plots <-
  lapply(LegendrePolyMatrices, function(mab)
    lattice::xyplot(T.1 + T.2 + T.3 + T.4 ~ x, data=mab,
                    col=c("black","red","blue","green"), lwd=2, type="l",
                    par.settings=list(axis.line=list(col="gray50", lwd=1)),
                    xlim=c(-0.12, 1.12), ylim=c(-2.5, 3),
                    scales=list(x=list(at=c(0, .5, 1))),
                    xlab=expression(theta), ylab=list(expression(T[j]), rot=0),
                    key=list(columns=4,
                             text=list(expression("Leg"[1],"Leg"[2],"Leg"[3],"Leg"[4])),
                             lines=list(lwd=3),
                             col=c("black","red","blue","green"))))
dim(plots) <- dim(LegendrePolyMatrices)
dimnames(plots) <- dimnames(LegendrePolyMatrices)
plots <- graphicsList(plots)

## Dark border around the plots of interest to the researcher from
## whom this example is taken.  The other panels provide reference
## distributions.
plots[[2, 2]]$par.settings$axis.line <- list(col="black", lwd=3) ## jp.LP <- LP.score.plot(.5,.5)
plots[[3, 3]]$par.settings$axis.line <- list(col="black", lwd=3) ## a1b1.LP <- LP.score.plot(1,1)
plots[[2, 5]]$par.settings$axis.line <- list(col="black", lwd=3) ## LP.score.plot(.5,10)
## tmp <- sapply(plots, print) ## plots go to interactive device.

## one per page of interactive device
for (alpha in seq(along=alphas))
  for (beta in seq(along=betas))
    print(update(plots[[alpha, beta]], main=paste(alphas[alpha], betas[beta])))

## all 15 on same page of interactive device
for (i in 1:3)
  for (j in 1:5)
    print(update(plots[[i, j]],
                 main=paste(alphas[i], betas[j]), legend=NULL),
          split=c(j, i, 5, 3), more=TRUE)
grid::grid.newpage()

###########
## place all 15 trellis objects into 15 panels of a single trellis object
## this uses latticeExtra:::c.trellis and HH:::rbind.trellis
plots15 <-
  rbind(do.call(c, plots[1,]),
        do.call(c, plots[2,]),
        do.call(c, plots[3,]))
dimnames(plots15) <- rev(dimnames(plots))  ## note reversal!
plots15 <- latticeExtra::useOuterStrips(update(plots15, as.table=TRUE))  ## start at top
## lattice is row major; matrices are column major
## Display on interactive device.
## Note dark borders of several panels are lost.
plots15



## latex

## display in LaTeX of single trellis object with an array of panels.
dvi(                                     ## need more display width than default
  latex(plots15,                         ## one trellis object with 15 panels
        height.x.axis=.37, height.xlab=.13, width.y.axis=.44, width.ylab=.2,
        key=plots15$legend$top$args$key, ## lattice key is not always in the same place
        height.key=.2, width.key=5.5,    ## inches ## default key width is the same as panel width
        key.includegraphics=list(hspace.left="2.2in"), ## move key toward center
        xlab=TRUE, ylab=TRUE,            ## R xlab and ylab
        y.axis.includegraphics=list(hspace.left="-.2in")), ## reduce space between ylab and y.axis
  width=8.5)                             ## need more display width than default


## minimal arguments to see an array of individualized, multiple trellis objects in LaTeX
dvi(                                    ## need more width than default
  latex(plots,                          ## graphicsList of 15 trellis objects
        collapse=function(x)            ## keep the borders on each panel
          layoutCollapse(x, axis.line=list()),
        height.x.axis=.37, height.xlab=.13, width.y.axis=.44, width.ylab=.2,
        key=plots[[1,1]]$legend$top$args$key, ## lattice key is not always in the same place
        height.key=.2, width.key=5.5,   ## inches ## default key width is the same as panel width
        key.includegraphics=list(hspace.left="2in"), ## move key toward center
        xlab=TRUE, ylab=TRUE,           ## R xlab and ylab
        y.axis.includegraphics=list(hspace.left="-.2in")), ## reduce space between ylab and y.axis
  width=8.5, height=5.3)                ## need more width than default



## Annotated display in LaTeX, array of individualized, multiple trellis objects in LaTeX
##
## larger fonts in the LaTeX labeling, column grouping for panels
dimnames(plots) <- lapply(dimnames(LegendrePolyMatrices),
                          function(x) paste0("\\Huge\\strut", x)) ## larger font
dvi(                                    ## need more width than default
  latex(plots,                         ## vector of 15 trellis objects
        collapse=function(x)            ## keep the col and lwd of borders on each panel
          layoutCollapse(x, axis.line=list()),
        height.x.axis=.37, height.xlab=.13, width.y.axis=.44, width.ylab=.2,
        key=plots[[1,1]]$legend$top$args$key, ## lattice key is not always in the same place
        height.key=.2, width.key=5.5,   ## inches ## default key width is the same as panel width
        key.includegraphics=list(hspace.left="2in"), ## move key toward center
        xlab=TRUE, ylab=TRUE,           ## R xlab and ylab
        y.axis.includegraphics=list(hspace.left="-.2in"), ## reduce space between ylab and y.axis
        rowlabel="\\Huge$\\alpha$\\strut",
        cgroup=c("", "\\Huge$\\beta$"), n.cgroup=c(2,5),
        caption=
          "\\Huge The LP score functions $T_j = \\mbox{Leg}_j(G_{\\alpha,\\beta}(\\theta))$\\dots"),
  width=8.5, height=8)       ## need more width than default


## Annotated display in LaTeX, array of individualized, multiple trellis objects in LaTeX
## with latex labels for x and y axis
##
## larger fonts in the LaTeX labeling, column grouping for panels
dimnames(plots) <- lapply(dimnames(LegendrePolyMatrices),
                          function(x) paste0("\\Huge\\strut", x)) ## larger font
dvi(                                    ## need more width than default
  latex(plots,                         ## vector of 15 trellis objects
        collapse=function(x)            ## keep the col and lwd of borders on each panel
          layoutCollapse(x, axis.line=list()),
        height.x.axis=.37, height.xlab=.13, width.y.axis=.44, width.ylab=.2,
        key=plots[[1,1]]$legend$top$args$key, ## lattice key is not always in the same place
        height.key=.2, width.key=5.5,   ## inches ## default key width is the same as panel width
        key.includegraphics=list(hspace.left="2in"), ## move key toward center
        label.x.axis="\\LARGE$\\theta$", label.y.axis="\\LARGE$T_j$", ## latex labels for axes
        y.axis.includegraphics=list(hspace.left="-.2in"), ## reduce space between ylab and y.axis
        rowlabel="\\Huge$\\alpha$\\strut",
        cgroup=c("", "\\Huge$\\beta$"), n.cgroup=c(1,5),
        caption=
          "\\Huge The LP score functions $T_j = \\mbox{Leg}_j(G_{\\alpha,\\beta}(\\theta))$\\dots"),
  width=8.5, height=8)       ## need more width than default


### msWord
w32 <- (version$os == "mingw32")


## with column groups and Greek group headers
## R labels for axes
dimnames(plots) <- dimnames(LegendrePolyMatrices)
LG.docx <- msWord(plots,
                   collapse=function(x)    ## keep the borders on each panel
                     layoutCollapse(x, axis.line=list()),
                  title="LG",
                  height.panel=1.12,
                  width.panel=1.312,
                  height.x.axis=.34,
                  width.y.axis=.5,
                  height.xlab=.18,
                  width.ylab=.3,
                  height.key=.3,
                  width.key=5.5,
                  width.rowname=.5,
                  landscape=TRUE,
                  xlab=TRUE,  ## R xlab and ylab
                  ylab=TRUE,
                  key=plots15$legend$top$args$key,
                  key.par.properties=list(padding.left=130), ## pts from left margin
                  ##                                            72 pts/inch
                  graph.header=if (w32) "beta" else "\u03b2", ## beta,
                  rowlabel=if (w32) "alpha" else "\u03b1", ## alpha
                  FlexTableWidths=c(0.450, 0.300, 0.500, 1.4, 1.4, 1.4, 1.4, 1.4))
LG.docx
## from a run without the FlexTableWidths argument
## in which the right-hand part of the panels was clipped
## > attr(LG.docx, "FlexTableWidths")
## [1] 0.500 0.300 0.500 1.312 1.312 1.312 1.312 1.312
## based on this, we reran it with the numbers shown above.

## with column groups and Greek group headers and labels for axes
LG2.docx <- msWord(plots,
                   collapse=function(x)    ## keep the borders on each panel
                     layoutCollapse(x, axis.line=list()),
                  title="LG2",
                  height.panel=1.12,
                  width.panel=1.312,
                  height.x.axis=.34,
                  width.y.axis=.5,
                  height.xlab=.18,
                  width.ylab=.3,
                  height.key=.3,
                  width.key=5.5,
                  width.rowname=.5,
                  landscape=TRUE,
                  label.x.axis=if (w32) "theta" else "\u03b8", ## theta ## MS Word labels for axes
                  label.y.axis="Tj",
                  key=plots15$legend$top$args$key,
                  key.par.properties=list(padding.left=130), ## pts from left margin
                  ##                                            72 pts/inch
                  graph.header=if (w32) "beta" else "\u03b2", ## beta,
                  rowlabel=if (w32) "alpha" else "\u03b1", ## alpha
                  FlexTableWidths=c(0.450, 0.500, 1.4, 1.4, 1.4, 1.4, 1.4))
LG2.docx
## > attr(LG2.docx, "FlexTableWidths")
## [1] 0.500 0.500 1.312 1.312 1.312 1.312 1.312
## ylab=FALSE (by default), so we have one less value here.
