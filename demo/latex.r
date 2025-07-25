library(microplot)
## Hmisc options for pdflatex
## graphics files are .pdf

## This file is close to tutorial in that it displays many pdf files
## each illustating different values of the control arguments.

## These are the LaTeX options I use
latexSetOptions()

dd <- data.frame(rr=rep(letters[1:4], each=3*10),
                 cc=rep(LETTERS[5:7], each=10, times=4),
                 x=rnorm(120, s=20+rep(1:12, each=10)),
                 y=rnorm(120, m=100, s=20+rep(1:12, each=10)),
                 g=rep(c("1","2","3","4","5","6","7","8","9","A","B","C"), each=10),
                 rra=rep(letters[8:9], each=5))

tt <- lattice::xyplot(y ~ x | cc * rr, data=dd,
                      group=g, pch=levels(dd$g), col=HH::likertColor(12), cex=2,
                      xlim=c(-130, 130),
                      ylim=c( -18, 218),
                      scales=list(cex=.6),
                      key=list(text=list(levels(dd$g)),
                               lines=list(col=HH::likertColor(12), lwd=4, size=3),
                               columns=6, space="bottom"))
tt

latticeExtra::useOuterStrips(tt)


## latex draws the panels top left to top right then down,
## eventually getting to bottom right.
##
## trellis, by default with as.table=FALSE, stats at bottom left, then
## writes rows left to right, eventually at top right.
## latex.trellis reproduces the appearance of the displayed trellis plot.

## test latex.trellis
## eight orientations: start at each corner, move by rows and by columns
## 1.   starting at lower left, by rows
update(tt, main="tt")

## 1.a no spacing control
latex(tt, caption="tt")

## 1.b height and width of axes, key,
##     these numbers come from trial and error for this graph
latex(tt, caption="tt with key",
      rowlabel="row",
      height.x.axis=.34, ## inch
      width.y.axis=.43,  ## inch
      key=tt$legend$bottom$args$key, width.key=6,
      key.includegraphics=list(hspace.left=".35in", scale=.8))



## 1.c trim some space from the left side of the y.axis,
##     and adjust width to compensate;
latex(tt, caption="tt",
      rowlabel="row",
      height.x.axis=.34, ## inch
      width.y.axis=.43,  ## inch -------------------------------|
      y.axis.includegraphics= ##                                |
        list(viewport="0 0 30 72", trim="6 0 0 0", width=paste0(.43*24/30,"in")))

## 2.  as.table, starting at upper left, by rows
update(tt, as.table=TRUE, main="tt, as.table=TRUE")
latex(update(tt, as.table=TRUE), caption="tt, as.table=TRUE",
      rowlabel="row",
      height.x.axis=.34, ## inch
      width.y.axis=.43,  ## inch
      y.axis.includegraphics=
        list(viewport="0 0 30 72", trim="6 0 0 0", width=paste0(.43*24/30,"in")))

## 3. transpose, start at lower left and go up by columns
update(t(tt), main="t(tt)")
latex(t(tt), caption="t(tt)",
      rowlabel="row",
      height.x.axis=.34, ## inch
      width.y.axis=.43,  ## inch
      y.axis.includegraphics=
        list(viewport="0 0 30 72", trim="6 0 0 0", width=paste0(.43*24/30,"in")))

## 4. transpose and as.table=TRUE, start at upper left and go down by columnsa
update(t(tt), as.table=TRUE, main="t(tt), as.table=TRUE")
latex(update(t(tt), as.table=TRUE), caption="t(tt), as.table=TRUE",
      rowlabel="row",
      height.x.axis=.34, ## inch
      width.y.axis=.43,  ## inch
      y.axis.includegraphics=
        list(viewport="0 0 30 72", trim="6 0 0 0", width=paste0(.43*24/30,"in")))


## lattice allows four options of orientation, all starting on the
## left.  To get it to start on the right, restate the lattice call with reversed columns

## 5. start at lower right, Right to Left by rows
dd$ccRL <- factor(dd$cc, levels=rev(levels(dd$cc)))
ttRL <- lattice::xyplot(y ~ x | ccRL * rr, data=dd,
                        group=g, pch=levels(dd$g), col=HH::likertColor(12), cex=2,
                        xlim=c(-130, 130),
                        ylim=c( -18, 218),
                        scales=list(cex=.6),
                        key=list(text=list(levels(dd$g)),
                                 lines=list(col=HH::likertColor(12), lwd=4, size=3),
                                 columns=6, space="bottom"))
ttRL

latex(ttRL, caption="ttRL",
      rowlabel="row",
      height.x.axis=.34, ## inch
      width.y.axis=.43,  ## inch
      y.axis.includegraphics=
        list(viewport="0 0 30 72", trim="6 0 0 0", width=paste0(.43*24/30,"in")))

## 6, 7, 8 left as an exercise


## vector
vv <- lattice::xyplot(y ~ x | interaction(cc, rr), data=dd,
                      group=g, pch=levels(dd$g),
                      col=HH::likertColor(12), cex=2, between=list(y=1),
                      xlim=c(-100, 100),
                      ylim=c(   0, 200),
                      xlab="", ylab="",
                      scales=list(cex=.7),
                      as.table=TRUE)
vv

latex(vv, height.panel=.41,
      height.x.axis=.34, ## inch
      width.y.axis=.44,  ## inch
      rowlabel="group",
      vectorgraph.colname="Graph Panels")
