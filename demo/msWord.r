library(microplot)
## MS Word display of 1D and 2D graphs

dd <- data.frame(rr=rep(letters[1:4], each=3*10),
                 cc=rep(LETTERS[5:7], each=10, times=4),
                 x=rnorm(120, s=20+rep(1:12, each=10)),
                 y=rnorm(120, m=100, s=20+rep(1:12, each=10)),
                 g=rep(factor(c("1","2","3","4","5","6","7","8","9","A","B","C")), each=10),
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


## MS Word draws the panels top left to top right then down,
## eventually getting to bottom right.
##
## trellis, by default with as.table=FALSE, stats at bottom left, then
## writes rows left to right, eventually at top right.
## msWord.trellis reproduces the appearance of the displayed trellis plot.

msWord(tt, caption="tt with key",
       rowlabel="Row", width.rowname=.5, graph.header="Column",
       height.panel=1.3, width.panel=1.3,
       height.x.axis=.33, ## inch
       width.y.axis=.57,  ## inch
       key=tt$legend$bottom$args$key, height.key=1, width.key=6)


## with as.table=TRUE
msWord(update(tt, as.table=TRUE), caption="tt, as.table, with key",
       rowlabel="Row", width.rowname=.5, graph.header="Column",
       height.panel=1.3, width.panel=1.3,
       height.x.axis=.33, ## inch
       width.y.axis=.57,  ## inch
       key=tt$legend$bottom$args$key, height.key=1, width.key=6)


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

msWord(vv, height.panel=.62,
       width.rowname=.6,
       height.x.axis=.33, ## inch
       width.y.axis=.65,  ## inch
       rowlabel="group",
       vectorgraph.colname="Graph Panels")
