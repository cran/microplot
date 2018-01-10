## Hmisc options for pdflatex
## graphics files are .pdf

## These are the LaTeX options I use
options(latexcmd="pdflatex") ## Macintosh, Windows, linux
options(dviExtension="pdf")  ## Macintosh, Windows, linux
if (nchar(Sys.which("open"))) {
  options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
} else {
  options(xdvicmd="xdg-open")  ## ubuntu linux
}
latexCheckOptions()

dd <- data.frame(rr=rep(letters[1:4], each=3*10),
                 cc=rep(LETTERS[5:7], each=10, times=4),
                 x=rnorm(120, s=20+rep(1:12, each=10)),
                 y=rnorm(120, m=100, s=20+rep(1:12, each=10)),
                 g=rep(c("1","2","3","4","5","6","7","8","9","A","B","C"), each=10),
                 rra=rep(letters[8:9], each=5))

tt <- lattice::xyplot(y ~ x | cc * rr, data=dd,
                      group=g, pch=levels(dd$g), col=HH::likertColor(12), cex=2,
                      xlim=c(-100, 100),
                      ylim=c(   0, 200))
                      ## xlab=letters[8:11],  ## not included in latex()
                      ## ylab=LETTERS[12:14],
                      ## xlab.top=letters[15:18],
                      ## ylab.right=LETTERS[19:21])

tt



if (FALSE) {
latticeExtra::useOuterStrips(tt)

update(tt, scales=list(x=list(relation="free"), y=list(relation="free")))

update(tt, scales=list(x=list(relation="free")))

update(tt, scales=list(y=list(relation="free")))

update(tt, xlab="xlab", ylab="ylab", xlab.top="xlab.top", ylab.right="ylab.right")
}

## latex draws the panels top left to top right then down,
## eventually getting to bottom right.
##
## trellis, by default with as.table=FALSE, stats at bottom left, then
## writes rows left to right, eventually at top right.
## latex.trellis reproduces the appearance of the displayed trellis plot.


## figure out how best to handle pages, as in layout=c(4, 3, 2)
## deeper conditionings as useOuterStripsT2L1
## work out xlab etc
## arbitrary nestings of rows and columns of trellis object
## between=list(c(), c())

## test latex.trellis
## four orientations
## 1
update(tt, main="tt")
## no axis control
Hmisc::latex(tt, caption="tt", raise="-7ex")
## axis control
Hmisc::latex(tt, caption="tt", raise="-7ex",
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="25 0 15 0", width=paste0(32/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"))

## return R matrix for further processing before manually calling latex()
tt.r <-
Hmisc::latex(tt, caption="tt", raise="-7ex",
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="25 0 15 0", width=paste0(32/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"),
             return.value="R")
tt.r

## 2
update(tt, as.table=TRUE, main="tt, as.table=TRUE")
Hmisc::latex(update(tt, as.table=TRUE), caption="tt, as.table=TRUE", raise="-7ex",
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="25 0 15 0", width=paste0(32/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"))

## 3
update(t(tt), main="t(tt)")
Hmisc::latex(t(tt), caption="t(tt)", raise="-7ex",
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="25 0 15 0", width=paste0(32/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"))

## 4
update(t(tt), as.table=TRUE, main="t(tt), as.table=TRUE")
Hmisc::latex(update(t(tt), as.table=TRUE), caption="t(tt), as.table=TRUE", raise="-7ex",
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="25 0 15 0", width=paste0(32/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"))


## lattice allows four options of orientation, all starting on the
## left.  To get it to start on the right, restate the lattice call with reversed columns

## 5, Right to Left
dd$ccRL <- factor(dd$cc, levels=rev(levels(dd$cc)))
ttRL <- lattice::xyplot(y ~ x | ccRL * rr, data=dd,
                      group=g, pch=levels(dd$g), col=HH::likertColor(12), cex=2,
                      xlim=c(-100, 100),
                      ylim=c(   0, 200))

Hmisc::latex(ttRL, caption="ttRL", raise="-7ex",
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="25 0 15 0", width=paste0(32/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"))

## 6, 7, 8 and vector left as an exercise



## 1, more height
Hmisc::latex(t(tt), height.as="1.5in", raise="-7ex",
             rowlabel="row")

Hmisc::latex(t(tt), height.as="1.5in", raise="-7ex",  ## scaling by 1.5 needed in these three places
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="26 0 15 0", width=paste0(1.5*31/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(1.5*24/72,"in"), raise="-3ex"))


## 1, collapse
Hmisc::latex(tt, raise="-7ex",
             collapse=function(x) layoutCollapse(x, axis.line=list(col="green")),
             rowlabel="row",
             y.axis=list(viewport="0 0 72 72", trim="30 0 15 0", width=paste0(27/72,"in"), hspace.right="-1em"),
             x.axis=list(viewport="0 0 72 72", trim="0 25 0 23", height=paste0(24/72,"in"), raise="-3ex"))

## 1, rowseparator
Hmisc::latex(tt, raise="-7ex", rowlabel="row",
             rowseparator=TRUE)

## 1, rowseparator and collapse
Hmisc::latex(tt, raise="-7ex", rowlabel="row",
             collapse=function(x) layoutCollapse(x, axis.line=list(col="green")),
             rowseparator=TRUE)


## vector
vv <- lattice::xyplot(y ~ x | interaction(cc, rr), data=dd,
                      group=g, pch=levels(dd$g), col=HH::likertColor(12), cex=2)
vv

Hmisc::latex(vv)

Hmisc::latex(vv, height.as=".35in", raise="-2ex")

Hmisc::latex(vv, height.as=".35in", raise="-2ex", rowseparator=TRUE)

Hmisc::latex(update(vv, as.table=TRUE), height.as=".35in", raise="-2ex", rowseparator=TRUE)

Hmisc::latex(t(as.matrix(vv)), height.as=".35in", raise="-2ex", rowseparator=TRUE)


## 3D not currently available
ee <- lattice::xyplot(y ~ x | cc * rr * rra, data=dd,
                      group=g, pch=levels(dd$g), col=HH::likertColor(12), cex=2)
ee
try(Hmisc::latex(ee))
