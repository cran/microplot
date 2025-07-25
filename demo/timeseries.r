library(microplot)
## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()


co2df <- data.frame(co2,
                    month=rep(
                      factor(month.name, levels=month.name),
                      length=length(co2)),
                    year=rep(factor(1959:1997), each=12, length=length(co2)))

co2xy <-
  lattice::xyplot(co2 ~ month | year, data=co2df, layout=c(8,5),
                  pch=c("J","F","M","A","M","J","J","A","S","O","N","D"),
                  col=HH::likertColor(12),
                  type="b", col.line="gray70",
                  scales=list(x=list(at=c(1,7), labels=c("J","Jl"), cex=.7),
                              y=list(at=c(320, 360), cex=.7)),
                  between=list(y=.5),
                  xlab="", ylab="")
co2xy


## 0.  Single column using latex.trellis.

dvi(height=11, width=6,
    latex(co2xy, height.panel=.2,
      caption="A column can be made to fit on a page, but is too tiny to see the pattern over time.")
)

## 1. Individual years per panel, using latex
##    Place 39 years into 5x8 matrix with an empty cell.
dir.verify("co2")
mm <- microplot(co2xy, figPrefix="co2/co2xy",
                height.panel=1, width.panel=.4, height.x.axis=.32, width.y.axis=.42,
                collapse=function(x)
                  layoutCollapse(x, axis.line=list(col="gray50"),
                                 layout.widths=layoutWidthsCollapse(axis.right=.2)))

mm2 <- matrix(c(rev(mm), ""), 5, 8, byrow=TRUE)[5:1,]
dimnames(mm2) <- list(c(1991, 1983, 1975, 1967, 1959), paste0("+", c(0:7)))
attributes(mm2)[c("axis.names", "class")] <- attributes(mm)[c("axis.names", "class")]
mm2

## 1.a Axes too far away from panels.
latex(mm2, rowlabel="Year",
      caption="Five rows of eight individual years.  Continuity between years in a row is lost.")


## 1.b Control position of axes.
ii <- as.includegraphics(mm2,
                         y.axis=list(viewport="0 0 30 72", trim="5 0 0 0",
                                     width=paste0(.42*25/30,"in")),
                         hspace.left="-1.1em", hspace.right="-.7em")
latex(ii, rowlabel="Year",
      caption="Five rows of eight years.  $y$-axis narrower.  Columns pushed together.")





## 2. Eight year sequences per panel
co2df8 <- co2df
co2df8$yr8 <- rep(factor(rev(c(1991, 1983, 1975, 1967, 1959))), c(8,8,8,8,7)*12)
co2df8$m8x12 <- rep(1:96, length=468)
co2df8[11:14,]
co2df8[383:386,]
tail(co2df8)

library(lattice)
co2xy8 <-
  xyplot(co2 ~ m8x12 | "+0~~~~+1~~~~+2~~~~+3~~~~+4~~~~+5~~~~+6~~~~+7" * yr8,
         data=co2df8, layout=c(1,5),
         xlim=c(0, 97),
         pch=c("J","F","M","A","M","J","J","A","S","O","N","D"),
         col=HH::likertColor(12),
         type="b", col.line="gray70",
         scales=
           list(x=list(at=seq(1, 96, 6), labels=rep(c("Jan","Jul"), 8), cex=.7),
                y=list(at=c(320, 360), cex=.7)),
         xlab="", ylab="") +
  latticeExtra::layer(panel.abline(v=seq(12.5, 96, 12), col="gray50"))

update(latticeExtra::useOuterStrips(co2xy8), strip=FALSE, xlab=paste0("+", c(0:7)))

## now repeat in latex
latex(co2xy8, height.panel=1, width.panel=3, height.x.axis=.32, width.y.axis=.45,
      collapse=function(x) layoutCollapse(x, axis.line=list(col="gray50")),
      caption="Set as five rows of eight-year sequences. Controlled axis positions.",
      rowlabel="year",
      y.axis=list(viewport="0 0 32 72", trim="6 0 0 0",
                  width=paste0(.45*26/32,"in")))

detach("package:lattice") ## can't unload; imported by other packages.
