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

library(lattice)

co2df <- data.frame(co2,
                    month=rep(
                      factor(month.name, levels=month.name),
                      length=length(co2)),
                    year=rep(factor(1959:1997), each=12, length=length(co2)))

co2xy <-
  xyplot(co2 ~ month | year, data=co2df, layout=c(8,5),
         pch=c("J","F","M","A","M","J","J","A","S","O","N","D"),
         col=HH::likertColor(12),
         type="b", col.line="gray70",
         scales=list(x=list(at=c(1,7), labels=c("J","Jl"), cex=.7),
                     y=list(at=c(320, 360), cex=.7)))

update(co2xy, between=list(y=.5))

## 0.  Single column using latex.trellis.  Most of graph is not on page.
## Hmisc::latex(co2xy, raise="-7ex")
##
## Hmisc::latex(co2xy, raise="-7ex", x.axis=FALSE, y.axis=FALSE)



## 1. Individual years per panel, using latex.default.
##    Place 39 years into 5x8 matrix with an empty cell.
mm <- microplot(co2xy, height=1, width=.4,
                collapse=function(x) layoutCollapse(x, axis.line=list(col="gray50"),
                                                    layout.widths=layoutWidthsCollapse(axis.right=.2)))

ii <- matrix(c(as.includegraphics(rev(mm), height="1in", raise="-7ex"), ""),
             byrow=TRUE, nrow=5, ncol=8)[5:1,]
dimnames(ii) <- list(c(1991, 1983, 1975, 1967, 1959), paste0("+", c(0:7)))
attr(ii, "axis.names") <- as.includegraphics(attr(mm, "axis.names"), height="1in", raise="-7ex")

## 1.z simplest, without axes
## ll <- Hmisc::latex(ii)
## ll$style <- "graphicx"
## ll

## 1.a  Put x- and y-axes in default positions.
lla <- Hmisc::latex(
                rbind(
                  cbind(attr(ii,"axis.names")["y"], ii),
                  c("", rep(attr(ii,"axis.names")["x"], ncol(ii)))
                ),
                caption="co2: Set as five rows of eight individual years.  Continuity between years in a row is lost.",
                rowlabel="year")
lla$style<- "graphicx"
lla


## 1.b Control position of axes.
iib <- ii
attr(iib, "axis.names")["x"] <- as.includegraphics(attr(mm, "axis.names")["x"],
                                                   viewport="0 0 28 72", trim="0 25 0 25",
                                                   height=paste0((72-(25+25))/72, "in"),
                                                   raise="-3ex")
attr(iib, "axis.names")["y"] <- as.includegraphics(attr(mm, "axis.names")["y"], height="1in", raise="-7ex", hspace.right="-1.2em")
##
llb <- Hmisc::latex(
                rbind(
                  cbind(attr(iib,"axis.names")["y"], iib),
                  c("", rep(attr(iib,"axis.names")["x"], ncol(iib)))
                ),
                caption="co2: Set as five rows of eight individual years.  Continuity between years in a row is lost.",
                rowlabel="year")
llb$style<- "graphicx"
llb





## 2. Eight year sequences per panel, using latex.trellis
co2df8 <- co2df
co2df8$yr8 <- rep(factor(rev(c(1991, 1983, 1975, 1967, 1959))), c(8,8,8,8,7)*12)
co2df8$m8x12 <- rep(1:96, length=468)

co2xy8 <-
  xyplot(co2 ~ m8x12 | "+0~~~~~+1~~~~~+2~~~~~+3~~~~~+4~~~~~+5~~~~~+6~~~~~+7" * yr8,
         data=co2df8, layout=c(1,5),
         xlim=c(0, 97),
         pch=c("J","F","M","A","M","J","J","A","S","O","N","D"),
         col=HH::likertColor(12),
         type="b", col.line="gray70",
         scales=list(x=list(at=seq(1, 96, 6), labels=rep(c("Jan","Jul"), 8), cex=.7),
                     y=list(at=c(320, 360), cex=.7))) +
  latticeExtra::layer(panel.abline(v=seq(12.5, 96, 12), col="gray50"))

update(co2xy8, strip=FALSE, strip.left=TRUE, xlab=paste0("+", c(0:7)))

## 2.a Default position of axes,
Hmisc::latex(co2xy8, width=3, raise="-7ex",
             collapse=function(x) layoutCollapse(x, axis.line=list(col="gray50")),
             caption="co2: Set as five rows of eight-year sequences.",
             rowlabel="year", x.axis=TRUE,
             y.axis=TRUE)

## 2.b Control position of axes.
Hmisc::latex(co2xy8, width=3, raise="-7ex",
             collapse=function(x) layoutCollapse(x, axis.line=list(col="gray50")),
             caption="co2: Set as five rows of eight-year sequences.",
             rowlabel="year",
             x.axis=list(viewport="0 0 216 72",
                         trim="0 25 0 25",
                         height=paste0((72-(25+25))/72, "in"),
                         raise="-3ex"),
             y.axis=list(viewport="0 0 216 72",
                         trim="97 0 93 0",
                         hspace.right="-1.2em"))
