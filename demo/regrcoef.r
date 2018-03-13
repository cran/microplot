## This example is based on an R-help query initiated 2018 February 8
## by Greg Holly and responses, including the regrcoef dataset, by
## Petr Pikal.  This example uses microplot to embed a set of barchart
## panels (constructed twice, once each by lattice and ggplot2) into a
## LaTeX table, allowing full scientific typography for the labels in
## the table.

library(microplot)
latexSetOptions("pdflatex")

regrcoef <- data.frame(coef=c(-0.19, 0.45, -0.09, -0.16, 0.4, -0.75, 0.14, -0.01,
                              -0.06, -8.67, 1.42, 2.21, 0.13, -0.46, 0.06, 0.06),
                       x=rep(c("x1","x2","x3","x4"), 4),
                       y=rep(c("y1","y2","y3","y4"), each=4))

ynames <- c("palmitoyl-arachidonoyl-glycerol (16:0/20:4) [1]*",
            "longname$_3$-$x^2$", "Fancier Name", "another name")
levels(regrcoef$y) <- ynames

tt <- formatDF(with(regrcoef,
                     matrix(coef, byrow=TRUE,
                            4, 4, dimnames=list(levels(y), levels(x)))))


dimnames(tt) <- list(ynames, c("\\large$\\strut\\beta_{x_1}$",
                               "\\large$\\strut\\beta_{x_2}$",
                               "\\large$\\strut\\beta_{x_3}$",
                               "\\large$\\strut\\beta_{x_4}$"))

color <- c("hotpink","lightblue")


## lattice
coefbars <-
  lattice::barchart(coef ~ x | y, groups=(coef >= 0), stack=TRUE,
                    layout=c(1,4),
                    col=color, border=color,
                    data=regrcoef,
                    scales=list(y=list(cex=.3, tck=.3, at=c(-8, -4, 0)),
                                x=list(cex=.7)),
                    as.table=TRUE, horizontal=FALSE, origin=0,
                    par.settings=list(axis.line=list(lwd=.5)))
coefbars

dvi(width=7.5, height=2.3,
    latex(coefbars, dataobject=tt, data.first=FALSE,
          label.x.axis="$X$ vars", label.y.axis="$\\beta$",
          height.panel=.3,
          height.x.axis=.27, width.y.axis=.27, ## inch
          rowlabel="chemical", vectorgraph.colname=" ",
          axis.line=list(col="black", lwd=.1))
    )


## ggplot

library(ggplot2)
coefcolor <- color[1 + (regrcoef$coef > 0)]
coefbarsgg <-
  ggplot(regrcoef, aes(x, coef)) +
  geom_col(position="dodge", fill=coefcolor, color=coefcolor) +
  facet_wrap(~ y, ncol=1) +
  theme(axis.text.y=element_text(size=5)) +
  scale_y_continuous(breaks = c(0, -4, -8))
coefbarsgg

dvi(width=7.5, height=2.3,
    latex(coefbarsgg, dataobject=tt, data.first=FALSE,
          label.x.axis="$X$ vars", label.y.axis="$\\beta$",
          height.panel=.3,
          height.x.axis=.27, width.y.axis=.27, ## inch
          rowlabel="chemical", vectorgraph.colname=" ",
          axis.line=list(col="black", lwd=.1))
    )

detach("package:ggplot2") ## can't unload


## lattice and MS Word

ttw <- format(with(regrcoef,
                   matrix(coef, byrow=TRUE,
                          4, 4, dimnames=list(levels(y), levels(x)))),
              digits=3)

msWord(coefbars, dataobject=ttw, data.first=FALSE,
       label.x.axis="X vars", label.y.axis="beta",
       height.panel=.3,
       height.x.axis=.27, width.y.axis=.8, ## inch
       rowlabel="chemical", vectorgraph.colname=" ",
       width.rowname=3.5,
       width.dataobject=.5,
       axis.line=list(col="black", lwd=.1))

