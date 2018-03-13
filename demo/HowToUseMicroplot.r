library(microplot)

## These are the LaTeX options I use
latexSetOptions()
## See ?Hmisc::latex for discussion of these options.

## This file illustrates how the microplot functions work.
## See the other examples for more interesting applications.

## 1.
## This example writes a set of pdf files and then uses the Hmisc::latex
## function to display them in LaTeX.

## The graphs are constructed three times, once each with lattice,
## base graphics, and ggplot2.

tmp <- data.frame(y=c(-4, rnorm(20), 4),
                     group=factor(rep(c("A","B"), each=11)))
tmp

## All three examples use the pdf device

## lattice example

tmp.lattice <- lattice::bwplot(~ y | group, data=tmp, layout=c(1,2),
                               as.table=TRUE)
tmp.lattice

latex(tmp.lattice)

latex(tmp.lattice, y.axis=FALSE, height.x.axis=.35)

latex(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35)

latex(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots")

tmpfive <-
  rbind(A=fivenum(tmp[tmp$group=="A", "y"]),
        B=fivenum(tmp[tmp$group=="B", "y"]))
colnames(tmpfive) <- c("min", "Q1", "med", "Q3", "max")
tmpfive

latex(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=formatDF(tmpfive, dec=2))

fancy <- rbind("Full \\LaTeX\\ power with an equation $e^{-x^2}$",
               "$\\frac{dy}{dx}$ is also interesting")
## note double slash for the latex slash

latex(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=fancy)

dvi(width=8, height=3,
latex(tmp.lattice, y.axis=FALSE, height.panel=.7, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=cbind(fancy, formatDF(tmpfive, dec=2)),
      cgroup=c("phrase", "5 number summary", "graph"), n.cgroup=c(1,5,1))
)

dvi(width=8, height=3,
latex(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=cbind(fancy, formatDF(tmpfive, dec=2)),
      cgroup=c("phrase", "5 number summary", "lattice"), n.cgroup=c(1,5,1),
      caption="Change the spacing, and \\LaTeX\\ keeps the alignment for you.")
)


fancier <- rbind("Full \\LaTeX\\ power with an equation $e^{-x^2}$",
                 "\\begin{tabular}{l}$\\frac{dy}{dx}$ \\\\is also interesting\\end{tabular}")
## note double slash for the latex slash

dvi(width=8, height=3,
latex(tmp.lattice, y.axis=FALSE, height.panel=.7, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=cbind(fancier, formatDF(tmpfive, dec=2)),
      cgroup=c("phrase", "5 number summary", "graph"), n.cgroup=c(1,5,1),
      caption="Change the spacing, and \\LaTeX\\ keeps the alignment for you.")
)


## and it works exactly the same with ggplot
library(ggplot2)

tmpf <- cbind(tmp, fake="ff")
tmp.ggplot <- ggplot(tmpf, aes(fake, y)) +
  geom_boxplot() +
  facet_wrap(~ group, ncol=1) +
  coord_flip()
tmp.ggplot

dvi(width=8, height=3,
latex(tmp.ggplot, y.axis=FALSE, height.panel=.4, height.x.axis=.35,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=cbind(fancier, formatDF(tmpfive, dec=2)),
      cgroup=c("phrase", "5 number summary", "ggplot"), n.cgroup=c(1,5,1),
      caption="Change the spacing, and \\LaTeX\\ keeps the alignment for you.")
)


## It is similar with base graphics.  Here we construct the
## microplotMatrix manually because base graphics doesn't have the
## concept of a plot with multiple panels.  The steps shown here are
## captured in the latex.trellis and latex.ggplot methods and can
## therefore be hidden from the user.
##
## For latex we use pdf.

pdf("tmpb01.pdf", height=.5, width=1.5)
par(bty="n", xaxt="n", omd=c(0,1, 0,1), mai=c(0,0,0,0))
boxplot(tmp[tmp$group=="A", "y"], horizontal=TRUE, ylim=range(tmp$y))
dev.off()

pdf("tmpb02.pdf", height=.5, width=1.5)
par(bty="n", xaxt="n", omd=c(0,1, 0,1), mai=c(0,0,0,0))
boxplot(tmp[tmp$group=="B", "y"], horizontal=TRUE, ylim=range(tmp$y))
dev.off()

pdf("tmpb03.pdf", height=.5, width=1.5)
par(bty="n",           omd=c(0,1, 0,1), mai=c(.4,0,0,0))
boxplot(tmp[tmp$group=="A", "y"], horizontal=TRUE, ylim=range(tmp$y), border="transparent")
dev.off()

mm.basepdf <- rbind(A="tmpb01.pdf",
                    B="tmpb02.pdf",
                    "tmpb03.pdf")
class(mm.basepdf) <- c("microplotMatrix", "matrix")

dvi(width=8, height=3,
latex(mm.basepdf,
      rowlabel="Group", vectorgraph.colname="Boxplots",
      dataobject=cbind(fancier, formatDF(tmpfive, dec=2)),
      cgroup=c("phrase", "5 number summary", "base"), n.cgroup=c(1,5,1),
      caption="Change the spacing, and \\LaTeX\\ keeps the alignment for you.")
)




## and similarly with MS Word
## MS Word example.  Uses functions in ReporteRs package.

notSoFancy <- matrix(c("Text lines in Word can be placed",
                       "in the same table structure, and the spacing will be maintained"),
                     2, 1, dimnames=list(NULL,"text"))

tmp.word0 <-
msWord(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
       rowlabel="Group", width.rowname=.8,
       vectorgraph.colname="Boxplots",
       dataobject=cbind(notSoFancy, format(tmpfive, digits=2)),
       graph.header="lattice")
tmp.word0
attr(tmp.word0, "FlexTableWidths")
## [1] 0.8 1.0 1.0 1.0 1.0 1.0 1.0 0.1 1.0  ## Read these Numbers and Change Below
## Close the above window and issue the revised command with explicit FlexTableWidths

tmp.wordlattice2 <-
msWord(tmp.lattice, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
       rowlabel="Group", width.rowname=.8,
       vectorgraph.colname="Boxplots",
       dataobject=cbind(notSoFancy, format(tmpfive, digits=2)),
       graph.header="lattice",
       FlexTableWidths=c(.8, 2.2, .6, .6, .6, .6, .6, .1, 1.1),
       figPrefix="tmp.wordlattice2")
tmp.wordlattice2



tmp.wordggplot <-
msWord(tmp.ggplot, y.axis=FALSE, height.panel=.5, height.x.axis=.35,
       rowlabel="Group", width.rowname=.8,
       vectorgraph.colname="Boxplots",
       dataobject=cbind(notSoFancy, format(tmpfive, digits=2)),
       graph.header="ggplot",
       FlexTableWidths=c(.8, 2.2, .6, .6, .6, .6, .6, .1, 1.1))
tmp.wordggplot


## For Word we use png.  Again, with base graphics we must construct
## the microplotMatrix manually.  With lattice and trellis, the msWord
## methods were able to hide these details from the user.

png("tmpb01.png", height=.5, width=1, res=600, unit="in")
par(bty="n", xaxt="n", omd=c(0,1, 0,1), mai=c(0,0,0,0))
boxplot(tmp[tmp$group=="A", "y"], horizontal=TRUE, ylim=range(tmp$y))
dev.off()

png("tmpb02.png", height=.5, width=1, res=600, unit="in")
par(bty="n", xaxt="n", omd=c(0,1, 0,1), mai=c(0,0,0,0))
boxplot(tmp[tmp$group=="B", "y"], horizontal=TRUE, ylim=range(tmp$y))
dev.off()

png("tmpb03.png", height=.5, width=1, res=600, unit="in")
par(bty="n",           omd=c(0,1, 0,1), mai=c(.4,0,0,0))
boxplot(tmp[tmp$group=="A", "y"], horizontal=TRUE, ylim=range(tmp$y), border="transparent")
dev.off()

mm.basepng <- rbind(A="tmpb01.png",
                    B="tmpb02.png")
colnames(mm.basepng) <- "graphs"
attr(mm.basepng, "axis.names") <- c(x="tmpb03.png", y="")
class(mm.basepng) <- c("microplotMatrix", "matrix")



tmp.wordbase <-
  msWord(mm.basepng, y.axis=FALSE, height.panel=.5, width.panel=1,
       rowlabel="Group", width.rowname=.8,
       vectorgraph.colname="Boxplots",
       dataobject=cbind(notSoFancy, format(tmpfive, digits=2)),
       graph.header="base",
       FlexTableWidths=c(.8, 2.2, .6, .6, .6, .6, .6, .1, 1.1),
       title="tmp.base")
tmp.wordbase
