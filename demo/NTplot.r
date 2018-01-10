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


## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses lattice graphics.
## See ?microplot for simple examples with base graphics and ggplot2.

## I recommend demo("NTplot", package="microplot", ask=FALSE)

## This example is based on HH2 Figures 5.9 and 5.10, pages 144 and 148 of
## the Second Edition of Statistical Analysis and Data Display,
## Richard M. Heiberger and Burt Holland, Springer 2015.
##      http://www.springer.com/us/book/9781493921218

## library(HH)

pdf("NTplots%03d.pdf", onefile=FALSE, height=3.5, width=6) ## inch
for (mu1 in c(2, 2.5, 3, 3.5))
  for (degFreedom in c(Inf, 15, 3))
    print(HH::NTplot(cex.prob=1,
                     yhalf.multiplier=1.2,
                     main=NA, ylab=NULL, float=FALSE,
                     xlab=NULL,
                     mean1=mu1,
                     df=degFreedom,
                     distribution.name=if (degFreedom==Inf) "z" else "t",
                     xlim=c(-4, 7), ylim=c(0, .45),
                     par.settings=
                       list(layout.widths=list(
                              left.padding=5,
                              ylab=0,  ## the values of ylab for normal
                              ## and for t have different widths.
                              ## Therefore set the width of ylab to zero
                              ## and increase the padding on both sides.
                              ylab.axis.padding=5,
                              axis.key.padding=0,
                              right.padding=0),
                            clip=list(panel=FALSE),
                            layout.heights=list(
                              top.padding=1.5,
                              main.key.padding=4,
                              key.axis.padding=4.5,
                              axis.bottom=0,
                              axis.xlab.padding=1,
                              bottom.padding=3)
                            )
                 ),
          tablesOnPlot=FALSE)
dev.off()


graphnames <- paste0("NTplots", sprintf("%03i", 1:12), ".pdf")

df.mu1 <-
matrix(
  as.includegraphics(graphnames, height="4em", raise="-1em"),
  3, 4,
  dimnames=list(df=c("$\\infty$", "15", "3"), mu1=c(2, 2.5, 3, 3.5))
)

df.mu1 <- cbind("$\\mu_c$"=c(1.645, 1.753, 2.353), df.mu1)

NTplot.latex <- Hmisc::latex(df.mu1,
                             rowlabel="$\\nu$",
                             rowlabel.just="r",
                             cgroup=c("", "$\\mu_a$"),
                             n.cgroup=c(1, 4))
NTplot.latex$style <- "graphicx"
NTplot.latex  ## this line requires latex in the PATH


## landscape
df.mu2 <-
matrix(
  as.includegraphics(graphnames, height="12em", raise="-4em", hspace.right="0in",
                     viewport="0 0 432 252",
                     trim="64.8 0 0 0",
                     width=paste0(.85*(6/3.5)*12,"em")),
  3, 4,
  dimnames=list(df=c("$\\infty$", "15", "3"), mu1=c(2, 2.5, 3, 3.5))
)

df.mu2 <- cbind("$\\mu_c$"=c(1.645, 1.753, 2.353), df.mu2)

NTplot2.latex <- Hmisc::latex(df.mu2,
                             rowlabel="$\\nu$",
                             rowlabel.just="r",
                             cgroup=c("", "$\\mu_a$"),
                             n.cgroup=c(1, 4))
NTplot2.latex$style <- "graphicx"
Hmisc::dvi(NTplot2.latex, height=7, width=12) ## wider paper  ## this line requires latex in the PATH
