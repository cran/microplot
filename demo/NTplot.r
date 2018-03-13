## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()


## This demo writes a set of pdf files and then uses the Hmisc::latex
## function to call LaTeX.

## This example uses lattice graphics.
## See ?microplot for simple examples with base graphics and ggplot2.

## I recommend demo("NTplot", package="microplot", ask=FALSE)

## This example is based on HH2 Figures 5.9 and 5.10, pages 144 and 148 of
## the Second Edition of Statistical Analysis and Data Display,
## Richard M. Heiberger and Burt Holland, Springer 2015.
##      http://www.springer.com/us/book/9781493921218
## ?HH::NTplot
## library(HH)

library(lattice)
##
## pdf("NTplots%03d.pdf", onefile=FALSE, height=3.5, width=6) ## inch
pdf("NTplots%03d.pdf", onefile=FALSE, height=2.5, width=3.5) ## inch
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
                              left.padding=0,
                              ylab=0,  ## the values of ylab for normal
                              ## and for t have different widths.
                              ## Therefore set the width of ylab to zero
                              ## and increase the padding on both sides.
                              ylab.axis.padding=0,
                              axis.key.padding=0,
                              right.padding=0),
                            clip=list(panel=FALSE),
                            layout.heights=list(
                              top.padding=0,
                              main.key.padding=0,
                              key.axis.padding=3.5,
                              axis.bottom=0,
                              axis.xlab.padding=3.5,
                              bottom.padding=0)
                            )
                 ),
          tablesOnPlot=FALSE)
dev.off()
##
detach("package:lattice")  ## can't be unloaded; imported by several other packages

graphnames <- matrix(paste0("NTplots", sprintf("%03i", 1:12), ".pdf"), byrow=TRUE,
                     3, 4, dimnames=list(df=c("$\\infty$", "15", "3"), mu1=c(2, 2.5, 3, 3.5)))

tmp <- as.includegraphics(graphnames)
df.mu <- cbind("$\\mu_c$"=c(1.645, 1.753, 2.353), tmp)
class(df.mu) <- class(tmp)
df.mu

NTplot.latex <- latex(df.mu,
                      rowlabel="$\\nu$",
                      rowlabel.just="r",
                      cgroup=c("", "$\\mu_a$"),
                      n.cgroup=c(1, 4))
dvi(NTplot.latex, height=8.1, width=15.8) ## inches; wider paper
