library(HH) ## loads lattice, latticeExtra
library(microplot) ## microplot_1.0-24, microplot_1.0-16 is on CRAN

## Hmisc options for pdflatex
## graphics files are .pdf
options(latexcmd='pdflatex')
options(dviExtension='pdf')
if (nchar(Sys.which("open"))) {
  options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
} else {
  options(xdvicmd="xdg-open")  ## ubuntu linux
}
latexCheckOptions()





## Legendre Polynomials
library(orthopolynom)
LP.score <- function(alpha, beta, m = 4, B = 100) {
  x <- seq(1/B, 1 - 1/B, length = B)
  u <- stats::pbeta(x, alpha, beta)
  poly <-  slegendre.polynomials(m, normalized=TRUE)
  data.frame(x=x, T=sapply(poly[-1], predict, u))
}

alphas <- c(.25, .5, 1)
betas <- c(.25, .5, 1, 2, 10)


## generate matrices
matrices <- matrix(list(), nrow=length(alphas), ncol=length(betas),
                   dimnames=list(alphas, betas))
for (alpha in seq(along=alphas))
  for (beta in seq(along=betas))
    matrices[[alpha, beta]] <- LP.score(alphas[alpha], betas[beta])

detach("package:orthopolynom", unload=TRUE)
detach("package:polynom", unload=TRUE)


## set of plots
plots <-
  lapply(matrices, function(mab)
    xyplot(T.1 + T.2 + T.3 + T.4 ~ x, data=mab,
           col=c("black","red","blue","green"), lwd=2, type="l",
           par.settings=list(axis.line=list(col="gray50", lwd=1)),
           xlim=c(-0.04, 1.04), ylim=c(-2.5, 3),
           xlab=expression(theta), ylab=list(expression(T[j]), rot=0),
           key=list(columns=4,
                    text=list(expression("Leg"[1],"Leg"[2],"Leg"[3],"Leg"[4])),
                    lines=list(lwd=3),
                    col=c("black","red","blue","green"))))
dim(plots) <- dim(matrices)
dimnames(plots) <- dimnames(matrices)

plots[[2, 2]]$par.settings$axis.line <- list(col="black", lwd=3) ## jp.LP <- LP.score.plot(.5,.5)
plots[[3, 3]]$par.settings$axis.line <- list(col="black", lwd=3) ## a1b1.LP <- LP.score.plot(1,1)
plots[[2, 5]]$par.settings$axis.line <- list(col="black", lwd=3) ## LP.score.plot(.5,10)
## print(plots)

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
lattice:::lattice.setStatus("print.more"=FALSE)

## place list of all 15 trellis objects into 15 panels of a single trellis object
## this uses latticeExtra:::c.trellis and HH:::rbind.trellis
plots15 <-
  rbind(do.call(c, plots[1,]),
        do.call(c, plots[2,]),
        do.call(c, plots[3,]))
dimnames(plots15) <- rev(dimnames(plots))  ## note reversal!
plots15 <- useOuterStrips(update(plots15, as.table=TRUE))  ## start at top
## lattice is row major; matrices are column major
## display on interactive device.  Note dark outlining of several panels is lost.
plots15


## simple display in LaTeX of single trellis object with an array of panels.
Hmisc::latex(plots15) ## default width is too narrow, legend (key) missing

## minimal arguments to see LaTeX display of single trellis object with an array of panels.
Hmisc::dvi(                                    ## need more width than default
 Hmisc::latex(plots15,
              key=plots15$legend$top$args$key, ## lattice key is not always in the same place
              width.key=6), ## inches          ## default key width is the same as panel width
 width=8.5)                                    ## need more display width than default


## minimal arguments to see an array of individualized, multiple trellis objects in LaTeX
Hmisc::dvi(                                    ## need more width than default
 Hmisc::latex(plots[[1,1]], plots,
              collapse=function(x)
                layoutCollapse(x, axis.line=list()),
              key=plots[[1,1]]$legend$top$args$key, ## lattice key is not always in the same place
              width.key=6), ## inches          ## default key width is the same as panel width
 width=8.5)                                    ## need more width than default



## Annotated display in LaTeX, array of individualized, multiple trellis objects in LaTeX
##
## This version allows independent control of update items using layoutCollapse.
## plots is a matrix object with column-major dimensioning.
## plots[[1,1]] is a atrellis object
dimnames(plots) <- lapply(dimnames(matrices), function(x) paste("\\Huge\\strut", x))
plots.latex <-
  Hmisc::latex(plots[[1]], obji=plots,
               height=2, width=2.5,
               collapse=function(x)
                 layoutCollapse(x, axis.line=list()), ## retain individual col and lwd for panels
               raise="-8em", hspace.right="-1.2em",
               height.x.axis=1,
               x.axis.includegraphics=list(raise="-4em", viewport="0 0 216 72", trim="0 10 0 15"),
               width.y.axis=1,
               y.axis.includegraphics=list(viewport="0 0 72 144", trim="15 0 12 0", hspace.right="-1ex"),
               key=plots15$legend$top$args$key,
               rowlabel="\\Huge$\\alpha$\\strut",
               cgroup=c("", "\\Huge$\\beta$"), n.cgroup=c(1,5),
               caption=
                 "\\Huge The LP score functions $T_j = \\mbox{Leg}_j(G_{\\alpha,\\beta}(\\theta))$\\dots",
               height.key=.25, width.key=6,
               key.includegraphics=list(hspace.left="5.4in", raise=NULL))
Hmisc::dvi(plots.latex, height=8.8, width=15)
