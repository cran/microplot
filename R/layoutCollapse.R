layoutHeightsCollapse <- function(...) {
  x.settings <- lattice::trellis.par.get()$layout.heights
  x.settings[] <- 0
  x.settings$panel=1
  inputs <- list(...)
  if (length(inputs))
    x.settings[names(inputs)] <- inputs
  x.settings
}

layoutWidthsCollapse <- function(...) {
  y.settings <- lattice::trellis.par.get()$layout.widths
  y.settings[] <- 0
  y.settings$panel=1
  inputs <- list(...)
  if (length(inputs))
    y.settings[names(inputs)] <- inputs
  y.settings
}

if (FALSE) {
## obsolescent, but left here for reference
layout.1.1.Collapse <- function(x) {
  update(x,
         xlab="", ylab="", xlab.top=NULL, ylab.right=NULL,
         main=NULL, sub=NULL,
         strip=FALSE, strip.left=FALSE,
       par.settings=list(
         layout.heights=layoutHeightsCollapse(),
         layout.widths=layoutWidthsCollapse(),
         strip.border=list(col="transparent"),
         axis.line=list(col="transparent")),
       layout=c(1,1)
       )
}
}

layoutCollapse <- function(x,
                           xlab="", ## NULL would give us xlab.default
                           ylab="", ## NULL would give us ylab.default
                           xlab.top=NULL,
                           ylab.right=NULL,
                           main=NULL,
                           sub=NULL,
                           strip=FALSE,
                           strip.left=FALSE,
                           layout.heights=layoutHeightsCollapse(),
                           layout.widths=layoutWidthsCollapse(),
                           strip.border=list(col="transparent"),
                           axis.line=list(col="transparent"),
                           layout=c(1,1),
                           ...) ## any argument to update.trellis
{
  update(x,
         xlab=xlab, ylab=ylab, xlab.top=xlab.top, ylab.right=ylab.right,
         main=sub, sub=sub,
         strip=strip, strip.left=strip.left,
         par.settings=list(
           layout.heights=layout.heights,
           layout.widths=layout.widths,
           strip.border=strip.border,
           axis.line=axis.line),
         layout=layout,
         ...
         )
}

if (FALSE) {
  ## for example
  latex(tt,
        collapse=function(x)
          layoutCollapse(x,
                         layout.heights=list(axis.bottom=.3),
                         layout.widths=list(axis.left=.3),
                         axis.line=list(col="green")))
}
