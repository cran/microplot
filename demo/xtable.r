## xtable example

## These are the settings for my machines
## Set options for Hmisc::latex
latexSetOptions()


tmp <- matrix(rnorm(20), 2, 5, byrow=TRUE,
              dimnames=list(c("A", "B"), paste0("X", 1:5)))

tmp.df <- data.frame(y=as.vector(t(tmp)),
                     group=factor(rep(row.names(tmp), each=5)))

tmp.lattice <- lattice::bwplot(group ~ y | group, data=tmp.df, layout=c(1,2),
                               as.table=TRUE,
                               scales=list(y=list(relation="free", at=NULL)))
tmp.lattice
tmpl.graphnames <- paste0("tmpl", sprintf("%03i", 1:2), ".pdf")

for (i in seq(along=tmpl.graphnames)) {
  pdf(tmpl.graphnames[i], height=.4, width=2)
  print(update(layoutCollapse(tmp.lattice[i]), layout=c(1,1)))
  dev.off()
}


tmpl.display <-
  data.frame(formatDF(round(tmp, 2), dec=2),
             graphs=as.includegraphics(tmpl.graphnames))

print(xtable::xtable(tmpl.display,
                     caption="xtable of graphnames of lattice column"),
      sanitize.text.function = function(x) {x},
      ## xtable converts "\abc" to "$\backslash$abc"
      ## sanitize restores it back to "\abc".
      file="tmpldisplayxtable.tex")

## Display on screen using Hmisc::dvi
tmpx.latex <- list(file="tmpldisplayxtable.tex", style="graphicx")
class(tmpx.latex) <- "latex"
tmpx.latex  ## this line requires latex in the PATH
