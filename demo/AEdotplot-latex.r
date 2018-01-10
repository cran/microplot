data(AEdata, package="HH")

AE1 <- HH::AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)
AE2 <- HH::AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)

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

AE1.latex <- Hmisc::latex(AE1)
AE2.latex <- Hmisc::latex(AE2)

Hmisc::dvi(AE1.latex, height=7.6, width=8) ## wider paper
Hmisc::dvi(AE2.latex, height=8.8, width=8) ## wider paper


if (FALSE) {
## Hmisc options for latex
## graphics files are .ps
options(latexcmd='latex')
options(dviExtension='dvi')
if (nchar(Sys.which("open"))) {
  options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
} else {
  options(xdvicmd="xdg-open")  ## ubuntu linux
}
latexCheckOptions()

AE1.latex <- Hmisc::latex(AE1, device="postscript")
AE2.latex <- Hmisc::latex(AE2, device="postscript")

Hmisc::dvi(AE1.latex, height=7.5, width=8.1) ## wider paper
Hmisc::dvi(AE2.latex, height=8.7, width=8.2) ## wider paper
}

if (FALSE) {
## Hmisc options for latex
## graphics files are .png
options(latexcmd='pdflatex')
options(dviExtension='pdf')
if (nchar(Sys.which("open"))) {
  options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
} else {
  options(xdvicmd="xdg-open")  ## ubuntu linux
}
latexCheckOptions()

AE1.latex <- Hmisc::latex(AE1, device="png")
AE2.latex <- Hmisc::latex(AE2, device="png")

Hmisc::dvi(AE1.latex, height=7.5, width=8) ## wider paper
Hmisc::dvi(AE2.latex, height=8.7, width=8) ## wider paper
}
