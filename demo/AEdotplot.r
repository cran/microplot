library(microplot)
## Hmisc options for pdflatex
## graphics files are .pdf
latexSetOptions()



data(AEdata, package="HH")

levels(AEdata$AE) <- Hmisc::upFirst(tolower(levels(AEdata$AE)))

AE1 <- HH::AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)

AEdatac1 <- AEdata

AEdatac1$AE <- factor(AEdata$AE, levels=levels(attr(AE1, "AEtable")$AEdata$PREF))

AE1c <- HH::AEdotplot(AE ~ nAE/nTRT | AE, groups = TRT, data = AEdatac1)

class(AE1c)



AE2 <- HH::AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)

AEdatac2 <- AEdata

AEdatac2$AE <- factor(AEdata$AE, levels=unlist(AE2$text.plot$y.limits))

AEdatac2$OrgSys <- factor(AEdata$OrgSys, levels=names(attr(AE2, "n.events")))

AE2c <- HH::AEdotplot(AE ~ nAE/nTRT | AE, groups = TRT, data = AEdatac2) ## AE, not OrgSys in condition

class(AE2c)


#####  latex #####

dvi(latex(AE1c),
    height=9.4, width=9.6)


dvi(
  latex(AE2c,
        rowlabel="Most Frequent On-Therapy Adverse Events",
        rgroup=c(rev(names(attr(AE2, "n.events"))),"\\vspace*{-2ex}"),
        n.rgroup=c(rev(attr(AE2, "n.events")),1)),
  height=10.4, width=9.5)


if(FALSE){
#####  msWord #####

AE1c.docx <- msWord(AE1c,
                    rgroup.exclude.borders=c(34, 35))
AE1c.docx


AE2c.docx <- msWord(AE2c,
                    rgroup=c(rev(names(attr(AE2, "n.events"))),""),
                    n.rgroup=c(rev(attr(AE2, "n.events")),2),
                    rgroup.exclude.borders=c(34, 35))
AE2c.docx

}
