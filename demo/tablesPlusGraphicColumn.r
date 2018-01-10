## Count of Toxicities with simulated data
## LaTeX example and MSWord example

Tox2 <-
  structure(list(Grade1 = c(20, 30, 20, 10),
                 Grade2 = c(10, 5, 10, 20),
                 Grade3 = c(5, 2, 4, 5),
                 Grade4 = c(0, 0, 3, 0),
                 Grade5 = c(0, 0, 0, 1)),
            .Names = c("Grade1", "Grade2", "Grade3", "Grade4", "Grade5"),
            row.names = c("Anemia", "Lymphocyte count decrease",
                          "Neutrophil count decrease",
                          "White blood cell decrease"),
            class = "data.frame")
Tox2$Toxicity <- rownames(Tox2)
Tox2

ToxColor <- c(rev(HH::likertColor(4)), "brown4")

## One panel
LL1 <- HH::likert(Toxicity ~ Grade1 + Grade2 + Grade3 + Grade4 + Grade5,
                  data=Tox2, col=ToxColor, ReferenceZero=2.5,
                  scales=list(x=list(limits=c(-37,12), at=seq(-30,10,10), labels=c(30,20,10,0,10))),
                  auto.key=list(title="Grade", text=format(1:5)),
                  main="Counts of Grade by Toxicity")
LL1

## Four panels, one for each level of Toxicity
LL4 <- HH::likert(Toxicity ~ Grade1 + Grade2 + Grade3 + Grade4 + Grade5 | Toxicity,
                  layout=c(1,4), strip=FALSE,
                  data=Tox2, col=ToxColor, ReferenceZero=2.5,
                  scales=list(x=list(limits=c(-37,12), at=seq(-30,10,10), labels=c(30,20,10,0,10)),
                              y=list(relation="free")),
                  auto.key=FALSE,                      ## auto.key doesn't isolate well.
                  key = list(text=list(format(1:5)),   ## Draw a highly specified key.
                             rectangles=list(col=ToxColor, border="white"),
                             columns=5, size=2.8, between=.5,
                             between.columns=1.5, title = "Grade"),
                  main="Counts of Grade by Toxicity")
LL4


## Hmisc options.
options(latexcmd='pdflatex')
options(dviExtension='pdf')
if (nchar(Sys.which("open"))) {
  options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
} else {
  options(xdvicmd="xdg-open")  ## ubuntu linux
}
latexCheckOptions()


## 1. Display just the graphs using "trellis" method of latex(),
##    therefore latex.trellis will automatically set the style to "graphicx".
Hmisc::latex(LL4, raise="-2ex", y.axis=FALSE, height=.4, width=2.5)



## 2. display as LaTeX table of numbers with one column of graphs

filenamespdf <-
  microplot(LL4, height=.5, width=4, key=LL4$legend$top$args$key, device="pdf")

Tox2f <- format(Tox2[4:1, 1:5])
dimnames(Tox2f)[[2]] <- 1:5

Tox2f.df <- data.frame(rbind(Tox2f,
                             " " ="",  ## one blank
                             "  "=""), ## two blanks
                       "Counts by Grade"=c(as.includegraphics(filenamespdf, height="1.5em"),
                                           as.includegraphics(attr(filenamespdf, "axis.names")["x"], height="1.5em"),
                                           as.includegraphics(attr(filenamespdf, "key.name"),
                                                              viewport="0 0 288 36",
                                                              trim="45 0 45 0",
                                                              width=paste0(1.45*(198/288)*(4/.5)*1.5,"em"),
                                                              height=paste0(1.45*1.5,"em"))),
                       stringsAsFactors=FALSE,
                       check.names=FALSE)

Toxdf.latex <- Hmisc::latex(Tox2f.df,
                            title="Toxicity",
                            caption="Simulated Counts of Grade by Toxicity",
                            cgroup=c("Grade",""),
                            col.just=c(rep("r",5), "l"),
                            n.cgroup=c(5,1))
Toxdf.latex$style <- "graphicx"  ## needed because Toxdf.latex is a data.frame, not a "trellis" object
##
print.default(Toxdf.latex)  ## display the file name.  \include this file in a larger .tex file
Toxdf.latex  ## Call the system latex program and display the resulting pdf file on screen.





## 3. Display as MS Word table with column of graphs.
## MSWord_TablePlusGraphicColumn is a wrapper that uses functions in the ReporteRs package.
  filenamespng <-
    microplot(LL4, height=.5, width=3, key=LL4$legend$top$args$key, device="png")

  Tox2f <- format(Tox2[4:1, 1:5])
  dimnames(Tox2f)[[2]] <- 1:5

  Toxdf.docx <-
    MSWord_TablePlusGraphicColumn(Tox2f,
                                  title="Toxicity",
                                  rowname.header="Toxicity",
                                  data.header="Grade",
                                  graph.header="Counts by Grade",
                                  panel.files=filenamespng,
                                  axis.files=attr(filenamespng, "axis.names"),
                                  legend.file=attr(filenamespng, "key.name"),
                                  height=.5, width=3, ## inches  ## must be the same height and width as in the microplot call
                                  FlexTableWidths=c(2, rep(.4, 5), 3.2), ## inches, rownames count as a column
                                  ##                               3.2  slightly larger than the width as in the microplot call
                                  rmh.borders=TRUE,
                                  caption="Simulated Counts of Grade by Toxicity")

  Toxdf.docx  # file name
  system(paste("open", Toxdf.docx))  ## pick up this graph and paste it into a larger .docx file.

