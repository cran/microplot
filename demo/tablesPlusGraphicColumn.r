library(microplot)
latexSetOptions()

## LaTeX example and MSWord example

data(toxicity) ## Count of Toxicities with simulated data
toxicity$Toxicity <- rownames(toxicity)

ToxColor <- c(rev(HH::likertColor(4)), "brown4")

## One panel
LL1 <- HH::likert(Toxicity ~ Grade1 + Grade2 + Grade3 + Grade4 + Grade5,
                  xlab="", ylab="",
                  data=toxicity, col=ToxColor, ReferenceZero=2.5,
                  scales=list(x=list(limits=c(-37,13), at=seq(-30,10,10),
                                     labels=c(30,20,10,0,10))),
                  auto.key=list(title="Grade", text=format(1:5)),
                  main="Counts of Grade by Toxicity")
LL1

## Four panels, one for each level of Toxicity
LL4 <- HH::likert(
             Toxicity ~ Grade1 + Grade2 + Grade3 + Grade4 + Grade5 | Toxicity,
             layout=c(1,4), strip=FALSE, xlab="", ylab="",
             data=toxicity[4:1,], col=ToxColor, ReferenceZero=2.5,
             scales=list(x=list(limits=c(-37,13), at=seq(-30,10,10),
                                labels=c(30,20,10,0,10)),
                         y=list(relation="free")),
             auto.key=FALSE,   ## auto.key doesn't isolate well
             key = list(text=list(format(1:5)),  ## highly specified key
                        rectangles=list(col=ToxColor, border="white"),
                        columns=5, size=2.8, between=.5,
                        between.columns=1.5, title = "Grade",
                        space="bottom"),
             main="Counts of Grade by Toxicity")
LL4


## 1. Display key, control of panel heights, specify column labels,
latex(LL4, y.axis=FALSE, height.panel=.25, width.panel=1.8,
      height.x.axis=.4, rowlabel="Toxicity", vectorgraph.colname="Counts by Grade",
      key=LL4$legend$bottom$args$key, height.key=.5, width.key=3,
      key.includegraphics=list(scale=.7, hspace.left="2.6in"))



## 2. Display as LaTeX table of numbers along with one column of graphs
toxicityf <- format(data.matrix(toxicity[, 1:5]))
colnames(toxicityf) <- 1:5

latex(LL4, y.axis=FALSE, height.panel=.25, width.panel=1.8,
      height.x.axis=.5, rowlabel="Toxicity", vectorgraph.colname="Counts by Grade",
      key=LL4$legend$bottom$args$key, height.key=.5, width.key=3,
      key.includegraphics=list(scale=.7, hspace.left="1.8in"),
      dataobject=toxicityf,
      cgroup=c("Grade",""),
      n.cgroup=c(5,1))


## 3. Display as MS Word table with column of graphs.

Toxdf.docx <-
  msWord(LL4, height.panel=.5, width.panel=3,
         key=LL4$legend$bottom$args$key,
         height.key=.5, width.key=3, key.align="right",
         dataobject=toxicityf, width.dataobject=.4,
         title="Toxicity2",
         rowlabel="Toxicity", width.rowname=2,
         data.header="Grade",
         graph.header="",
         vectorgraph.colname="Counts by Grade",
         y.axis=FALSE,
         caption="Simulated Counts of Grade by Toxicity")

print.default(Toxdf.docx)  ## print.default displays the file name

Toxdf.docx ## print method opens file
## You may pick up this graph and paste it into a larger .docx file.


## 4. Display as HTML table with column of graphs.

Toxdf.flextable <-
  msWord(LL4, height.panel=.5, width.panel=3,
         key=LL4$legend$bottom$args$key,
         height.key=.5, width.key=3, key.align="right",
         dataobject=toxicityf, width.dataobject=.4,
         title="Toxicity2",
         rowlabel="Toxicity", width.rowname=2,
         data.header="Grade",
         graph.header="",
         vectorgraph.colname="Counts by Grade",
         y.axis=FALSE,
         caption="Simulated Counts of Grade by Toxicity",
         filetype="html")

Toxdf.flextable
## Printing the returned value will display the table in your browser.
## If you wish to keep the html object, you MUST save it manually!
## The original file is in a temporary directory and will vanish when R is closed.
## Switch to the browser window and save the displayed table as a 'Web Archive'
## in a directory of your choice.  Choose an appropriate basename for the saved file,
## as the default basename of the file is the noninformative 'index'.
