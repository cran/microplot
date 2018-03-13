library(microplot)
library(lattice)
library(latticeExtra)
library(ReporteRs)

## MS Word example using png files.

## This example shows both lattice and ggplot2 graphics.
##
## See ?microplot for additional examples with
## lattice graphics, base graphics, and ggplot2 graphics.

## 1. Produce table of numbers
iris.melt <- reshape2::melt(iris, id="Species")

## table of five-number summmary of each Species and measurement
iris2 <- array(iris.melt$value,
               dim=c(50, 3, 4),
               dimnames=list(NULL,
                 levels(iris.melt$Species),
                 levels(iris.melt$variable)))
iris2.fivenum <- apply(iris2, 2:3, fivenum)
dimnames(iris2.fivenum)[[1]] <- c("min", "Q1", "med", "Q3", "max")

## Species and Measurement in same columns, Measurement names adjusted to be unique
BW5num <-
rbind(
data.frame(t(iris2.fivenum[,1,])),
data.frame(t(iris2.fivenum[,2,])),
data.frame(t(iris2.fivenum[,3,])))

BW5num <- data.frame(Species=rep(dimnames(iris2.fivenum)[[2]], each=4),
                     Measurement=row.names(BW5num)[1:4],
                     format(BW5num, digits=3, nsmall=2),
                     "Box Plots"="",
                     check.names=FALSE,
                     stringsAsFactors=FALSE)



## 2. lattice
##    matrix of bwplots
irisBW <- bwplot( ~ value | Species * variable, data=iris.melt)
latticeExtra::useOuterStrips(irisBW)  ## screen device

##    twelve individual boxplots without axes
irisBW.update <- layoutCollapse(irisBW)

png("irisBW%03d.png", height=.25, width=1.5, units="in", res=300)  ## inch
irisBW.update ## 12 panels
update(irisBW.update[1,1], ## x-axis
       par.settings=list(layout.heights=list(axis.bottom=1, panel=0),
                         axis.line=list(col="black")))
suppress <- dev.off()

graphnames <- paste0("./irisBW", sprintf("%03i", 1:13), ".png")

graphicsnames <- c(matrix(graphnames[1:12], 4, 3, byrow=TRUE), graphnames[13])


## 3. Use ReporteRs package to construct docx file with the table and microplots.

doc <- docx(title="Microplot package")
doc <- addParagraph(doc, "Microplot package: creating MS Word table with ReporteRs.")
doc <- addParagraph(doc, "")

FT <- FlexTable(data=BW5num,
                header.columns=TRUE,
                add.rownames=FALSE,
                header.text.props = textProperties(font.weight="bold"))
FT[,1] <- textProperties(font.weight = "bold")
for (i in 1:12)
  FT[i, 8] <-  pot_img(graphicsnames[i], width=1.5, height=.25)

doc <- addParagraph(doc, "Table with lattice bwplot column")
doc <- addFlexTable(doc, FT)



FTx <- FlexTable(data=rbind(BW5num, " "=""),
                 header.columns=TRUE,
                 add.rownames=FALSE,
                 header.text.props = textProperties(font.weight="bold"))
FTx[,1] <- textProperties(font.weight = "bold")
for (i in 1:13)
  FTx[i, 8] <-  pot_img(graphicsnames[i], width=1.5, height=.25)

doc <- addParagraph(doc, "")
doc <- addParagraph(doc, "Table with lattice bwplot column and axis row")
doc <- addFlexTable(doc, FTx)

writeDoc(doc, file="microplot-lattice-ReporteRs-png.docx")

system(paste("open", "microplot-lattice-ReporteRs-png.docx"))
