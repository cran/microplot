microplot.AEdotplot <- function(object,  ## class(object)=="AEdotplot", for example, one of these:
                                ## object <- AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)
                                ## object <- AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)
                                ## object <- AEdotplot(AE ~ nAE/nTRT | AE, groups = TRT, data = AEdata)
                                figPrefix=Hmisc::first.word(substitute(object)),
                                title="Adverse Event",
                                main=attr(object, "main")[[1]],
                                sub=attr(object, "sub")[[1]],
                                height=.2, width.pct=2, width.RR=1.5, ## inch, each panel
                                key=attr(object, "ae.key"),
                                device=c("pdf","postscript","png", if (version$os == "mingw32") "win.metafile"),
                                ...) { ## ... ignored

  naoA <- length(attributes(object)$AEtable)
  rgroup <-  NULL
  n.rgroup <- NULL
  OrgSys <- NULL

  if (naoA == 1) { ## object <- AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)
    AEtable <- attributes(object)$AEtable$AEdata
    class(AEtable) <- class(AEtable)[-1]
    RAND <- AEtable$RAND ## 'AEtable$RAND' makes R CMD check happier
    AEplotEdit <- AEdotplot(PREF ~ SAE/SN | PREF, groups = RAND, data = AEtable,
                            sortbyRelativeRisk=FALSE)
    AEtable.ordered <- attributes(AEplotEdit)$AEtable
  } else { ## object <- AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)
    ## or  ## object <- AEdotplot(AE ~ nAE/nTRT | AE, groups = TRT, data = AEdata)
    AEtable.ordered <- do.call(rbind, attributes(object)$AEtable)  ## PREF is ordered factor
    if (naoA == nrow(AEtable.ordered)/2) { ## object <- AEdotplot(AE ~ nAE/nTRT | AE, groups = TRT, data = AEdata)
      AEplotEdit <- object
      AEtable.ordered <- attributes(object)$AEtable
    }
    else { ## object <- AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)
      class(AEtable.ordered) <- class(AEtable.ordered)[-1]
      rgroup <- c(rev(names(attributes(object)$AEtable)), "")
      n.rgroup <- c(rev(sapply(attributes(object)$AEtable, nrow)/2), 2)
      OrgSys <- ordered(rep(rgroup, n.rgroup), levels=OrgSys)
      RAND <- AEtable.ordered$RAND ## 'AEtable.ordered$RAND' makes R CMD check happier
      AEplotEdit <- AEdotplot(PREF ~ SAE/SN | PREF, groups = RAND, data = AEtable.ordered)
      AEtable.ordered <- attributes(AEplotEdit)$AEtable
    }
  }

  Percent.graphnames <- microplot(update(AEplotEdit[[1]], as.table=TRUE), figPrefix=paste0(figPrefix, "Percent"),
                                  height=height, width=width.pct, device=device,
                                  key=key)
  RelRisk.graphnames <- microplot(update(AEplotEdit[[2]], as.table=TRUE), figPrefix=paste0(figPrefix, "RelRisk"),
                                  height=height, width=width.pct, device=device,
                                  key=FALSE)


  AEtable <- t(sapply(AEtable.ordered,
                      function(x) {
                        c(unlist(x[1, c(2,3,6)]),
                          unlist(x[2, c(2,3,6, 7:13)]))}))
  rownames(AEtable) <- Hmisc::upFirst(tolower(rownames(AEtable)))
  nAE <- nrow(AEtable) ## length(levels(AEtable$AEdata[,"PREF"]))


  fullplot <-
    data.frame(round(AEtable[,1, drop=FALSE], digits=0),
               round(AEtable[,3, drop=FALSE], digits=2),
               round(AEtable[,4, drop=FALSE], digits=0),
               round(AEtable[,6, drop=FALSE], digits=2),
               round(AEtable[,7, drop=FALSE], digits=2),
               Percent=Percent.graphnames[1:nAE],
               "RelRisk-95%CI"=RelRisk.graphnames[1:nAE],
               check.names=FALSE, stringsAsFactors=FALSE)
  if (is.null(OrgSys))
    fullplot <- fullplot[order(fullplot$relrisk, decreasing=TRUE),]
  else
    fullplot <- fullplot[nrow(fullplot):1,]
  fullplot[" ", 6:7] <- c(attr(Percent.graphnames,"axis.names")["x"],
                          attr(RelRisk.graphnames,"axis.names")["x"])
  fullplot["  ", 6] <- attr(Percent.graphnames,"key.name")
  names(fullplot)[1:5] <- c("nAE","pct","nAE","pct","RelRisk")
  attr(fullplot, "figPrefix") <- figPrefix
  attr(fullplot, "title") <- title
  attr(fullplot, "caption") <- main
  attr(fullplot, "sub") <- sub
  attr(fullplot, "cgroup") <- c(levels(AEtable.ordered[[1]]$RAND),"", "AEdotplot")
  attr(fullplot, "n.cgroup") <- c(2,2,1,2)
  attr(fullplot, "rgroup") <- rgroup
  attr(fullplot, "n.rgroup") <- n.rgroup
  attr(fullplot, "device") <- device
  attr(fullplot, "height") <- height
  attr(fullplot, "width.pct") <- width.pct
  attr(fullplot, "width.RR") <- width.RR
  class(fullplot) <- c("AEdotplot.microplot", class(fullplot))
  fullplot
}

latex.AEdotplot.microplot <- function(object, ...) {
  class(object) <- class(object)[-1]
  keyrow <- nrow(object)
  object[,6] <- as.includegraphics(object[,6], height="1em")
  object[-keyrow,7] <- as.includegraphics(object[-keyrow,7], height="1em")
  fullplot.latex <- Hmisc::latex(object,
                                 file=paste0(attr(object,"figPrefix"), ".tex"),
                                 title=attr(object, "title"),
                                 caption=attr(object, "caption"),
                                 cgroup=attr(object, "cgroup"),
                                 n.cgroup=attr(object, "n.cgroup"),
                                 rgroup=attr(object, "rgroup"),
                                 n.rgroup=attr(object, "n.rgroup"),
                                 ...)
  fullplot.latex$style <- "graphicx"
  fullplot.latex$sub <- attr(object, "sub")
  fullplot.latex
}



MSWord.AEdotplot.microplot <- function(object,
                                       filetype=c("docx","html"),
                                       graph.file.directory="./",
                                       width.pct=attr(object,"width.pct"),
                                       width.RR=attr(object,"width.RR"),
                                       height=attr(object,"height"),
                                       title=attr(object, "title"),
                                       caption=attr(object, "caption"),
                                       figPrefix=attr(object, "figPrefix"),
                                       rmh.borders=TRUE,
                                       FlexTableWidths=c(1.5, .6, .6, .6, .6, .7, 2.1, 1.6),
                                       file=paste0(figPrefix, ".", filetype),
                                       ... ## ... ignored
                                       ) {

  filetype <- match.arg(filetype)

  cgroup <- attr(object,"cgroup")
  n.cgroup <- attr(object, "n.cgroup")
  rgroup <- attr(object,"rgroup")
  n.rgroup <- attr(object, "n.rgroup")

  objectSansFig <- object
  objectSansFig[,1:5] <- format(object[,1:5])
  objectSansFig[nrow(object)+c(-1,0),1:5] <- " " ## axis and legend rows
  objectSansFig[nrow(object), 7] <- ""
  AE.rows <- 1:nrow(object)
  OrgSys.rows <- integer(0)

  if (length(attr(object,"rgroup")) > 0) {
    OrgSys.rows <- cumsum(c(1, attr(object,"n.rgroup")+1))[1:length(attr(object,"rgroup"))]
    OrgSys.names <- attr(object,"rgroup")
    overlap <- which(OrgSys.names %in% row.names(objectSansFig))
    OrgSys.names[overlap] <- paste0(OrgSys.names[overlap], " ")
    OrgSys.df <- data.frame(check.names=FALSE,
                            matrix("", nrow=length(attr(object,"rgroup")), ncol=ncol(object),
                                   dimnames=list(OrgSys.names, names(object))))

    AE.rows <- (1:(nrow(object) + nrow(OrgSys.df)))[-OrgSys.rows]
    objectSansFig <- rbind(objectSansFig, OrgSys.df)[order(c(AE.rows, OrgSys.rows)),]
  }
  objectSansFig[, 8:9] <- objectSansFig[, 6:7]
  objectSansFig[, 6:7] <- NA
  names(objectSansFig)[3:4] <- c("nA", "pc") ## first few letters


  FT <- FlexTable(data=objectSansFig[, 1:7],
                  header.columns=TRUE,
                  add.rownames=TRUE,
                  header.text.props = textProperties(font.weight="bold"),
                  header.par.props = parProperties(text.align="center")
                  )

  FT[,1] <- textProperties(font.weight = "bold")
  FT[,1] <- parProperties(text.align="left")
  FT[OrgSys.rows,1] <- parProperties(text.align="center")
  FT[1:nrow(objectSansFig), 1+(1:(ncol(objectSansFig)-2))] <- parProperties(text.align="right")
  FT[1, 1, to="header"] <- "Adverse Event"
  FT[1, 4, to="header"] <- "E" ## remaining letters
  FT[1, 5, to="header"] <- "t" ## remaining letters
  FT[1, 1, to="header"] <- parProperties(text.align="center")

  ## Percent
  ## FlexTable counts the rownames as a column
  for (i in AE.rows)
    FT[i, 7] <- pot_img(paste0(graph.file.directory, objectSansFig[i,8]),
                        width=width.pct, height=height)
  ## Relative risk
  for (i in AE.rows[1:(length(AE.rows)-1)])
    FT[i, 8] <- pot_img(paste0(graph.file.directory, objectSansFig[i,9]),
                        width=width.RR, height=height)  ## borders

  if (rmh.borders) {
    FT <- setFlexTableBorders(
      FT,
      inner.vertical   = borderNone(),
      inner.horizontal = borderNone(),
      outer.vertical   = borderNone(),
      outer.horizontal = borderNone()
    )
    FT[AE.rows[1:(length(AE.rows)-2)], 1, side="right"] <- borderProperties()
    FT[OrgSys.rows, , side="top"] <- borderProperties()
    FT[1, , side="top"] <- borderProperties()
  }

  ## header row
  FT <- addHeaderRow(FT,
                     value=c("",cgroup),
                     colspan=c(1, n.cgroup), first=TRUE,
                     par.properties=parProperties(text.align="center"),
                     cell.properties = cellProperties(border.color="transparent"))
  FT[1,, to="header"] <-
    cellProperties(border.top.color="black",
                   border.bottom.color="black",
                   border.left.color="transparent",
                   border.right.color="transparent")
  FT[nrow(objectSansFig),] <-
    cellProperties(border.top.color="transparent",
                   border.bottom.color="black",
                   border.left.color="transparent",
                   border.right.color="transparent")
  FT <- setFlexTableWidths(FT, widths=FlexTableWidths)

  switch(filetype,
         docx={
           doc <- docx(title=title)
           doc <- addParagraph(doc, "")
           doc <- addParagraph(doc, caption,
                               stylename="rTableLegend")
           doc <- addFlexTable(doc, FT)
           writeDoc(doc, file=file)
         },
         html={
           htmltools::save_html(htmltools::browsable(htmltools::HTML(ReporteRs::as.html(FT))),
                                file=file,
                                libdir=graph.file.directory)
         })

  invisible(file)
}


latex.AEdotplot <- function(object, figPrefix=Hmisc::first.word(substitute(object)), device="pdf", ...) {
  ## ... are arguments to latex.default
  Hmisc::latex(microplot(object, figPrefix=figPrefix, device=device), ...)
}

MSWord.AEdotplot <- function(object, filetype="docx", figPrefix=Hmisc::first.word(substitute(object)), device="png", ...) {
  ## ... ignored
  MSWord(microplot(object, figPrefix=figPrefix, device=device), filetype=filetype)
}
