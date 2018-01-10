MSWord_TablePlusGraphicColumn <- function(object,
                                          filetype=c("docx","html"),
                                          title=Hmisc::first.word(deparse(substitute(object))),
                                          rowname.header,
                                          data.header,
                                          graph.header,
                                          graph.file.directory="./",
                                          panel.files,  ## make sure they are in the correct order
                                          axis.files, legend.file,  ## optional
                                          height=stop("specify height of each panel in inches", call.=FALSE),
                                          width=stop("specify width of each panel in inches", call.=FALSE),
                                          height.key=height, width.key=width, ## inches
                                          FlexTableWidths, ## inches
                                          rmh.borders=TRUE,
                                          caption,
                                          file=paste0(title, ".", filetype),
                                          doc.title="Microplot",
                                          ...) {

  filetype <- match.arg(filetype)

  data.with.plot <- cbind(object, graph.header="")
  dimnames(data.with.plot)[[2]][ncol(data.with.plot)] <- graph.header

  if (!missing(axis.files)) {
    if ("x" %in% names(axis.files)) {
      data.with.plot <- rbind(data.with.plot,
                              " "="") ## 1 space
      axis.row <- nrow(data.with.plot)
    }
    if ("y" %in% names(axis.files)) {
      ## nothing written yet
    }
  }
  if (!missing(legend.file)) {
    data.with.plot <- rbind(data.with.plot,
                            "  "="") ## 2 spaces
    legend.row <- nrow(data.with.plot)
  }


  FT <- FlexTable(data=data.with.plot,
                      header.columns=TRUE,
                      add.rownames=TRUE,
                      header.text.props = textProperties(font.weight="bold"),
                      header.par.props = parProperties(text.align="center"),
                      header.cell.props=cellProperties(padding.left=10))
  FT[,1] <- textProperties(font.weight = "bold")
  FT[,1] <- parProperties(text.align="left")
  FT[1:nrow(object), 1+(1:ncol(object))] <- parProperties(text.align="right")
  if (!missing(rowname.header)) {
    FT[1, 1, to="header"] <- rowname.header
    FT[1, 1, to="header"] <- parProperties(text.align="center")
    }

  graph.column <- ncol(data.with.plot) + 1  ## FlexTable counts the rownames as a column
  for (i in seq(along=panel.files))
    FT[i, graph.column] <- pot_img(paste0(graph.file.directory, panel.files[i]),
                                          width=width, height=height)

  if (!missing(axis.files) && ("x" %in% names(axis.files)))
    FT[axis.row, graph.column] <-
      pot_img(paste0(graph.file.directory, axis.files["x"]),
                     width=width, height=height)
  if (!missing(legend.file)) {
    FT[legend.row, graph.column] <-
      pot_img(paste0(graph.file.directory, legend.file),
              width=width.key, height=height.key)
  }

  FT <- setFlexTableWidths(FT, widths = FlexTableWidths)


  ## borders
  if (rmh.borders) {
    FT <- setFlexTableBorders(
      FT,
      inner.vertical   = borderNone(),
      inner.horizontal = borderNone(),
      outer.vertical   = borderNone(),
      outer.horizontal = borderNone()
    )
    FT[1:nrow(object), 1, side="right"] <- borderProperties()
    FT[1, , side="top"] <- borderProperties()
  }

  ## header row
  FT <- addHeaderRow(FT,
                     value=c("", data.header, ""),
                     colspan=c(1, ncol(object), 1), first=TRUE,
                     par.properties=parProperties(text.align="center"),
                     cell.properties = cellProperties(border.color="transparent"))
  FT[1, 1+seq(along=ncol(object)), to="header"] <-
    cellProperties(border.top.color="transparent",
                   border.bottom.color="black",
                   border.left.color="transparent",
                   border.right.color="transparent")


  switch(filetype,
         docx={
           doc <- docx(title=doc.title)
           doc <- addParagraph(doc, "")
           if (!missing(caption))
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
