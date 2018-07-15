## msWord.microplotMatrix vaguely corresponds to latex.includegraphicsMatrix

msWord <- function (object, ...)
  UseMethod("msWord")

msWord.microplotMatrix <-
  function(object, ## microplotMatrix
           ##         (matrix of filenames containing individual panels)
           ##         (0 columns permitted)
           filetype=c("docx","html"),
           dataobject=matrix(, nrow(object), 0), ## numeric or character matrix
           data.first=TRUE,
           title=first.word(deparse(substitute(object))),
           rowlabel=title,
           rowname=rownames(object),
           data.header="data",
           graph.header="graph",
           graph.file.directory="./",
           axis.files=attr(object,"axis.names"),
           lab.files=attr(object,"lab.names"),
           key.file=attr(object,"key.name"),
           key.par.properties=list(),
           x.axis=(!is.null(axis.files) && !is.null(axis.files["x"])),
           y.axis=(!is.null(axis.files) && !is.null(axis.files["y"])),
           xlab=FALSE,
           ylab=FALSE,
           label.x.axis="",  ## empty, nchar=0
           label.y.axis=" ", ## one space, nchar=1
           height.panel=1, ## inches
           width.panel=1, ## inches
           height.x.axis=height.panel[1], ## inches ## [1] is defensive for lazy evaluation
           width.y.axis=width.panel[1], ## inches
           height.xlab=height.panel[1], ## inches
           width.ylab=width.panel[1], ## inches
           height.key=height.panel[1], ## inches
           width.key=width.panel[1], ## inches
           FlexTableWidths=NULL, ## inches ## value used will be an attribute of result
           rmh.borders=TRUE,
           caption=NULL,
           file=paste0(title, ".", filetype),
           doc.title="Microplot",
           width.rowname=.4,
           width.dataobject=1,
           width.between=.1,
           landscape=FALSE,
           rgroup=NULL,
           n.rgroup=NULL,
           rgroup.exclude.borders=NULL,
           ...) {
    warning("The msWord methods are temporarily disabled in microplot_1.0-40.\n",
            "All other features of microplot continue to work.\n\n",
            "msWord depends on the ReporteRs package which was archived on July 16, 2018.\n",
            "The functions in the replacement package flextable have different\n",
            "calling sequences and I haven't worked out the details yet.\n",
            "In the meantime, please use the older release microplot_1.0-39\n",
            "along with the archived ReporteRs.\n\n")
  }

    ## this is the not-yet-functioning draft of the the new msWord
##     mm <- object

##     filetype <- match.arg(filetype)
##     title <- RemoveDot(title)

##     height.panel        <- rep(height.panel,     length=nrow(object)    )
##     width.panel         <- rep(width.panel,      length=ncol(object)    )
##     width.dataobject    <- rep(width.dataobject, length=ncol(dataobject))

##   w32 <- (version$os == "mingw32")  ## we use "\u2009" thinspace on non-Windows machines and "." on Windows machines

##   ## 0 columns of dataobject are possible, even 0 columns of object are possible
##     col.rn <- as.numeric(!is.null(rowname))
##     if (data.first) {
##     column.rownames   <- col.rn
##     column.dataobject <- col.rn + seq(along=dataobject[1,])
##     column.between    <- if (ncol(object)>0 && ncol(dataobject)>0)
##                          col.rn +      ncol(dataobject)     + (ncol(dataobject)>0)
##                          else integer(0)
##     column.ylab <- if (ylab && y.axis)
##                          col.rn +  ncol(dataobject)         + (ncol(dataobject)>0)      +      ylab
##                          else integer(0)
##     column.y.axis     <- if (y.axis)
##                          col.rn +      ncol(dataobject)     + (ncol(dataobject)>0)      +      ylab + y.axis
##                          else integer(0)
##     column.object     <- col.rn +      ncol(dataobject)     + (ncol(dataobject)>0)      +      ylab + y.axis   + seq(along=object[1,])
##     header.row <-  c(""[col.rn],               c(data.header, "")[ncol(dataobject)>0],                ""[y.axis],        graph.header   )
##     header.span <- c(1[col.rn],         c(ncol(dataobject),    1)[ncol(dataobject)>0],     (ylab+y.axis)[y.axis],         ncol(object))
##     if (is.null(FlexTableWidths))
##       FlexTableWidths <- c(width.rowname[col.rn],
##                                   width.dataobject,
##                            width.between[ncol(dataobject)>0],
##                                                                               width.ylab[ylab],
##                                                                                    width.y.axis[y.axis],
##                                                                                                           width.panel)
##   } else {
##     column.rownames   <- col.rn
##     column.ylab       <- if (ylab && y.axis)
##                          col.rn +  ylab
##                          else integer(0)
##     column.y.axis     <- if (y.axis)
##                            col.rn + ylab + y.axis
##                          else
##                            integer(0)
##     column.object     <- col.rn + ylab + y.axis + seq(along=object[1,])
##     column.between    <- if (ncol(object)>0 && ncol(dataobject)>0)
##                          col.rn + ylab + y.axis +      ncol(object)     + (ncol(dataobject)>0)
##                          else integer(0)
##     column.dataobject <- col.rn + ylab + y.axis +             ncol(object)  + (ncol(dataobject)>0) + seq(along=dataobject[1,])
##     header.row <-  c(""[col.rn] ,         ""[y.axis],            graph.header,   c("", data.header    )[ncol(dataobject)>0])
##     header.span <- c(1[col.rn],           (ylab+y.axis)[y.axis],  ncol(object),  c(1, ncol(dataobject))[ncol(dataobject)>0])
##     if (is.null(FlexTableWidths))
##       FlexTableWidths <- c(width.rowname[col.rn],
##                            width.ylab[ylab],
##                            width.y.axis[y.axis],
##                            width.panel,
##                            width.between[ncol(dataobject)>0],
##                            width.dataobject)
##   }


##   data.with.plot <- matrix("", nrow = nrow(object) + x.axis + xlab,
##                             ncol = col.rn + ncol(dataobject) + (ncol(dataobject)>0) + ylab + y.axis + ncol(object),
##                             dimnames=list(c(rownames(object), " "[x.axis], " "[xlab]), NULL))
##   colnames(data.with.plot) <- rep("", ncol(data.with.plot))
##   data.with.plot[1:nrow(object) , column.rownames ] <- rowname
##   data.with.plot[1:nrow(object) , column.dataobject] <- dataobject
##   if (!is.null(label.x.axis) && (nrow(object) < nrow(data.with.plot)))
##     data.with.plot[nrow(object)+1, column.y.axis] <- label.x.axis

##   colnames(data.with.plot)[column.rownames  ] <- rowlabel
##   colnames(data.with.plot)[column.dataobject] <- colnames(dataobject)
##   colnames(data.with.plot)[column.between   ] <- if (w32) "." else "\u2009" ##  U+2009  thin space (HTML &#8201; &thinsp;).
##   colnames(data.with.plot)[column.ylab      ] <- if (w32) "." else "\u2009" ##  https://en.wikipedia.org/wiki/Thin_space
##   colnames(data.with.plot)[column.y.axis    ] <- if (w32) "." else "\u2009" ##  https://en.wikipedia.org/wiki/Thin_space
##   if (!is.null(label.y.axis)) colnames(data.with.plot)[column.y.axis    ] <- label.y.axis
##   colnames(data.with.plot)[column.object    ] <- colnames(object)

##   if (!is.null(rgroup) && !is.null(n.rgroup)) {
##     group.rows <- cumsum(c(1, n.rgroup+1))[1:length(rgroup)]
##     group.names <- rgroup
##     overlap <- which(group.names %in% row.names(data.with.plot))
##     group.names[overlap] <- paste0(group.names[overlap], " ")
##     group.df <- matrix("", nrow=length(rgroup), ncol=ncol(data.with.plot),
##                        dimnames=list(group.names, colnames(data.with.plot)))
##     group.df[,1] <- group.names

##     object.rows <- (1:(nrow(data.with.plot) + nrow(group.df)))[-group.rows]
##     data.with.plot <- rbind(data.with.plot, group.df)[order(c(object.rows, group.rows)),]
##   } else {
##     group.rows <- numeric(0)
##     object.rows <- 1:nrow(object)
##   }

##     FT <- flextable(data.frame(data.with.plot))  ## not yet: restore non-syntactic column names
##     FT <- bold(FT, i=1, part="header")
##     FT <- align(FT, i=1, align="center", part="header")
##     FT <- bold(FT, j=1, part="body")
##     FT <- align(FT, j=1, align="left", part="body")
##     FT <- align(FT, i=object.rows, j=column.dataobject, align="right", part="body")

##   ## FT <- FlexTable(data=data.with.plot,
##   ##                     header.columns=TRUE,
##   ##                     add.rownames=FALSE,
##   ##                     header.text.props = textProperties(font.weight="bold"),
##   ##                     header.par.props = parProperties(text.align="center"))
##   ## FT[,1] <- textProperties(font.weight = "bold")
##   ## FT[,1] <- parProperties(text.align="left")
##   ## FT[object.rows, column.dataobject] <- parProperties(text.align="right")


##     for (i in 1:length(object.rows))
##       for (j in seq(along=column.object))
##         if (nchar(object[i, column.object[j]+1-column.object[1]]) > 0) {
##           src <- paste0(graph.file.directory, object[i, column.object[j]+1-column.object[1]])
##           image.src <- as_image(j,
##                                 src=src,
##                                 width=width.panel[j], height=height.panel[i])
##           FT <- display(FT, i=i, col_key=j, pattern="{{I}}",
##                         formatters=list(I ~ image.src
##                                         ))
##         }
##           ## FT[object.rows[i], column.object[j]] <- pot_img(paste0(graph.file.directory, object[i, column.object[j]+1-column.object[1]]),
##           ##                                                 width=width.panel[j], height=height.panel[i])
## recover()

##     if (y.axis) {
##       if (ylab) {
##         for (i in object.rows)
##           if (nchar(lab.files["y"]) > 0)
##             FT <- display(FT, i=i, col_key=column.ylab, pattern="{{I}}",
##                           formatters=list(I ~ as_image(column.ylab,
##                                                        src=paste0(graph.file.directory, lab.files["y"]),
##                                                        width=width.ylab, height=height.panel[i])
##                                           ))
##            ## FT[i, column.ylab] <- pot_img(paste0(graph.file.directory, lab.files["y"]),
##            ##      a                          width=width.ylab, height=height.panel[i])
##       }
##       for (i in object.rows)
##         if (nchar(axis.files["y"]) > 0)
##           FT <- display(FT, i=i, col_key=column.y.axis, pattern="{{I}}",
##                         formatters=list(I ~ as_image(column.y.axis,
##                                                      src=paste0(graph.file.directory, axis.files["y"]),
##                                                      width=width.y.axis, height=height.panel[i])
##                                         ))
##           ## FT[i, column.y.axis] <- pot_img(paste0(graph.file.directory, axis.files["y"]),
##           ##                                 width=width.y.axis, height=height.panel[i])
##     }

##     if (x.axis) {
##       for (j in seq(along=column.object))
##         if (nchar(axis.files["x"]) > 0)
##           FT <- display(FT, i=nrow(object)+1, col_key=column.object[j], pattern="{{I}}",
##                         formatters=list(I ~ as_image(column.object[j],
##                                                      src=paste0(graph.file.directory, axis.files["x"]),
##                                                      width=width.panel[j], height=height.x.axis)
##                                         ))
##           ## FT[nrow(object)+1, column.object[j]] <-
##           ##   pot_img(paste0(graph.file.directory, axis.files["x"]),
##           ##           width=width.panel[j], height=height.x.axis)

##       if (xlab) {
##         for (j in seq(along=column.object))
##           if (nchar(lab.files["x"]) > 0)
##             FT <- display(FT, i=nrow(object)+2, col_key=column.object[j], pattern="{{I}}",
##                           formatters=list(I ~ as_image(column.object[j],
##                                                        src=paste0(graph.file.directory, lab.files["x"]),
##                                                        width=width.panel[j], height=height.xlab)
##                                           ))
##             ## FT[nrow(object)+2, column.object[j]] <-
##             ##   pot_img(paste0(graph.file.directory, lab.files["x"]),
##             ##           width=width.panel[j], height=height.xlab)
##       }
##     }

##     ##  FT[group.rows,1] <- parProperties(text.align="center")
##     FT <- align(FT, i=group.rows, j=1, align="center", part="body")

##   ## FT <- setFlexTableWidths(FT, widths = FlexTableWidths)
##   FT <- width(FT, width = FlexTableWidths)

## ## not yet
##   ## ## borders
##   ## if (rmh.borders) {
##   ##   FT <- setFlexTableBorders(
##   ##     FT,
##   ##     inner.vertical   = borderNone(),
##   ##     inner.horizontal = borderNone(),
##   ##     outer.vertical   = borderNone(),
##   ##     outer.horizontal = borderNone()
##   ##   )
##   ##   right.border.rows <-
##   ##     if (is.null(rgroup.exclude.borders))
##   ##       object.rows
##   ##     else
##   ##       object.rows[-rgroup.exclude.borders]
##   ##   FT[right.border.rows, 1, side="right"] <- borderProperties()
##   ##   FT[1, , side="top"] <- borderProperties()

##   ##   FT[group.rows, , side="top"] <- borderProperties(color="black")
##   ## }


##     ## header row
##     if ((ncol(object) > 0 && ncol(dataobject) > 0) ||
##         !missing(data.header) ||
##         !missing(graph.header) > 0) {
## recover() ## start here for detail
##       FT <- add_header(FT,
##                          top=TRUE,
##                          value=header.row,
##                          colspan=header.span,
##                          par.properties=parProperties(text.align="center"),
##                          cell.properties = cellProperties(border.color="transparent"))
##       ## FT <- addHeaderRow(FT,
##       ##                    value=header.row,
##       ##                    colspan=header.span,
##       ##                    first=TRUE,
##       ##                    par.properties=parProperties(text.align="center"),
##       ##                    cell.properties = cellProperties(border.color="transparent"))

## ## not yet
##       ## if (nchar(data.header) > 0)
##       ##   FT[1, column.dataobject, to="header"] <-
##       ##     cellProperties(border.top.color="transparent",
##       ##                    border.bottom.color="black",
##       ##                    border.left.color="transparent",
##       ##                    border.right.color="transparent")
##       ## if (nchar(graph.header) > 0)
##       ##   FT[1, column.object, to="header"] <-
##       ##     cellProperties(border.top.color="transparent",
##       ##                    border.bottom.color="black",
##       ##                    border.left.color="transparent",
##       ##                    border.right.color="transparent")
##     }

##   switch(filetype,
##          docx={
##            doc <- read_docx() ## title=doc.title)
##            if (landscape)
##              doc = body_end_section_portrait(doc)
##            doc <-  body_add_par(doc, "")
##            if (!missing(caption))
##              doc <-  body_add_par(doc, caption,
##                                  stylename="rTableLegend")
##            doc <- body_add_flextable(doc, FT)
##            if (!is.null(key.file)) {
##              ## doc <-  body_add_par(doc,
##              ##                     pot_img(paste0(graph.file.directory, key.file),
##              ##                             width=width.key, height=height.key),
##              ##                     par.properties=do.call(parProperties, key.par.properties))
##              key.pathname <- matrix(paste0(graph.file.directory, key.file), 1, 1, dimnames=list("ROW","ONE"))
##              key.ft <- flextable(key.pathname)
##              key.ft <- display(key.ft, i="ROW", col_key="ONE",
##                                pattern="{{K}}",
##                                formatters=list( K ~ as_image(ONE, src=key.pathname,
##                                                              width=width.key, height=height.key)),
##                                fprops=list())
##              doc <- body_add_flextable(doc, key.ft)
##              }
##            if (landscape)
##              doc = body_end_section_landscape(doc)
##            print(doc, target=file)
##            class(file) <- c("msWordFilename", "OSfilename")
##          },
##          html={ ## not right yet
##            htmltools::save_html(htmltools::browsable(htmltools::HTML(ReporteRs::as.html(FT))),
##                                 file=file,
##                                 libdir=graph.file.directory)
##            class(file) <- c("htmlFilename", "OSfilename")
##          })

##     attr(file, "FlexTableWidths") <- FlexTableWidths

##     attr(file, "microplotMatrix") <- mm

##   file
## }



msWord.graphicsClass <-
  function(object,
           ## microplot arguments
           figPrefix=first.word(deparse(substitute(object))),
           device="png",
           key=FALSE,
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## microplot,
           ## msWord.microplotMatrix
           )
{
  ## change "." to "-" in figPrefix
  ##  The 'latex' macro \code{\\includegraphics} requires that there be no
  ##  \code{"."} in the filename basename.  We enforce it for msWord also.
  figPrefix <- RemoveDot(figPrefix)
  dir.verify(figPrefix)

  ## microplot.* for object class: trellis, ggplot, graphicList
  mm <- microplot(object=object,
                  device=device,
                  key=key,
                  figPrefix=paste0(figPrefix,"/fig"),
                  ...)
  ## mm is a matrix of characters containing filenames, with 0, 1, 2, or 3 attributes
  ## with class="microplotMatrix"

  force(title)
  msWord(mm, title=title, ...)
}


msWord.trellis <-
  function(object=stop("trellis object is required", call. = FALSE),
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## msWord.graphicsClass,
           ## microplot,
           ## msWord.microplotMatrix
           )
{
  force(title)
  msWord.graphicsClass(object, figPrefix=figPrefix, title=title, ...)
}


msWord.ggplot <-
  function(object=stop("ggplot object is required", call. = FALSE),
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## msWord.graphicsClass,
           ## microplot,
           ## msWord.microplotMatrix
           )
{
  force(title)
  msWord.graphicsClass(object, figPrefix=figPrefix, title=title, ...)
}


msWord.graphicsList <-
  function(object=stop("graphicsList object is required", call. = FALSE),
           ## matrix or vector of trellis objects or ggplot objects,
           ## with dim and dimnames,
           ## normally each containing one panel.
           ## The axes and key will be taken from object[[1]].
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## msWord.graphicsClass,
           ## microplot,
           ## msWord.microplotMatrix
           )
{
  force(title)
  msWord.graphicsClass(object, figPrefix=figPrefix, title=title, ...)
}
