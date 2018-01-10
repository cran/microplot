latex.trellis <-
  function(## microplot arguments
           object=stop("trellis object is required", call. = FALSE), ## object must have class "trellis"
           obji=NULL, ## Vector or matrix of trellis objects, with dimnames, normally each containing one panel.
           ##         ## When obji is non-null, the axes and key will be taken from object.
           ##         ## We need object to be a trellis object so the latex generic will dispatch to latex.trellis.
           figPrefix=Hmisc::first.word(deparse(substitute(object))),
           device=c("pdf","postscript","png", ## other devices require a user-written function
                    if (version$os == "mingw32") "win.metafile"),
           height=1, width=1, ## microplot, numeric, in inches
           collapse=layoutCollapse, ## see below for example ## zero out unwanted layout.heights and layout.widths.
           height.x.axis=height,
           par.settings.x.axis=
             list(layout.heights=list(panel=0, axis.bottom=1, axis.xlab.padding=1, xlab=1),
                  axis.line=list(col="black")),
           width.y.axis=width,
           par.settings.y.axis=
             list(layout.widths=list(ylab=1, ylab.axis.padding=1, axis.left=1, panel=0),
                  axis.line=list(col="black")),
           key=FALSE,    ## FALSE or a list of arguments defining a key
           height.key=height, width.key=width,
           ##
           ## as.includegraphics arguments
           height.as=NULL, ## character with TeX units, for example "1in"
           width.as=NULL,  ## keeps aspect ratio
           scale=NULL,     ## ignored if either height.as or width.as is specified.
           raise=NULL,     ## argument to as.includegraphics
           hspace.left=NULL,  ## argument to as.includegraphics
           hspace.right=NULL, ## argument to as.includegraphics
           wd=getwd(),     ## working directory for graphics files
           viewport=NULL,  ## if specified, then left bottom right top (character)
           ## used for pdf png jpeg
           ## See MediaBox in pdf file.
           ## Ask operating system for png or jpg file.
           bb=NULL, ## if specified, then left bottom right top (character)
           ## used for bmp tiff ps, ask operating system for values
           trim=NULL, ## for example, "0 0 0 0" left bottom right top (character)
           x.axis.includegraphics=TRUE, ## logical or a list of arguments
           y.axis.includegraphics=TRUE, ## logical or a list of arguments
           key.includegraphics=is.list(key),  ## logical or a list of arguments
           ##
           ## latex.includegraphicsMatrix arguments, including arguments to latex.default
           ...) {

    ## microplot will zero out unwanted layout.heights and layout.widths space.
    ## for example:
    ## latex(tt, collapse=function(x) layoutCollapse(x,
    ##                                               layout.heights=list(axis.bottom=.3),
    ##                                               layout.widths=list(axis.left=.3),
    ##                                               axis.line=list(col="green")))

    ## microplot.trellis
    mm <- microplot(object=object,
                    figPrefix=figPrefix,
                    device=device,
                    height=height, width=width, ## numeric, in inches
                    collapse=collapse,
                    height.x.axis=height.x.axis,
                    par.settings.x.axis=par.settings.x.axis,
                    width.y.axis=width.y.axis,
                    par.settings.y.axis=par.settings.y.axis,
                    key=key,
                    height.key=height.key, width.key=width.key)
    if (!is.null(obji)) {
      mm1 <- mm
      mm <- array(character(length(obji)), dim(obji), dimnames(obji))
      for (i in 1:length(obji))
        mm[i] <- microplot(object=obji[[i]],
                           figPrefix=paste0(figPrefix, sprintf("%3.3i",i)),
                           device=device,
                           height=height, width=width, ## numeric, in inches
                           collapse=collapse,
                           height.x.axis=height.x.axis,
                           par.settings.x.axis=par.settings.x.axis,
                           width.y.axis=width.y.axis,
                           par.settings.y.axis=par.settings.y.axis,
                           key=FALSE,
                           height.key=height.key, width.key=width.key)
      attr(mm, "axis.names") <- attr(mm1, "axis.names")
      attr(mm, "key.name") <- attr(mm1, "key.name")
      class(mm) <- class(mm1)
    }
    ## mm is a matrix of characters containing filenames, with 0, 1, or 2 attributes
    ## with class="microplotMatrix"


    ii <- as.includegraphics(mm, height=height.as, width=width.as,
                             scale=scale, raise=raise, hspace.left=hspace.left, hspace.right=hspace.right, wd,
                             viewport, bb, trim,
                             x.axis=x.axis.includegraphics,
                             y.axis=y.axis.includegraphics,
                             key=key.includegraphics)
    ## class(ii) <- "includegraphicsMatrix"
    ## ii is a matrix of characters containing \\includegraphics{} statements, with 0, 1, or 2 attributes

    latex.includegraphicsMatrix(ii, title=figPrefix, ...) ## method called directly because the class is not currently in use
 }


latex.includegraphicsMatrix <-
  function(ii, ## ii is not currently an includegraphicsMatrix object.
           rowlabel="rowname", ## latex, column heading of row dimnames
           title="figPrefix",  ## latex.default
           rowseparator=FALSE,
           return.value=c("latex","R"),
           bottom=if (!is.null(attr(ii, "key.name")))
                    attr(ii, "key.name"),
           ...) { ## arguments to latex.default
  return.value <- match.arg(return.value)


  ## all four of these must be defined before changing ii below
  ncol.ii <- ncol(ii)
  y.axis <- attr(ii, "axis.names")["y"]
  if (length(y.axis)==0 || is.na(y.axis)) y.axis <- NULL
  x.axis <- attr(ii, "axis.names")["x"]
  if (length(x.axis)==0 || is.na(x.axis)) x.axis <- NULL
  force(bottom)

  if (!is.null(y.axis)) ii <- cbind(" "=y.axis, ii)

  if (!is.null(x.axis))
    ii <- rbind(ii,
                c(if (!is.null(y.axis)) " ", ## "y.axis" is correct
                  rep(x.axis, ncol.ii)))

  if (rowseparator) {
    ii2 <- rbind(ii, ii)
    ii2[seq(1, nrow(ii2), 2),] <- " "
    ii2[seq(2, nrow(ii2), 2),] <- ii
    rownames(ii2) <- as.vector(rbind(" ", rownames(ii)))
    ii <- ii2
  }

  if (return.value == "R") return(ii)

  ll <- latex(ii, title=title, rowlabel=rowlabel, insert.bottom=bottom, ...)
  ll$style <- "graphicx"
  ll
}
