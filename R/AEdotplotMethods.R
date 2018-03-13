microplot.AEdotplot <- function(object, figPrefix, width.left=2, width.right=1.5,
                                height.panel=.2, height.x.axis=.45, ...) {
  text <- do.call(rbind,
                  lapply(rev(object$text.plot$panel.args),
                         function(txt) {matrix(txt$labels, nrow=1, ncol=5,
                                               dimnames=list(levels(txt$y), txt$x))}))
  text[] <- format(as.numeric(text))

  left <- microplot(object$left.plot, figPrefix=paste0(figPrefix,"/left"),
                    width.panel=width.left, height.panel=height.panel,
                    height.x.axis=height.x.axis,
                    key=attr(object, "ae.key"),
                    ...)
  left <- microplotAttrDisplay(left,
                               y.axis=NULL, ylab=NULL, xlab=NULL,
                               columnKey=1)

  right <- microplot(object$right.plot, figPrefix=paste0(figPrefix,"/right"),
                     width.panel=width.right, height.panel=height.panel,
                     height.x.axis=height.x.axis,
                     ...)
  right <- microplotAttrDisplay(right,
                                y.axis=NULL, ylab=NULL, xlab=NULL,
                                key="", columnKey=1)  ## placeholder key for this case

  list(dataobject=text, object=structure(cbind(left, right),
                                       class=c("microplotMatrix", "matrix")))

}

latex.AEdotplot <- function(object,
                            figPrefix = first.word(deparse(substitute(object))),
                            rowlabel="Most Frequent On-Therapy Adverse Events",
                            device="pdf",
                            ...) {

  ## change "." to "-" in figPrefix
  ##  The 'latex' macro \code{\\includegraphics} requires that there be no
  ##  \code{"."} in the filename basename.
  figPrefix <- RemoveDot(figPrefix)
  dir.verify(figPrefix)

  mm <- microplot(object, figPrefix=figPrefix, ...)

  mmd <- mm$dataobject ## already formatted
  mmd[] <- gsub(" ","\\\\phantom{0}", mmd[])
  mmd[] <- paste0("$", mmd, "$")

  latex(mm$object,
        dataobject=mmd,
        device=device,
        title=figPrefix,
        rowlabel=rowlabel,
        ...)
}

msWord.AEdotplot <- function(object,
                             figPrefix = first.word(deparse(substitute(object))),
                             device="png",
                             height.panel=.25, height.x.axis=.45,
                             width.left=2, width.right=1.5,
                             height.key=height.panel,
                             width.dataobject=.7,
                             rowlabel="Adverse Event", width.rowname=2,
                             ...) {

  ## change "." to "-" in figPrefix
  ##  The 'latex' macro \code{\\includegraphics} requires that there be no
  ##  \code{"."} in the filename basename.
  figPrefix <- RemoveDot(figPrefix)
  dir.verify(figPrefix)

  mm <- microplot(object, device=device,
                  figPrefix=figPrefix,
                  height.panel=height.panel, height.x.axis=height.x.axis,
                  width.left=width.left, width.right=width.right,
                  height.key=height.key,
                  ...)

  msWord(mm$object,
         dataobject=rbind(mm$dataobject,  ## already formatted
                          "", ""),
         title=figPrefix,
         rowlabel=rowlabel, width.rowname=width.rowname,
         height.panel=c(rep(height.panel, length=nrow(mm$dataobject)),
                        height.x.axis,
                        height.key),
         width.panel=c(width.left, width.right),
         width.dataobject=width.dataobject,
         landscape=TRUE,
         ...)

}
