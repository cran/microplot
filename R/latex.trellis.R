latex.graphicsClass <-
  function(object,
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ##
           ## microplot arguments
           device={
             latexcmd <- options()$latexcmd
             if (is.null(latexcmd))
               latexcmd <- "latex"
             switch(latexcmd,
                    pdflatex="pdf",
                    latex=,
                    "postscript")
           },
           ... ## can include arguments to
           ## latex.graphicsClass,
           ## microplot,
           ## as.includegraphics,
           ## latex.includegraphicsMatrix,
           ## latex.default
           )
{
  ## change "." to "-" in figPrefix
  ##  The 'latex' macro \code{\\includegraphics} requires that there be no
  ##  \code{"."} in the filename basename.
  figPrefix <- RemoveDot(figPrefix)
  dir.verify(figPrefix)

  ## microplot.* for object class: trellis, ggplot, graphicList
  mm <- microplot(object=object,
                  figPrefix=paste0(figPrefix,"/fig"),
                  device=device, ...)
  ## mm is a matrix of characters containing filenames, with 0, 1, 2, or 3 attributes
  ## with class="microplotMatrix"

  ii <- as.includegraphics(mm, ...)

  latex(ii, title=title, microplotMatrix=mm, ...) ## ii is an "includegraphicsMatrix"
}


latex.trellis <-
  function(object=stop("trellis object is required", call. = FALSE),
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## latex.graphicsClass,
           ## microplot,
           ## as.includegraphics,
           ## latex.includegraphicsMatrix,
           ## latex.default
           )
{
  force(title)
  latex.graphicsClass(object, figPrefix=figPrefix, title=title, ...)
}


latex.ggplot <-
  function(object=stop("ggplot object is required", call. = FALSE),
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ## latex.graphicsClass,
           ## microplot,
           ## as.includegraphics,
           ## latex.includegraphicsMatrix,
           ## latex.default
           )
{
  force(title)
  latex.graphicsClass(object, figPrefix=figPrefix, title=title, ...)
}


latex.graphicsList <-
  function(object=stop("graphicsList object is required", call. = FALSE),
           ## matrix or vector of trellis objects or ggplot objects,
           ## with dim and dimnames,
           ## normally each containing one panel.
           ## The axes and key will be taken from object[[1]].
           figPrefix=first.word(deparse(substitute(object))),
           title=figPrefix, ## subject to lazy evaluation
           ... ## can include arguments to
           ##     microplot,
           ##     as.includegraphics,
           ##     latex.includegraphicsMatrix,
           ##     latex.default
           )
{
  force(title)
  latex.graphicsClass(object, figPrefix=figPrefix, title=title, ...)
}


latex.includegraphicsMatrix <-
  function(object,
           dataobject, data.first=TRUE,
           title=first.word(deparse(substitute(object))),
           microplotMatrix=NULL,
           arraystretch=1,     ## The normal interrow space is multiplied by arraystretch,
           ##                     so changing it from its default value of 1 to 1.5 makes
           ##                     the rows 1.5 times farther apart.
           ##                     Uses the latex.default argument 'insert.top'.
           bottom.hline.raise=NULL, ## character string with latex unit, for example "-10ex"
           ##        arraystretch interferes with bottom.hline.raise
           ##        Pick arraystretch first.
           bottom=if (!is.null(attr(object, "key.name"))) ## used as insert.bottom in latex.default
                    attr(object, "key.name"),
           col.just.object=rep("c", ncol(object)),
           col.just.dataobject=rep("r", ncol(dataobject)),
           n.cgroup=NULL, ## generated below if cgroup is specified in ... and n.cgroup is not
           ...) { ## arguments to latex.default
    force(bottom)
    force(title)
    if (!missing(object)) {
      object <- as.matrix(object)
      force(col.just.object)
    }
    if (!missing(dataobject)) {
      dataobject <- as.matrix(dataobject)
      force(col.just.dataobject)
    }

    ii <- object

    if (missing(dataobject)) {
      col.just <- col.just.object
    } else {
    ndiff <- nrow(object) - nrow(dataobject)
    for (i in seq(length=ndiff)) dataobject <- rbind(dataobject,"")

    ncol.object <- ncol(object)
    ncol.dataobject <- ncol(dataobject)

    cgroup <- list(...)$cgroup
    need.spacer <- is.null(cgroup)
    ## n.cgroup ## generated below if cgroup is specified in ... and n.cgroup is not


    if (data.first) {
      if (!is.null(cgroup) & is.null(n.cgroup))
        n.cgroup <- c(ncol.dataobject, ncol.object)
      object <- cbind(dataobject, " "=""[need.spacer], object)
      col.just <- c(col.just.dataobject,
                    "c"[need.spacer], ## spacer
                    col.just.object)
    } else {
      if (!is.null(cgroup) & is.null(n.cgroup))
        n.cgroup <- c(ncol.object, ncol.dataobject)
      object <- cbind(object, " "=""[need.spacer], dataobject)
      col.just <- c(col.just.object,
                    "c"[need.spacer], ## spacer
                    col.just.dataobject)
    }


  }

  insert.top <- if (arraystretch==1)
                  ""
                else
                  paste0("}\\renewcommand{\\arraystretch}{", arraystretch, "}{")

  if (!is.null(bottom.hline.raise))
    object[nrow(object), ncol(object)] <-
      paste0(object[nrow(object), ncol(object)], "\\\\[", bottom.hline.raise, "]")

  ll <- latex(unclass(object), title=title,
              insert.bottom=bottom, insert.top=insert.top,
              col.just=col.just,
              n.cgroup=n.cgroup,
              ...) ## ... includes cgroup
  class(ll) <- c("latexConsole", class(ll))
  ll$style <- "graphicx"

  attr(ll, "microplotMatrix") <- microplotMatrix
  attr(ll, "includegraphicsMatrix") <- ii

  ll
}

latex.microplotMatrix <-
  function(object,
           title=first.word(deparse(substitute(object))),
           ...) {
    force(title)
    latex(as.includegraphics(object, ...), title=title, ...)
}

dir.verify <- function(path)
  if (dir.exists(path)) TRUE else (dir.create(path) == TRUE) ## forces a visible return value
