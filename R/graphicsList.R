graphicsList <- function(...) {
  object <- list(...)
  if (length(object) == 1 &&
      ("ggplot" %in% class(object[[1]][[1]]) ||
       "trellis" == class(object[[1]][[1]])))
      object <- object[[1]]

  if ("graphicsList" %in% class(object)) return(object)

  if (!is.list(object))
    stop('graphicsList takes a list of "trellis" objects or a list of "ggplot" objects as its argument.\n',
         'The list may have dim or dimnames, with length(dim(object)) either 1 or 2.',
         call. = FALSE)

  object.class <- sapply(object, class)
  if (all(object.class == "trellis") ||
      all(sapply(object, function(x) "ggplot" %in% class(x)))) {
    object <- as.matrix(object)
    class(object) <- c("graphicsList", class(object))
    if (is.null(dim(object)))
      dim(object) <- c(length(object), 1)
    if (is.null(dimnames(object)))
      dimnames(object) <- list(rep(LETTERS, length=dim(object)[1]),
                               rep(letters, length=dim(object)[2]))
    object
  } else {
    stop('graphicsList requires all items in its argument list to have the same class, either "trellis" or "ggplot".',
         call. = FALSE)
  }
  object
}



microplot.graphicsList <-
  function(object, ## an array of identically structured,
           ## single-panel, graphics objects (trellis or ggplot)
           ## with dim and dimnames
           figPrefix=first.word(deparse(substitute(object))),
           device=c("pdf","postscript","ps","png"),
           res=600, type=getOption("bitmapType"), ## used by png
           height.panel=1, width.panel=1, ## numeric in inches
           key=FALSE,
           height.key=height.panel, width.key=width.panel,
           ## valid arguments for microplot.trellis or microplot.ggplot
           ...) {
  figPrefix <- RemoveDot(figPrefix)
  device <- match.arg(device)
  if (device=="ps") device <- "postscript"

  mm <- array("", dim=dim(object), dimnames=dimnames(object))
  class(mm) <- c("microplotMatrix", class(mm))

  collapse <- list(...)$collapse
  if (is.logical(collapse) && !collapse) {
    for (i in length(object):1) {
      mm[i] <- paste0(figPrefix, sprintf("%3.3i", i), ".", device)
      writePanel(object=object[[i]],
                 filename=mm[i],
                 device=device,
                 height=height.panel,
                 width=width.panel,
                 res=res,
                 type=type,
                 ...)
    }
    is.key <- is.list(key)

    if (is.key) {
      attr(mm, "key.name") <- paste0(figPrefix, sprintf("%3.3i", length(object)+1), ".", device)
      if ("ggplot" %in% class(object[[1]])) {
      writePanel(key,
                 device, attr(mm, "key.name"), height=height.key, width=width.key, res=res, type=type, ...)
      } else { ## "trellis"
      writePanel(plot_grid(draw.key(key)), ## cowplot::plot_grid produces a "ggplot" object
                 device, attr(mm, "key.name"), height=height.key, width=width.key, res=res, type=type, ...)
      }
    }
  } else {
    for (i in length(object):1)
      mm[i] <- mmi <- microplot(object=object[[i]],
                                figPrefix=paste0(figPrefix, sprintf("%3.3i", i)),
                                device=device,
                                height.panel=height.panel,
                                width.panel=width.panel,
                                res=res,
                                type=type,
                                key=key,
                                width.key=width.key,
                                height.key=height.key,
                                ...) ## collapse, if specified, is still in ...
    attr(mm, "axis.names") <- attr(mmi, "axis.names")  ## mmi is now based on object[[1]]
    attr(mm, "lab.names") <- attr(mmi, "lab.names")
    attr(mm, "key.name") <- attr(mmi, "key.name")
    class(mm) <- class(mmi)
  }
  mm
}
