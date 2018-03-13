as.includegraphics <- function (object, ...) {
  UseMethod("as.includegraphics")
}

as.includegraphics.default <-
  function(object,
           height.includegraphics=NULL, ## LaTeX measurement (character)
           width.includegraphics=NULL, ## retains original aspect ratio, LaTeX measurement (character)
           scale=NULL, ## number
           raise=NULL, ## LaTeX measurement (character)
           tabularinclude=TRUE,
           hspace.left=NULL,  ## LaTeX measurement (character)
           hspace.right=NULL, ## LaTeX measurement (character)
           wd=getwd(), ## working directory.  No embedded spaces in directory name.
           viewport=NULL, ## if specified, then left bottom right top (character)
           ## used for pdf png jpeg
           ## See MediaBox in pdf file.
           ## Ask operating system for png or jpg file.
           bb=NULL, ## if specified, then left bottom right top (character)
           ## used for bmp tiff ps, ask operating system for values
           trim=NULL, ## for example, "0 0 0 0" left bottom right top (character)
           x.axis.includegraphics=TRUE,      ## logical or a list of arguments to latex \includegraphics[here]{}
           y.axis.includegraphics=TRUE,      ## logical or a list of arguments
           xlab.includegraphics=FALSE,       ## logical or a list of arguments
           ylab.includegraphics=FALSE,       ## logical or a list of arguments
           key.includegraphics=!is.null(attr(object, "key.name")), ## logical or a list of arguments
           as.attr=FALSE, ## logical
           label.x.axis="", ## empty, nchar=0
           label.y.axis=" ", ## one space, nchar=1
           columnKey=NULL, ## see ?microplotAttrDisplay
           ...
           ) {

    if (length(grep(" ", wd))) stop("The graphics files are in a directory '", wd,
                                    "'\nwhose name has embedded spaces.\n",
                                    "The \\includegraphics macro in Hmisc::latex can't use that name.\n",
                                    "Please change your working directory with 'setwd()' to a directory\n",
                                    "with no embedded spaces anywhere in its pathname.\n",
                                    "See 'help(as.includegraphics)' for a workaround.", call.=FALSE)

    graphicx.options <-
      if ((is.null(height.includegraphics)) &&
          (is.null(width.includegraphics)) &&
          (is.null(scale)) &&
          (is.null(viewport)) &&
          (is.null(bb)) &&
          (is.null(trim)))
        ""
      else {
        g.o <- c(
          if (!is.null(height.includegraphics)) paste0("height=", height.includegraphics),
          if (!is.null(width.includegraphics)) paste0("width=", width.includegraphics),
          if (!is.null(scale)) paste0("scale=", scale),
          if (!is.null(viewport)) paste0("viewport=", viewport),
          if (!is.null(bb)) paste0("bb=", bb),
          if (!is.null(trim)) paste0("trim=", trim, ", clip=true")
        )
        l.g.o <- length(g.o)
        if (l.g.o > 1)
          g.o <- paste0(as.vector(rbind(g.o, c(rep(",", l.g.o-1), ""))), collapse="")
        paste0("[", g.o, "]")
      }

    result <- paste0(if (!is.null(hspace.left)) paste0("\\hspace{", hspace.left, "}"),
                     "\\includegraphics",
                     graphicx.options,
                     "{", file.path(wd, object[]), "}",
                     if (!is.null(hspace.right)) paste0("\\hspace{", hspace.right, "}"))
    if (!is.null(raise))
      result <- paste0("\\raisebox{", raise, "}{", result, "}")
    if (!is.null(tabularinclude))
      result <- paste0("\\setlength{\\tabcolsep}{0pt}\\begin{tabular}{c}", result, "\\end{tabular}")

    dim(result) <- dim(object)
    dimnames(result) <- dimnames(object)
    names(result) <- names(object)
    result[object==""] <- ""

    as.incl.attr <- function(attrib, flag, in.list) {
      if ((is.logical(flag) && flag) || is.list(flag))
        if (!is.na(attrib) && !is.null(attrib)) {
          if (is.list(flag)) in.list[names(flag)] <- flag
          do.call(as.includegraphics, c(list(attrib), in.list))
        }
    }


    in.list <- list(height=height.includegraphics, width=width.includegraphics, scale=scale, raise=raise,
                    hspace.left=hspace.left, hspace.right=hspace.right, wd=wd,
                    viewport=viewport, bb=bb, trim=trim, tabularinclude=tabularinclude)

    if (!is.null(attr(object, "axis.names"))) {
      attr(result, "axis.names") <-
        c(x=as.incl.attr(unname(attr(object, "axis.names")["x"]), x.axis.includegraphics, in.list),
          y=as.incl.attr(unname(attr(object, "axis.names")["y"]), y.axis.includegraphics, in.list))
    }

    if (!is.null(attr(object, "lab.names"))) {
      attr(result, "lab.names") <-
        c(x=as.incl.attr(unname(attr(object, "lab.names")["x"]), xlab.includegraphics, in.list),
          y=as.incl.attr(unname(attr(object, "lab.names")["y"]), ylab.includegraphics, in.list))
    }

    if (!is.null(attr(object, "key.name")))
      attr(result, "key.name") <-
        as.incl.attr(attr(object, "key.name"), key.includegraphics, in.list)

    object.class <- class(object)
    is.mM <- match("microplotMatrix", class(object), 0)
    if (is.mM != 0) object.class <- object.class[-is.mM]

    result <- structure(result, class=c("includegraphicsMatrix", object.class))
    ## attr(result, "microplotMatrix") <- object

    if (as.attr)
      result ## attributes in microplotMatrix become attributes in includegraphicsMatrix
    else
      microplotAttrDisplay(result, ## axis.names and lab.names and key.name (as specified)
                           ##         become columns in result
                           label.x.axis=label.x.axis,
                           label.y.axis=label.y.axis,
                           columnKey=columnKey)

    ## do.call(microplotAttrDisplay, ## is.list(as.attr)
    ##         c(list(result), as.attr)) ## loses key; y.axis gets replaced with FALSE
    ## ## what I want: other attributes to matrix, key stays as attribute
  }


`[.includegraphicsMatrix` <-
function (x, i, j, drop = FALSE)
{
  ## mM <- attr(x, "microplotMatrix")
  val <- NextMethod(drop=drop)
  attributes(val)[c("class","axis.names","lab.names","key.name")] <-
    attributes(x)[c("class","axis.names","lab.names","key.name")]
  ## if (!is.null(mM) && all(dim(x) == dim(mM))) {
  ##   if (missing(i)) i <- seq(length=nrow(mM))
  ##   if (missing(j)) j <- seq(length=ncol(mM))
  ##   attr(val, "microplotMatrix") <- mM[i, j, drop=drop]
  ## }
  val
}

as.includegraphics.trellis <- function (object, ...) {
  stop("as.includegraphics is not defined for 'trellis' objects.", call.=FALSE)
}

as.includegraphics.ggplot <- function (object, ...) {
  stop("as.includegraphics is not defined for 'ggplot' objects.", call.=FALSE)
}

as.includegraphics.graphicsList <- function (object, ...) {
  stop("as.includegraphics is not defined for 'graphicsList' objects.", call.=FALSE)
}

as.includegraphics.microplotMatrix <- function (object, ...) {
  NextMethod("as.includegraphics")
}

as.includegraphics.includegraphicsMatrix <- function (object, ...) {
  object
}
