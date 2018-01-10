as.includegraphics <-
  function(object,
           height=NULL, ## LaTeX measurement (character)
           width=NULL, ## retains original aspect ratio, LaTeX measurement (character)
           scale=NULL, ## number
           raise=NULL, ## LaTeX measurement (character)
           hspace.left=NULL,  ## LaTeX measurement (character)
           hspace.right=NULL, ## LaTeX measurement (character)
           wd=getwd(), ## working directory
           viewport=NULL, ## if specified, then left bottom right top (character)
           ## used for pdf png jpeg
           ## See MediaBox in pdf file.
           ## Ask operating system for png or jpg file.
           bb=NULL, ## if specified, then left bottom right top (character)
           ## used for bmp tiff ps, ask operating system for values
           trim=NULL, ## for example, "0 0 0 0" left bottom right top (character)
           x.axis=TRUE, ## may be logical or a list of arguments
           y.axis=TRUE, ## may be logical or a list of arguments
           key=FALSE    ## may be logical or a list of arguments
         ) {
    graphicx.options <-
      if ((is.null(height)) &&
          (is.null(width)) &&
          (is.null(scale)) &&
          (is.null(viewport)) &&
          (is.null(bb)) &&
          (is.null(trim)))
        ""
      else {
        g.o <- c(
          if (!is.null(height)) paste0("height=", height),
          if (!is.null(width)) paste0("width=", width),
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

    dim(result) <- dim(object)
    dimnames(result) <- dimnames(object)

    as.incl.attr <- function(attrib, flag, in.list) {
      if ((is.logical(flag) && flag) || is.list(flag))
        if (!is.null(attrib)) {
          if (is.list(flag)) in.list[names(flag)] <- flag
          do.call(as.includegraphics, c(list(attrib), in.list))
        }
    }

    in.list <- list(height=height, width=width, scale=scale, raise=raise,
                    hspace.left=hspace.left, hspace.right=hspace.right, wd=wd,
                    viewport=viewport, bb=bb, trim=trim)

    if (!is.null(attr(object, "axis.names"))) {
      attr(result, "axis.names") <-
        c(x=as.incl.attr(attr(object, "axis.names")["x"], x.axis, in.list),
          y=as.incl.attr(attr(object, "axis.names")["y"], y.axis, in.list))
    }

    if (!is.null(attr(object, "keyname")))
      attr(result, "keyname") <-
        as.incl.attr(attr(object, "keyname"), key, in.list)

    object.class <- class(object)[-match("microplotMatrix", class(object), 0)]
    ## structure(result, class=c("includegraphicsMatrix", object.class)) ## not currently usisng "includegraphicsMatrix"
    structure(result, class=object.class)
  }
