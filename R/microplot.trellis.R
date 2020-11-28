microplot.trellis <-
  function(object=stop("trellis object is required", call. = FALSE), ## object must have class "trellis"
           figPrefix=first.word(deparse(substitute(object))),
           vectorgraph.colname=figPrefix,
           device=c("pdf","postscript","ps","png"),
           res=600, type=getOption("bitmapType"), ## used by png
           height.panel=1, width.panel=1, ## numeric in inches
           collapse=layoutCollapse, ## zero out unwanted layout.heights and layout.widths
           ## for example:
           ## microplot(tt,
           ##   collapse=function(x)
           ##     layoutCollapse(x,
           ##                    layout.heights=list(axis.bottom=.3),
           ##                    layout.widths=list(axis.left=.3),
           ##                    axis.line=list(col="green")))
           height.x.axis=height.panel[1],
           axis.line=list(col="black"),
           xaxis.line=axis.line,
           par.settings.x.axis=
             list(layout.heights=list(panel=0, axis.bottom=1, axis.xlab.padding=0, xlab=0),
                  axis.line=xaxis.line),
           width.y.axis=width.panel[1],
           yaxis.line=axis.line,
           par.settings.y.axis=
             list(layout.widths=list(ylab=0, ylab.axis.padding=0, axis.left=1, panel=0),
                  axis.line=yaxis.line),
           height.xlab=height.panel[1],
           par.settings.xlab=
             list(layout.heights=list(panel=0, axis.bottom=0, axis.xlab.padding=0, xlab=1),
                  axis.line=list(col="transparent")),
           width.ylab=width.panel[1],
           par.settings.ylab=
             list(layout.widths=list(ylab=1, ylab.axis.padding=0, axis.left=0, panel=0),
                  axis.line=list(col="transparent")),
           key=FALSE,    ## FALSE or a list of arguments defining a key
           height.key=height.panel[1], width.key=width.panel[1],
           ...  ## needed to match generic.  ignored in the trellis method
           ) {

    ## change "." to "-" in figPrefix
    ##  The 'latex' macro \code{\\includegraphics} requires that there be no
    ##  \code{"."} in the filename basename.
    figPrefix <- RemoveDot(figPrefix)

    if (any((dim(object) != sapply(dimnames(object), length))))
      stop("microplot.trellis cannot work directly with a subscripted trellis object.\n",
           "Try assigning dimnames to match the panel subscripting.",
           call.=FALSE)

    obji <- collapse(as.vector(object))  ## HH:::as.vector.trellis permits index 1:n
    n <- dim(obji)

    height.panel <- rep(height.panel, length=n)
    width.panel  <- rep(width.panel,  length=n)


    is.key <- is.list(key)

    device <- match.arg(device)
    if (device=="ps") device <- "postscript"

    ## Write each panel, the axis.x and axis.y, and key, to its own file.
    ## the return value is an array with up to two attributes.
    extension <- c(pdf=".pdf",
                   postscript=".ps",
                   png=".png")
    filenames <- paste0(figPrefix, sprintf("%3.3i",1:(n+4+is.key)), extension[device])

    ## panels
    for (i in 1:n)
      writePanel(update(obji[i], legend=NULL),
                 device, filenames[i], height=height.panel[i], width=width.panel[i], res=res)

    ## x axis ## n+1
    if (height.x.axis > 0)
    writePanel(update(obji[1], legend=NULL, xlab="", par.settings=par.settings.x.axis),
               device, filenames[n+1], height=height.x.axis, width=width.panel[1], res=res)

    ## y axis ## n+2
    if (width.y.axis > 0)
    writePanel(update(obji[1], legend=NULL, ylab="", par.settings=par.settings.y.axis),
               device, filenames[n+2], height=height.panel[1], width=width.y.axis, res=res)

    ## xlab ## n+3
    if (height.xlab > 0)
    writePanel(update(obji[1], legend=NULL, xlab=object$xlab, par.settings=par.settings.xlab, scales=list(draw=FALSE)),
               device, filenames[n+3], height=height.xlab, width=width.panel[1], res=res)

    ## ylab ## n+4
    if (width.ylab > 0)
    writePanel(update(obji[1], legend=NULL, ylab=object$ylab, par.settings=par.settings.ylab, scales=list(draw=FALSE)),
               device, filenames[n+4], height=height.panel[1], width=width.ylab, res=res)

    ## key ## n+5
    if (is.key) {
      writePanel(plot_grid(draw.key(key)), ## cowplot::plot_grid produces a "ggplot" object
                 device, filenames[n+5], height=height.key, width=width.key, res=res)
    }

    ## the return value is an array with up to three attributes
    as.table <- object$as.table
    transposed <- (length(object$perm.cond)==2) && all(object$perm.cond  == c(2, 1))

    if (length(dim(object)) == 2) {
      filenames.array <- array(filenames[1:n],
                               dim=dim(object),
                               dimnames=dimnames(object))
    } else { ## vector
      RowNames <- dimnames(object)[[1]]
      if ((length(object$condlevels[[1]]) == 1) &&
          (is.logical(object$strip) && !object$strip)) RowNames <- ""
      filenames.array <-
        if (as.table)
          matrix(filenames[1:n], n, 1,
                 dimnames=list(RowNames, vectorgraph.colname))
        else
          matrix(filenames[n:1], n, 1,
                 dimnames=list(rev(RowNames), vectorgraph.colname))
    }

  if (length(object$perm.cond)==2) {
    if (!as.table && !transposed) filenames.array <- t(filenames.array[, ncol(filenames.array):1, drop=FALSE])
    if ( as.table && !transposed) filenames.array <- t(filenames.array)
    if (!as.table &&  transposed) filenames.array <- filenames.array[nrow(filenames.array):1, , drop=FALSE]
    ## if ( as.table &&  transposed) filenames.array <- filenames.array
  }

  if (length(object$perm.cond) > 2)
    stop("microplot.trellis currently handles trellis objects with one or two dimensions\n",
         "input object has dim(object) == c(",
         paste0(dim(object),
                c(rep(", ", length(dim(object))-1), "")),
                ")",
         call. = FALSE)

    structure(filenames.array,
              axis.names= c(if (height.x.axis != 0) x=filenames[n+1],
                            if (width.y.axis  != 0) y=filenames[n+2]),
              lab.names = c(if (height.xlab   != 0) x=filenames[n+3],
                            if (width.ylab    != 0) y=filenames[n+4]),
              key.name = if (is.key) filenames[n+5],
              class=c("microplotMatrix", class(filenames.array)))

  }


`[.microplotMatrix` <-
function (x, i, j, drop = FALSE)
{
  val <- NextMethod(drop=drop)
  attributes(val)[c("class","axis.names","lab.names","key.name")] <-
    attributes(x)[c("class","axis.names","lab.names","key.name")]
  val
}

microplot <- function (object, ...) {
  UseMethod("microplot")
}

writePanel <- function(object, device, filename, height, width, res=600, ...) {
  switch(device,
         pdf=pdf(                  filename, height=height, width=width                     ), ## inch
         postscript=postscript(    filename, height=height, width=width, horizontal=FALSE   ), ## inch
         png=png(                  filename, height=height, width=width, units="in", res=res)) ## inch
  print(object)
  dev.off()
}
