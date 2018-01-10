microplot.trellis <-
  function(object=stop("trellis object is required", call. = FALSE), ## object must have class "trellis"
           figPrefix=Hmisc::first.word(deparse(substitute(object))),
           device=c("pdf","postscript","png", ## other devices require a user-written function
                    if (version$os == "mingw32") "win.metafile"),
           height=1, width=1, ## numeric in inches
           collapse=layoutCollapse, ## see below for example ## zero out unwanted layout.heights and layout.widths
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
           ...  ## needed to match generic.  ignored in the trellis method
           ) {

    if (any((dim(object) != sapply(dimnames(object), length))))
      stop("microplot.trellis cannot work directly with a subscripted trellis object.\n",
           "Try assigning dimnames to match the panel subscripting.",
           call.=FALSE)

    obji <- collapse(as.vector(object))  ## HH:::as.vector.trellis permits index 1:n
    n <- dim(obji)

    is.key <- is.list(key)

    device <- match.arg(device)

    ## Write each panel, the axis.x and axis.y, and key, to its own file.
    ## the return value is an array with up to two attributes.
    extension <- c(pdf=".pdf",
                   postscript=".ps",
                   png=".png",
                   win.metafile=".wmf")
    filenames <- paste0(figPrefix, sprintf("%3.3i",1:(n+2+is.key)), extension[device])

    for (i in 1:n) {
      switch(device,
             pdf=pdf(                  filenames[i], height=height, width=width                     ), ## inch
             postscript=postscript(    filenames[i], height=height, width=width, horizontal=FALSE   ), ## inch
             png=png(                  filenames[i], height=height, width=width, units="in", res=300), ## inch
             win.metafile=win.metafile(filenames[i], height=height, width=width                     )) ## inch
      print(update(obji[i], legend=NULL))   ## 1:n
      dev.off()
    }


    ## x axis
    switch(device,
           pdf=pdf(                  filenames[n+1], height=height.x.axis, width=width                     ), ## inch
           postscript=postscript(    filenames[n+1], height=height.x.axis, width=width, horizontal=FALSE   ), ## inch
           png=png(                  filenames[n+1], height=height.x.axis, width=width, units="in", res=300), ## inch
           win.metafile=win.metafile(filenames[n+1], height=height.x.axis, width=width                     )) ## inch

    print(update(obji[1], ## n+1
                 legend=NULL, xlab=object$xlab,
                 par.settings=par.settings.x.axis))
    dev.off()


    ## y axis
    switch(device,
           pdf=pdf(                  filenames[n+2], height=height, width=width.y.axis                     ), ## inch
           postscript=postscript(    filenames[n+2], height=height, width=width.y.axis, horizontal=FALSE   ), ## inch
           png=png(                  filenames[n+2], height=height, width=width.y.axis, units="in", res=300), ## inch
           win.metafile=win.metafile(filenames[n+2], height=height, width=width.y.axis                     )) ## inch

    print(update(obji[1], ## n+2
                 legend=NULL, ylab=object$ylab,
                 par.settings=par.settings.y.axis))
    dev.off()


    ## key
    if (is.key) {
      switch(device,
             pdf=pdf(                  filenames[n+2+is.key], height=height.key, width=width.key                     ), ## inch
             postscript=postscript(    filenames[n+2+is.key], height=height.key, width=width.key, horizontal=FALSE   ), ## inch
             png=png(                  filenames[n+2+is.key], height=height.key, width=width.key, units="in", res=300), ## inch
             win.metafile=win.metafile(filenames[n+2+is.key], height=height.key, width=width.key                     )) ## inch
      draw.key(key, draw=TRUE) ## n+2+is.key
      dev.off()
    }

    ## the return value is an array with up to two attributes
    filenames.array <- array(filenames[1:n],
                             dim=dim(object),
                             dimnames=dimnames(object))

    as.table <- object$as.table
    transposed <- (length(object$perm.cond)==2) && all(object$perm.cond  == c(2, 1))

  if (length(object$perm.cond)==1) {
    if (!as.table) filenames.array <- rev(filenames.array)
    filenames.array <- as.matrix(filenames.array)
    }

  if (length(object$perm.cond)==2) {
    if (!as.table && !transposed) filenames.array <- t(filenames.array[, ncol(filenames.array):1, drop=FALSE])
    if ( as.table && !transposed) filenames.array <- t(filenames.array)
    if (!as.table &&  transposed) filenames.array <- filenames.array[nrow(filenames.array):1, , drop=FALSE]
    ## if ( as.table &&  transposed) filenames.array <- filenames.array
  }

  if (length(object$perm.cond) > 2)
    stop("latex.trellis currently handles trellis objects with one or two dimensions\n",
         "input object has dim(object) == c(",
         paste0(dim(object),
                c(rep(", ", length(dim(object))-1), "")),
                ")",
         call. = FALSE)


    structure(filenames.array,
              axis.names=c(x=filenames[n+1], y=filenames[n+2]),
              key.name = if (is.key) filenames[n+2+is.key],
              class=c("microplotMatrix", class(filenames.array)))
              ## "microplotMatrix" class not currently used.
              ## as.includegraphics might eventually need it.
  }
